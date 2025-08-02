#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Solve the 9 ODRL quiz cases by detecting (and proving) conflicts.

- Parses each quiz .ttl file.
- Loads ODRL22.ttl to get odrl:includedIn relations.
- Supports action overlap via:
    * exact equality
    * odrl:includedIn (reflexive-transitive)
    * rdfs:subClassOf (reflexive-transitive)  <-- used by quiz-6 custom action
- Reports "permission vs prohibition" conflicts
- Reports "duty-blocked" conflicts (obligation ⊑ prohibition-action)

Printing style mirrors the lightweight proof steps used around EYE examples:
  premises (quoted from the graphs) -> action-overlap reason -> conflict conclusion

Usage:
    python solve_odrl_quiz.py

Requires:
    pip install rdflib

References:
  - ODRL 2.2 ontology/vocabulary: https://www.w3.org/ns/odrl/2/ODRL22.ttl
  - Quiz data:
      https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-1.ttl
      ...
      https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-9.ttl
"""

import sys
from typing import Dict, Iterable, List, Optional, Set, Tuple

from rdflib import Graph, Namespace, URIRef, BNode, RDF, RDFS
from rdflib.namespace import DCTERMS

# ---------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------

ODRL = Namespace("http://www.w3.org/ns/odrl/2/")
EX   = Namespace("http://example.com/ns#")  # safe default; actual quiz uses its own ex:
FOAF = Namespace("http://xmlns.com/foaf/0.1/")

# Where to fetch the ODRL vocabulary (for odrl:includedIn, actions, etc.)
ODRL_TTL_URL = "https://www.w3.org/ns/odrl/2/ODRL22.ttl"

QUIZ_URLS = [
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-1.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-2.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-3.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-4.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-5.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-6.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-7.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-8.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-9.ttl",
]

# Matching policy: how strict should assignee/target matching be?
SCOPE_MATCHING = dict(
    require_same_assignee=True,
    require_same_target=True,
)

# ---------------------------------------------------------------------
# Utilities
# ---------------------------------------------------------------------

def qname(g: Graph, term) -> str:
    """Pretty print a URI/BNode using graph namespace bindings."""
    if isinstance(term, (URIRef,)):
        try:
            return g.namespace_manager.normalizeUri(term)
        except Exception:
            return str(term)
    if isinstance(term, BNode):
        return f"_:{term}"
    return str(term)

def rt_closure(edges: Dict[URIRef, Set[URIRef]]) -> Dict[URIRef, Set[URIRef]]:
    """
    Reflexive-transitive closure for a directed relation.
    For each node a, result[a] includes a and all reachable supers.
    """
    closure: Dict[URIRef, Set[URIRef]] = {}
    nodes = set(edges.keys()) | {y for ys in edges.values() for y in ys}
    for a in nodes:
        seen = set()
        stack = [a]
        while stack:
            x = stack.pop()
            if x in seen:
                continue
            seen.add(x)
            for y in edges.get(x, ()):
                if y not in seen:
                    stack.append(y)
        # reflexive
        seen.add(a)
        closure[a] = seen
    return closure

# ---------------------------------------------------------------------
# Action overlap reasoning
# ---------------------------------------------------------------------

class ActionHierarchy:
    """
    Encapsulates action overlap via:
      - equality
      - odrl:includedIn*
      - rdfs:subClassOf*
    """

    def __init__(self, g_vocab: Graph, g_quiz: Graph):
        incl_edges: Dict[URIRef, Set[URIRef]] = {}

        def add_edge(s, o):
            if isinstance(s, URIRef) and isinstance(o, URIRef):
                incl_edges.setdefault(s, set()).add(o)

        # From the ODRL vocabulary
        for s, _, o in g_vocab.triples((None, ODRL.includedIn, None)):
            add_edge(s, o)

        # From the quiz graph(s) (e.g., quiz-6 uses rdfs:subClassOf on actions)
        for s, _, o in g_quiz.triples((None, ODRL.includedIn, None)):
            add_edge(s, o)
        for s, _, o in g_quiz.triples((None, RDFS.subClassOf, None)):
            add_edge(s, o)

        self.closure = rt_closure(incl_edges)

    def overlap(self, a: URIRef, b: URIRef) -> Tuple[bool, str]:
        """Return whether actions overlap and a human-readable reason."""
        if a == b:
            return True, f"same action ({a})"
        a_supers = self.closure.get(a, {a})
        b_supers = self.closure.get(b, {b})
        # If either is included (possibly transitively) in the other, they overlap
        if b in a_supers:
            return True, f"{a} ⊑ {b} via includedIn/subClassOf*"
        if a in b_supers:
            return True, f"{b} ⊑ {a} via includedIn/subClassOf*"
        return False, "no inclusion relation"

# ---------------------------------------------------------------------
# Extract ODRL statements
# ---------------------------------------------------------------------

class Rule:
    def __init__(self, kind: str, policy, node, assignee, action, target):
        self.kind = kind              # 'permission' | 'prohibition' | 'obligation'
        self.policy = policy          # policy IRI
        self.node = node              # blank node of the rule
        self.assignee = assignee      # URIRef or None
        self.action = action          # URIRef
        self.target = target          # URIRef or None

    def __repr__(self):
        return f"Rule({self.kind}, {self.policy}, {self.assignee}, {self.action}, {self.target})"

def extract_rules(g: Graph) -> List[Rule]:
    rules: List[Rule] = []
    KINDS = [(ODRL.permission, 'permission'),
             (ODRL.prohibition, 'prohibition'),
             (ODRL.obligation, 'obligation')]

    for pol in g.subjects(RDF.type, ODRL.Set):
        for pred, kind in KINDS:
            for node in g.objects(pol, pred):
                assignees = list(g.objects(node, ODRL.assignee)) or [None]
                actions   = list(g.objects(node, ODRL.action))
                targets   = list(g.objects(node, ODRL.target)) or [None]
                for a in assignees:
                    for act in actions:
                        for t in targets:
                            if isinstance(act, (URIRef,)):
                                rules.append(Rule(kind, pol, node, a, act, t))
    return rules

# ---------------------------------------------------------------------
# Conflict detection
# ---------------------------------------------------------------------

class Conflict:
    def __init__(self, kind: str, a: Rule, b: Rule, why_overlap: str):
        self.kind = kind          # 'perm-vs-prohib' | 'duty-blocked'
        self.a = a
        self.b = b
        self.why_overlap = why_overlap

def same_party_target(a: Rule, b: Rule) -> bool:
    if SCOPE_MATCHING["require_same_assignee"]:
        if a.assignee is None or b.assignee is None or a.assignee != b.assignee:
            return False
    if SCOPE_MATCHING["require_same_target"]:
        if a.target is None or b.target is None or a.target != b.target:
            return False
    return True

def detect_conflicts(g_vocab: Graph, g_quiz: Graph) -> Tuple[List[Rule], List[Conflict]]:
    rules = extract_rules(g_quiz)
    ah = ActionHierarchy(g_vocab, g_quiz)
    conflicts: List[Conflict] = []

    # permission vs prohibition
    perms  = [r for r in rules if r.kind == 'permission']
    prohib = [r for r in rules if r.kind == 'prohibition']
    for p in perms:
        for n in prohib:
            if not same_party_target(p, n):
                continue
            ok, reason = ah.overlap(p.action, n.action)
            if ok:
                conflicts.append(Conflict('perm-vs-prohib', p, n, reason))

    # duty blocked by prohibition: obligation action ⊑ prohibition action (or equal)
    duties = [r for r in rules if r.kind == 'obligation']
    for d in duties:
        for n in prohib:
            if not same_party_target(d, n):
                continue
            ok, reason = ah.overlap(d.action, n.action)
            if ok:
                conflicts.append(Conflict('duty-blocked', d, n, reason))

    return rules, conflicts

# ---------------------------------------------------------------------
# Pretty proof printing
# ---------------------------------------------------------------------

def rule_fact_lines(g: Graph, r: Rule) -> List[str]:
    lines = []
    pol = qname(g, r.policy)
    bn  = qname(g, r.node)
    ass = qname(g, r.assignee) if r.assignee else "?"
    act = qname(g, r.action)
    tgt = qname(g, r.target) if r.target else "?"
    lines.append(f"{pol} {r.kind}: {bn}")
    lines.append(f"  {bn} odrl:assignee {ass} ;")
    lines.append(f"  {bn} odrl:action  {act} ;")
    lines.append(f"  {bn} odrl:target  {tgt} .")
    return lines

def print_report(name: str, g_vocab: Graph, quiz_url: str) -> None:
    gq = Graph()
    # register common prefixes for nice qnames
    gq.namespace_manager.bind("odrl", ODRL, override=True)
    gq.namespace_manager.bind("dct", DCTERMS, override=False)
    gq.namespace_manager.bind("rdfs", RDFS, override=False)
    gq.parse(quiz_url, format="turtle")

    rules, conflicts = detect_conflicts(g_vocab, gq)

    print(f"\n=== {name} ===")
    if not conflicts:
        print("No conflicts detected.")
        return

    # Group conflicts by (assignee, target) for readability
    for i, c in enumerate(conflicts, start=1):
        print(f"[{i:02d}] CONFLICT: {'Permission vs Prohibition' if c.kind=='perm-vs-prohib' else 'Obligation blocked by Prohibition'}")
        # premises
        for r in (c.a, c.b):
            for line in rule_fact_lines(gq, r):
                print("  " + line)
        # action overlap reason
        print(f"  Overlap: {c.why_overlap}")
        # scope equality
        ass = qname(gq, c.a.assignee) if c.a.assignee else "?"
        tgt = qname(gq, c.a.target) if c.a.target else "?"
        print(f"  Same assignee/target: {ass} / {tgt}")
        # conclusion
        print("  Therefore: conflict.\n")

# ---------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------

def main() -> int:
    # Load ODRL vocabulary graph (to get odrl:includedIn hierarchy).
    gv = Graph()
    try:
        gv.parse(ODRL_TTL_URL, format="turtle")
    except Exception as e:
        # Fallback: still run without vocabulary (only exact-action matches / in-quiz subclass links)
        sys.stderr.write(f"[warn] Could not fetch ODRL22.ttl ({e}). "
                         "Proceeding without vocab; action overlap will be limited.\n")
        gv = Graph()
    gv.namespace_manager.bind("odrl", ODRL, override=True)
    gv.namespace_manager.bind("rdfs", RDFS, override=False)

    for idx, url in enumerate(QUIZ_URLS, start=1):
        print_report(f"quiz-{idx}.ttl", gv, url)
    return 0

if __name__ == "__main__":
    sys.exit(main())

