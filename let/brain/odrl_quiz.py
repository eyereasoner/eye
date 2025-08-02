#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Detect conflicts in the updated ODRL quiz files and print proof-style traces.

- Downloads and parses all quiz .ttl files listed below.
- Loads the ODRL 2.2 vocabulary to get odrl:includedIn (action hierarchy).
- Action overlap: equality, odrl:includedIn*, rdfs:subClassOf* (for custom actions).
- Constraint overlap: supports odrl:purpose with odrl:eq / odrl:neq / odrl:isA.
- Handles both odrl:prohibition and (typo/alias) odrl:prohibited on policies.

Output mirrors the lightweight "premises → overlap reason → conclusion" style.

Requires:
    pip install rdflib

References:
  ODRL ontology landing page (with ODRL22.ttl): https://www.w3.org/ns/odrl/2/
"""

import sys
from typing import Dict, Iterable, List, Optional, Set, Tuple

from rdflib import Graph, Namespace, URIRef, BNode, RDF, RDFS
from rdflib.namespace import DCTERMS

# Namespaces
ODRL = Namespace("http://www.w3.org/ns/odrl/2/")
EX   = Namespace("http://example.com/ns#")
FOAF = Namespace("http://xmlns.com/foaf/0.1/")

# Remote vocab (used at runtime to fetch odrl:includedIn hierarchy)
ODRL_TTL_URL = "https://www.w3.org/ns/odrl/2/ODRL22.ttl"

# Updated quiz set
QUIZ_URLS = [
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-01.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-02.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-03.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-04.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-05.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-06.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-07.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-08.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-09.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-10.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-01.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-02.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-03.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-04.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-05.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-06.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-07.ttl",
  "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-08.ttl",
]

# Matching policy
REQUIRE_SAME_ASSIGNEE = True
REQUIRE_SAME_TARGET   = True

# ---------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------

def qname(g: Graph, term) -> str:
    if isinstance(term, URIRef):
        try:
            return g.namespace_manager.normalizeUri(term)
        except Exception:
            return str(term)
    if isinstance(term, BNode):
        return f"_:{term}"
    return str(term)

def rt_closure(edges: Dict[URIRef, Set[URIRef]]) -> Dict[URIRef, Set[URIRef]]:
    """Reflexive–transitive closure: for each node a, closure[a] includes a and all reachable supers."""
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
        seen.add(a)
        closure[a] = seen
    return closure

# ---------------------------------------------------------------------
# Action hierarchy (includedIn/subClassOf)
# ---------------------------------------------------------------------

class ActionHierarchy:
    def __init__(self, g_vocab: Graph, g_quiz: Graph):
        incl_edges: Dict[URIRef, Set[URIRef]] = {}

        def add(s, o):
            if isinstance(s, URIRef) and isinstance(o, URIRef):
                incl_edges.setdefault(s, set()).add(o)

        for s, _, o in g_vocab.triples((None, ODRL.includedIn, None)):
            add(s, o)
        for s, _, o in g_quiz.triples((None, ODRL.includedIn, None)):
            add(s, o)
        for s, _, o in g_quiz.triples((None, RDFS.subClassOf, None)):
            add(s, o)

        self.closure = rt_closure(incl_edges)

    def overlap(self, a: URIRef, b: URIRef) -> Tuple[bool, str]:
        if a == b:
            return True, f"same action ({a})"
        a_sup = self.closure.get(a, {a})
        b_sup = self.closure.get(b, {b})
        if b in a_sup:
            return True, f"{qname(Graph(), a)} ⊑ {qname(Graph(), b)} via includedIn/subClassOf*"
        if a in b_sup:
            return True, f"{qname(Graph(), b)} ⊑ {qname(Graph(), a)} via includedIn/subClassOf*"
        return False, "no action inclusion relation"

# ---------------------------------------------------------------------
# Type hierarchy (for constraint isA checks)
# ---------------------------------------------------------------------

class TypeHierarchy:
    """rdfs:subClassOf* closure for class membership checks."""
    def __init__(self, g_vocab: Graph, g_quiz: Graph):
        edges: Dict[URIRef, Set[URIRef]] = {}
        def add(s, o):
            if isinstance(s, URIRef) and isinstance(o, URIRef):
                edges.setdefault(s, set()).add(o)
        for s, _, o in g_vocab.triples((None, RDFS.subClassOf, None)):
            add(s, o)
        for s, _, o in g_quiz.triples((None, RDFS.subClassOf, None)):
            add(s, o)
        self.closure = rt_closure(edges)

    def is_instance_of(self, g: Graph, x: URIRef, C: URIRef) -> bool:
        # x rdf:type T and T ⊑* C  (or T == C)
        for T in g.objects(x, RDF.type):
            if isinstance(T, URIRef):
                supers = self.closure.get(T, {T})
                if C in supers:
                    return True
        return False

# ---------------------------------------------------------------------
# Model
# ---------------------------------------------------------------------

class Constraint:
    def __init__(self, left: URIRef, op: URIRef, right):
        self.left = left
        self.op = op
        self.right = right  # URIRef or literal

class Rule:
    def __init__(self, kind: str, policy, node, assignee, action, target, constraints: List[Constraint]):
        self.kind = kind  # 'permission' | 'prohibition' | 'obligation'
        self.policy = policy
        self.node = node
        self.assignee = assignee
        self.action = action
        self.target = target
        self.constraints = constraints

# ---------------------------------------------------------------------
# Extract rules + constraints
# ---------------------------------------------------------------------

def _constraints(g: Graph, node) -> List[Constraint]:
    res: List[Constraint] = []
    for c in g.objects(node, ODRL.constraint):
        left  = next(g.objects(c, ODRL.leftOperand), None)
        op    = next(g.objects(c, ODRL.operator), None)
        right = next(g.objects(c, ODRL.rightOperand), None)
        if isinstance(left, URIRef) and isinstance(op, URIRef) and right is not None:
            res.append(Constraint(left, op, right))
    return res

def extract_rules(g: Graph) -> List[Rule]:
    rules: List[Rule] = []
    # Accept odrl:prohibition and non-standard odrl:prohibited (seen in new files)
    KIND_PREDICATES = [
        (ODRL.permission,  'permission'),
        (ODRL.prohibition, 'prohibition'),
        (ODRL.prohibited,  'prohibition'),  # alias/typo tolerance
        (ODRL.obligation,  'obligation'),
    ]
    for pol in g.subjects(RDF.type, ODRL.Set):
        for pred, kind in KIND_PREDICATES:
            for node in g.objects(pol, pred):
                assignees = list(g.objects(node, ODRL.assignee)) or [None]
                actions   = [a for a in g.objects(node, ODRL.action) if isinstance(a, URIRef)]
                targets   = list(g.objects(node, ODRL.target)) or [None]
                cons      = _constraints(g, node)
                for a in assignees:
                    for act in actions:
                        for t in targets:
                            rules.append(Rule(kind, pol, node, a, act, t, cons))
    return rules

# ---------------------------------------------------------------------
# Constraint overlap (purpose / eq|neq|isA)
# ---------------------------------------------------------------------

def _purpose_pairs(r: Rule) -> List[Tuple[URIRef, URIRef]]:
    """Return list of (op, right) for odrl:purpose constraints."""
    return [(c.op, c.right) for c in r.constraints if c.left == ODRL.purpose]

def _eq(a, b) -> bool:
    return a == b

def purpose_overlap(g: Graph, TH: TypeHierarchy,
                    r1: Rule, r2: Rule) -> Tuple[bool, str]:
    C1 = _purpose_pairs(r1)
    C2 = _purpose_pairs(r2)
    if not C1 and not C2:
        return True, "no purpose constraints"
    if not C1:
        return True, "left unconstrained (purpose)"
    if not C2:
        return True, "right unconstrained (purpose)"

    # Check if any pair is simultaneously satisfiable
    for op1, v1 in C1:
        for op2, v2 in C2:
            why = None
            ok = False

            if op1 == ODRL.eq and op2 == ODRL.eq:
                ok = _eq(v1, v2)
                if ok: why = f"eq({qname(g,v1)}) ∧ eq({qname(g,v2)}) ⇒ {qname(g,v1)}={qname(g,v2)}"

            elif op1 == ODRL.eq and op2 == ODRL.neq:
                ok = (v1 != v2)
                if ok: why = f"eq({qname(g,v1)}) ∧ neq({qname(g,v2)}) ⇒ {qname(g,v1)}≠{qname(g,v2)}"

            elif op1 == ODRL.neq and op2 == ODRL.eq:
                ok = (v1 != v2)
                if ok: why = f"neq({qname(g,v1)}) ∧ eq({qname(g,v2)}) ⇒ {qname(g,v1)}≠{qname(g,v2)}"

            elif op1 == ODRL.eq and op2 == ODRL.isA and isinstance(v1, URIRef) and isinstance(v2, URIRef):
                ok = TH.is_instance_of(g, v1, v2)
                if ok: why = f"eq({qname(g,v1)}) ∧ isA({qname(g,v2)}) via rdf:type/subClassOf*"

            elif op1 == ODRL.isA and op2 == ODRL.eq and isinstance(v1, URIRef) and isinstance(v2, URIRef):
                ok = TH.is_instance_of(g, v2, v1)
                if ok: why = f"isA({qname(g,v1)}) ∧ eq({qname(g,v2)}) via rdf:type/subClassOf*"

            # (Optional) more combinations can be added as needed.
            if ok:
                return True, f"purpose overlap: {why}"
    return False, "purpose constraints incompatible"

# ---------------------------------------------------------------------
# Conflict detection
# ---------------------------------------------------------------------

def same_scope(a: Rule, b: Rule) -> bool:
    if REQUIRE_SAME_ASSIGNEE and a.assignee != b.assignee:
        return False
    if REQUIRE_SAME_TARGET and a.target != b.target:
        return False
    return True

class Conflict:
    def __init__(self, kind: str, a: Rule, b: Rule, why_action: str, why_purpose: str):
        self.kind = kind  # 'perm-vs-prohib' | 'duty-blocked'
        self.a = a
        self.b = b
        self.why_action = why_action
        self.why_purpose = why_purpose

def detect_conflicts(g_vocab: Graph, gq: Graph) -> Tuple[List[Rule], List[Conflict]]:
    rules = extract_rules(gq)
    AH = ActionHierarchy(g_vocab, gq)
    TH = TypeHierarchy(g_vocab, gq)

    conflicts: List[Conflict] = []

    perms  = [r for r in rules if r.kind == 'permission']
    prohib = [r for r in rules if r.kind == 'prohibition']
    duties = [r for r in rules if r.kind == 'obligation']

    # Permission vs Prohibition
    for p in perms:
        for n in prohib:
            if not same_scope(p, n):
                continue
            okA, whyA = AH.overlap(p.action, n.action)
            if not okA:
                continue
            okP, whyP = purpose_overlap(gq, TH, p, n)
            if not okP:
                continue
            conflicts.append(Conflict('perm-vs-prohib', p, n, whyA, whyP))

    # Duty blocked by Prohibition (obligation action within prohibition action)
    for d in duties:
        for n in prohib:
            if not same_scope(d, n):
                continue
            okA, whyA = AH.overlap(d.action, n.action)
            if not okA:
                continue
            okP, whyP = purpose_overlap(gq, TH, d, n)
            if not okP:
                continue
            conflicts.append(Conflict('duty-blocked', d, n, whyA, whyP))

    return rules, conflicts

# ---------------------------------------------------------------------
# Proof-style printing
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
    for c in r.constraints:
        lines.append(f"  {bn} odrl:constraint [ odrl:leftOperand {qname(g,c.left)} ; odrl:operator {qname(g,c.op)} ; odrl:rightOperand {qname(g,c.right)} ] .")
    return lines

def print_report(name: str, g_vocab: Graph, quiz_url: str) -> None:
    gq = Graph()
    gq.namespace_manager.bind("odrl", ODRL, override=True)
    gq.namespace_manager.bind("dct", DCTERMS, override=False)
    gq.namespace_manager.bind("rdfs", RDFS, override=False)
    gq.parse(quiz_url, format="turtle")

    rules, conflicts = detect_conflicts(g_vocab, gq)

    print(f"\n=== {name} ===")
    if not conflicts:
        print("No conflicts detected.")
        return

    for i, c in enumerate(conflicts, start=1):
        print(f"[{i:02d}] CONFLICT: {'Permission vs Prohibition' if c.kind=='perm-vs-prohib' else 'Obligation blocked by Prohibition'}")
        for r in (c.a, c.b):
            for line in rule_fact_lines(gq, r):
                print("  " + line)
        print(f"  Action overlap: {c.why_action}")
        print(f"  Constraint overlap: {c.why_purpose}")
        ass = qname(gq, c.a.assignee) if c.a.assignee else "?"
        tgt = qname(gq, c.a.target) if c.a.target else "?"
        print(f"  Same assignee/target: {ass} / {tgt}")
        print("  Therefore: conflict.\n")

# ---------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------

def main() -> int:
    gv = Graph()
    try:
        gv.parse(ODRL_TTL_URL, format="turtle")
    except Exception as e:
        sys.stderr.write(f"[warn] Could not fetch ODRL22.ttl ({e}). Proceeding without it (reduced action reasoning).\n")
        gv = Graph()
    gv.namespace_manager.bind("odrl", ODRL, override=True)
    gv.namespace_manager.bind("rdfs", RDFS, override=False)

    for url in QUIZ_URLS:
        name = url.rsplit("/", 1)[-1]
        try:
            print_report(name, gv, url)
        except Exception as e:
            print(f"\n=== {name} ===")
            print(f"Error parsing or evaluating this quiz: {e}")
    return 0

if __name__ == "__main__":
    sys.exit(main())

