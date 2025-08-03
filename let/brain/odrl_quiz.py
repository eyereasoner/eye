#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Conflict checker for the updated ODRL quizzes (00-01..00-10, 01-01..01-08).

- Downloads each quiz .ttl
- Loads ODRL22.ttl to get odrl:includedIn (action hierarchy)
- Detects:
    * permission vs prohibition conflicts
    * obligation blocked by prohibition
- Evaluates constraint compatibility using ALL 12 ODRL operators:
    eq, neq, lt, lteq, gt, gteq, isA, isAllOf, isAnyOf, isNoneOf, hasPart, isPartOf

Proof output: premises → action overlap → constraint overlap → conclusion.

Requires:
    pip install rdflib

Refs:
  - ODRL operators list (12 standard operators). https://www.w3.org/ns/odrl/2/ODRL20.html
  - ODRL Vocab (operators + set-based semantics). https://www.w3.org/TR/odrl-vocab/
"""
import sys
from typing import Dict, Iterable, List, Optional, Set, Tuple, Union

from rdflib import Graph, Namespace, URIRef, BNode, RDF, RDFS, Literal
from rdflib.namespace import DCTERMS, XSD
from rdflib.collection import Collection

# Namespaces
ODRL = Namespace("http://www.w3.org/ns/odrl/2/")
EX   = Namespace("http://example.com/ns#")
FOAF = Namespace("http://xmlns.com/foaf/0.1/")

# Where to fetch ODRL vocabulary (operators, includedIn hierarchy)
ODRL_TTL_URL = "https://www.w3.org/ns/odrl/2/ODRL22.ttl"  # content-negotiated (TTL)

# Updated quiz set (18 files)
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

# Matching policy (tweak if needed)
REQUIRE_SAME_ASSIGNEE = True
REQUIRE_SAME_TARGET   = True

# ------------- helpers ------------------------------------------------

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
    """Reflexive–transitive closure of a directed relation."""
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

def to_python_literal(lit: Literal):
    """Convert rdflib Literal to Python scalar for comparisons."""
    try:
        v = lit.toPython()
        return v
    except Exception:
        return lit

def as_list(g: Graph, node) -> Optional[List]:
    """Return RDF Collection as Python list, or None if node is not a list head."""
    try:
        return list(Collection(g, node))
    except Exception:
        return None

# ------------- action/type hierarchies --------------------------------

class ActionHierarchy:
    """Overlap via equality, odrl:includedIn*, rdfs:subClassOf*."""
    def __init__(self, g_vocab: Graph, g_quiz: Graph):
        edges: Dict[URIRef, Set[URIRef]] = {}
        def add(s, o):
            if isinstance(s, URIRef) and isinstance(o, URIRef):
                edges.setdefault(s, set()).add(o)
        for s, _, o in g_vocab.triples((None, ODRL.includedIn, None)):
            add(s, o)
        for s, _, o in g_quiz.triples((None, ODRL.includedIn, None)):
            add(s, o)
        for s, _, o in g_quiz.triples((None, RDFS.subClassOf, None)):
            add(s, o)
        self.closure = rt_closure(edges)

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

class TypeHierarchy:
    """rdfs:subClassOf* for isA checks."""
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
        for T in g.objects(x, RDF.type):
            if isinstance(T, URIRef):
                supers = self.closure.get(T, {T})
                if C in supers:
                    return True
        return False

# ------------- model ---------------------------------------------------

class Constraint:
    def __init__(self, node, left: URIRef, op: URIRef, right):
        self.node = node
        self.left = left
        self.op = op
        self.right = right  # URIRef | Literal | BNode(list)

class Rule:
    def __init__(self, kind: str, policy, node, assignee, action, target, constraints: List[Constraint]):
        self.kind = kind  # 'permission'|'prohibition'|'obligation'
        self.policy = policy
        self.node = node
        self.assignee = assignee
        self.action = action
        self.target = target
        self.constraints = constraints

# ------------- extraction ---------------------------------------------

def _constraints(g: Graph, node) -> List[Constraint]:
    res: List[Constraint] = []
    for c in g.objects(node, ODRL.constraint):
        left  = next(g.objects(c, ODRL.leftOperand), None)
        op    = next(g.objects(c, ODRL.operator), None)
        right = next(g.objects(c, ODRL.rightOperand), None)
        if isinstance(left, URIRef) and isinstance(op, URIRef) and right is not None:
            res.append(Constraint(c, left, op, right))
    return res

def extract_rules(g: Graph) -> List[Rule]:
    rules: List[Rule] = []
    KINDS = [
        (ODRL.permission,  'permission'),
        (ODRL.prohibition, 'prohibition'),
        (ODRL.prohibited,  'prohibition'),  # alias seen in quizzes
        (ODRL.obligation,  'obligation'),
    ]
    for pol in g.subjects(RDF.type, ODRL.Set):
        for pred, kind in KINDS:
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

# ------------- constraints: operator compatibility --------------------

def _right_items(g: Graph, term) -> Optional[List]:
    """Return list items if rightOperand is an RDF Collection, else None."""
    return as_list(g, term)

def _is_literal(x) -> bool:
    return isinstance(x, Literal)

def _is_uri(x) -> bool:
    return isinstance(x, URIRef)

def _cmp_literals(a: Literal, b: Literal) -> Optional[int]:
    """Return comparison sign for typed literals; None if incomparable."""
    try:
        pa, pb = to_python_literal(a), to_python_literal(b)
        if type(pa) is type(pb):
            if pa < pb: return -1
            if pa > pb: return 1
            return 0
    except Exception:
        pass
    return None

def _valuestr(g: Graph, v) -> str:
    items = _right_items(g, v)
    if items is not None:
        return "(" + ", ".join(qname(g, i) for i in items) + ")"
    return qname(g, v)

def pair_compatible(g: Graph, TH: TypeHierarchy, c1: Constraint, c2: Constraint) -> Tuple[bool, str]:
    """Check if a single value can satisfy BOTH constraints c1 and c2 (same leftOperand)."""
    op1, v1 = c1.op, c1.right
    op2, v2 = c2.op, c2.right

    L1 = _right_items(g, v1)
    L2 = _right_items(g, v2)

    # --- Equality / inequality / numeric ------------------------------
    if op1 == ODRL.eq and op2 == ODRL.eq:
        ok = (v1 == v2)
        return ok, f"eq { _valuestr(g,v1) } & eq { _valuestr(g,v2) } ⇒ {'same' if ok else 'different'}"

    if (op1 == ODRL.eq and op2 == ODRL.neq) or (op1 == ODRL.neq and op2 == ODRL.eq):
        x = v1 if op1 == ODRL.eq else v2
        y = v2 if op1 == ODRL.eq else v1
        ok = (x != y)
        return ok, f"eq { _valuestr(g,x) } & neq { _valuestr(g,y) }"

    if _is_literal(v1) and _is_literal(v2):
        cmp = _cmp_literals(v1, v2)
        if cmp is not None:
            if op1 == ODRL.eq and op2 in (ODRL.lt, ODRL.lteq, ODRL.gt, ODRL.gteq):
                ok = (cmp < 0 if op2 == ODRL.lt else
                      cmp <= 0 if op2 == ODRL.lteq else
                      cmp > 0 if op2 == ODRL.gt else
                      cmp >= 0)
                return ok, f"eq {v1} & {qname(g,op2)} {v2}"
            if op2 == ODRL.eq and op1 in (ODRL.lt, ODRL.lteq, ODRL.gt, ODRL.gteq):
                ok = (cmp > 0 if op1 == ODRL.lt else
                      cmp >= 0 if op1 == ODRL.lteq else
                      cmp < 0 if op1 == ODRL.gt else
                      cmp <= 0)
                return ok, f"{qname(g,op1)} {v1} & eq {v2}"
            # range-range: (op1 v1) & (op2 v2)
            # We look for non-empty intersection.
            if op1 in (ODRL.lt, ODRL.lteq, ODRL.gt, ODRL.gteq) and op2 in (ODRL.lt, ODRL.lteq, ODRL.gt, ODRL.gteq):
                # pick a witness; basic interval consistency
                # Examples: gt(a) & lt(b) ⇒ a < b
                def low(op, val):
                    return (val, op in (ODRL.gt, ODRL.gteq))
                def high(op, val):
                    return (val, op in (ODRL.lt, ODRL.lteq))
                lo_v, lo_inclusive = (v1, op1==ODRL.gteq) if op1 in (ODRL.gt, ODRL.gteq) else (None, False)
                hi_v, hi_inclusive = (v1, op1==ODRL.lteq) if op1 in (ODRL.lt, ODRL.lteq) else (None, False)
                if op2 in (ODRL.gt, ODRL.gteq):
                    lo_v  = v2 if lo_v is None or to_python_literal(v2) > to_python_literal(lo_v) else lo_v
                    lo_inclusive = (op2==ODRL.gteq) if lo_v == v2 else lo_inclusive
                if op2 in (ODRL.lt, ODRL.lteq):
                    hi_v  = v2 if hi_v is None or to_python_literal(v2) < to_python_literal(hi_v) else hi_v
                    hi_inclusive = (op2==ODRL.lteq) if hi_v == v2 else hi_inclusive
                ok = True
                if lo_v is not None and hi_v is not None:
                    if to_python_literal(lo_v) > to_python_literal(hi_v):
                        ok = False
                    elif to_python_literal(lo_v) == to_python_literal(hi_v) and not (lo_inclusive and hi_inclusive):
                        ok = False
                return ok, f"range intersection of {qname(g,op1)} {v1} & {qname(g,op2)} {v2}"

    # --- Set-based: lists and URIs ------------------------------------
    if op1 == ODRL.eq and op2 == ODRL.isAnyOf and L2 is not None:
        return (v1 in L2), f"eq {qname(g,v1)} ∧ isAnyOf { _valuestr(g,v2) }"

    if op2 == ODRL.eq and op1 == ODRL.isAnyOf and L1 is not None:
        return (v2 in L1), f"isAnyOf { _valuestr(g,v1) } ∧ eq {qname(g,v2)}"

    if op1 == ODRL.eq and op2 == ODRL.isNoneOf and L2 is not None:
        return (v1 not in L2), f"eq {qname(g,v1)} ∧ isNoneOf { _valuestr(g,v2) }"

    if op2 == ODRL.eq and op1 == ODRL.isNoneOf and L1 is not None:
        return (v2 not in L1), f"isNoneOf { _valuestr(g,v1) } ∧ eq {qname(g,v2)}"

    if op1 == ODRL.isAnyOf and op2 == ODRL.isAnyOf and L1 is not None and L2 is not None:
        inter = set(L1) & set(L2)
        return (len(inter) > 0), f"isAnyOf ∩ isAnyOf = { {qname(g,x) for x in inter} }"

    if op1 == ODRL.isAnyOf and op2 == ODRL.isNoneOf and L1 is not None and L2 is not None:
        diff = [x for x in L1 if x not in set(L2)]
        return (len(diff) > 0), f"isAnyOf { _valuestr(g,v1) } vs isNoneOf { _valuestr(g,v2) }"

    if op2 == ODRL.isAnyOf and op1 == ODRL.isNoneOf and L1 is not None and L2 is not None:
        diff = [x for x in L2 if x not in set(L1)]
        return (len(diff) > 0), f"isNoneOf { _valuestr(g,v1) } vs isAnyOf { _valuestr(g,v2) }"

    if op1 == ODRL.isAllOf and op2 == ODRL.isAllOf and L1 is not None and L2 is not None:
        # S can contain all of L1 and all of L2 simultaneously
        return True, f"isAllOf { _valuestr(g,v1) } & isAllOf { _valuestr(g,v2) } ⇒ satisfiable"

    if op1 == ODRL.isAllOf and op2 == ODRL.isAnyOf and L1 is not None and L2 is not None:
        return True, f"isAllOf { _valuestr(g,v1) } & isAnyOf { _valuestr(g,v2) } ⇒ satisfiable"

    if op2 == ODRL.isAllOf and op1 == ODRL.isAnyOf and L1 is not None and L2 is not None:
        return True, f"isAnyOf { _valuestr(g,v1) } & isAllOf { _valuestr(g,v2) } ⇒ satisfiable"

    if op1 == ODRL.isAllOf and op2 == ODRL.isNoneOf and L1 is not None and L2 is not None:
        inter = set(L1) & set(L2)
        return (len(inter) == 0), f"isAllOf { _valuestr(g,v1) } vs isNoneOf { _valuestr(g,v2) }"

    if op2 == ODRL.isAllOf and op1 == ODRL.isNoneOf and L1 is not None and L2 is not None:
        inter = set(L2) & set(L1)
        return (len(inter) == 0), f"isNoneOf { _valuestr(g,v1) } vs isAllOf { _valuestr(g,v2) }"

    # --- isA (class membership) ---------------------------------------
    if op1 == ODRL.eq and op2 == ODRL.isA and _is_uri(v1) and _is_uri(v2):
        ok = TH.is_instance_of(g, v1, v2)
        return ok, f"eq({qname(g,v1)}) ∧ isA({qname(g,v2)}) via rdf:type/subClassOf*"

    if op2 == ODRL.eq and op1 == ODRL.isA and _is_uri(v2) and _is_uri(v1):
        ok = TH.is_instance_of(g, v2, v1)
        return ok, f"isA({qname(g,v1)}) ∧ eq({qname(g,v2)}) via rdf:type/subClassOf*"

    if op1 == ODRL.isA and op2 == ODRL.isA and _is_uri(v1) and _is_uri(v2):
        # Satisfiable if classes are compatible; we confirm if one subsumes the other
        v1sup = TH.closure.get(v1, {v1})
        v2sup = TH.closure.get(v2, {v2})
        ok = (v1 in v2sup) or (v2 in v1sup)
        return (True if ok else True), f"isA {qname(g,v1)} & isA {qname(g,v2)} (assumed compatible unless disjoint info present)"

    # --- hasPart / isPartOf (set-based; opportunistic) -----------------
    # Without explicit partonomy data, we assume potential satisfiability unless directly contradictory with eq.
    if (op1, op2) in ((ODRL.hasPart, ODRL.isPartOf), (ODRL.isPartOf, ODRL.hasPart)):
        return True, f"{qname(g,op1)} vs {qname(g,op2)} assumed satisfiable in absence of partonomy data"

    # Fallback: conservative default (no definite overlap reason found)
    return False, f"operators {qname(g,op1)} vs {qname(g,op2)}: no constructive witness found"

def constraints_overlap(g: Graph, TH: TypeHierarchy, r1: Rule, r2: Rule) -> Tuple[bool, str]:
    """
    Two rules overlap on constraints iff for every shared leftOperand L,
    there exists at least one pair of constraints (c1 in r1, c2 in r2)
    on L that is jointly satisfiable.
    If a leftOperand appears only on one side, we treat it as unconstrained ⇒ overlap.
    """
    byL1: Dict[URIRef, List[Constraint]] = {}
    byL2: Dict[URIRef, List[Constraint]] = {}
    for c in r1.constraints:
        byL1.setdefault(c.left, []).append(c)
    for c in r2.constraints:
        byL2.setdefault(c.left, []).append(c)

    shared = set(byL1.keys()) & set(byL2.keys())
    if not shared:
        return True, "no shared leftOperand constraints"

    reasons = []
    for L in shared:
        okL = False
        reasonL = "no compatible pair"
        for c1 in byL1[L]:
            for c2 in byL2[L]:
                ok, why = pair_compatible(g, TH, c1, c2)
                if ok:
                    okL = True
                    reasonL = f"{qname(g,L)}: {why}"
                    break
            if okL:
                break
        if not okL:
            return False, f"incompatible on {qname(g,L)} ({reasonL})"
        reasons.append(reasonL)
    return True, " ; ".join(reasons)

# ------------- conflict detection -------------------------------------

def same_scope(a: Rule, b: Rule) -> bool:
    if REQUIRE_SAME_ASSIGNEE and a.assignee != b.assignee:
        return False
    if REQUIRE_SAME_TARGET and a.target != b.target:
        return False
    return True

class Conflict:
    def __init__(self, kind: str, a: Rule, b: Rule, why_action: str, why_constraints: str):
        self.kind = kind
        self.a, self.b = a, b
        self.why_action = why_action
        self.why_constraints = why_constraints

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
            okC, whyC = constraints_overlap(gq, TH, p, n)
            if not okC:
                continue
            conflicts.append(Conflict('perm-vs-prohib', p, n, whyA, whyC))

    # Duty blocked by Prohibition
    for d in duties:
        for n in prohib:
            if not same_scope(d, n):
                continue
            okA, whyA = AH.overlap(d.action, n.action)
            if not okA:
                continue
            okC, whyC = constraints_overlap(gq, TH, d, n)
            if not okC:
                continue
            conflicts.append(Conflict('duty-blocked', d, n, whyA, whyC))

    return rules, conflicts

# ------------- proof printing ----------------------------------------

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
        right_str = _valuestr(g, c.right)
        lines.append(f"  {bn} odrl:constraint [ odrl:leftOperand {qname(g,c.left)} ; odrl:operator {qname(g,c.op)} ; odrl:rightOperand {right_str} ] .")
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
        label = "Permission vs Prohibition" if c.kind == "perm-vs-prohib" else "Obligation blocked by Prohibition"
        print(f"[{i:02d}] CONFLICT: {label}")
        for r in (c.a, c.b):
            for line in rule_fact_lines(gq, r):
                print("  " + line)
        print(f"  Action overlap: {c.why_action}")
        print(f"  Constraint overlap: {c.why_constraints}")
        ass = qname(gq, c.a.assignee) if c.a.assignee else "?"
        tgt = qname(gq, c.a.target) if c.a.target else "?"
        print(f"  Same assignee/target: {ass} / {tgt}")
        print("  Therefore: conflict.\n")

# ------------- main ---------------------------------------------------

def main() -> int:
    gv = Graph()
    try:
        gv.parse(ODRL_TTL_URL, format="turtle")
    except Exception as e:
        sys.stderr.write(f"[warn] Could not fetch ODRL22.ttl ({e}). Proceeding without it (reduced action/type reasoning).\n")
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

