#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
ODRL quiz solver with "Ambiguous" classification and summary output.

==================================================
OVERVIEW
==================================================

This tool loads a set of ODRL policy "quiz" files and prints reasoning
traces in a style inspired by EYE examples:

    premises  ->  overlap reason(s)  ->  conclusion

We check two kinds of pairs over the *same assignee and same target*:
  1) Permission  vs  Prohibition
  2) Obligation  vs  Prohibition   (duty blocked)

The key question is whether the Permission/Obligation "states of the world"
(over actions + constraints) overlap with the Prohibition "states" and how.

We return one of three statuses per pair:
  - No conflict : there is no world that satisfies both sides.
  - Conflict    : every world that satisfies P also satisfies the Prohibition.
  - Ambiguous   : there exists at least one conflicting world (P ∧ N),
                  AND there exists a "safe" world (P ∧ ¬N).
                  Example: P says purpose ∈ {Sell, Distribute}; N says ¬{Sell}.
                  Then "Sell" is prohibited (conflict exists); "Distribute"
                  is permitted (safe witness). Overall -> Ambiguous.

In "summary" mode we report a *single* verdict per quiz file:
  Ambiguous > Conflict > No conflict
and print just one representative proof (others are elided).


==================================================
ASSUMPTIONS / SEMANTICS (kept simple and explicit)
==================================================

ACTIONS
-------
Two actions "overlap" if they are equal or related by one-step-or-more
of either:
  - odrl:includedIn
  - rdfs:subClassOf
We compute the reflexive–transitive closure over those relations.

SCOPE
-----
We compare only pairs having the SAME odrl:assignee and SAME odrl:target.
(You can change this in the config flags below if you want different scope
matching semantics.)

CONSTRAINTS
-----------
We support the ODRL operator set:

  Equality/inequality/ranges over typed literals
    eq, neq, lt, lteq, gt, gteq

  Class membership (URIs with rdf:type and rdfs:subClassOf*)
    isA

  Set relations over RDF lists of URIs/literals
    isAnyOf, isNoneOf, isAllOf
    (For "isAllOf" on a single-valued attribute, we accept it iff the list
     contains exactly one item and that item equals the value.)

  Partonomy (hasPart, isPartOf)
    We do not attempt to build a general partonomy; absent explicit data,
    we conservatively treat these as potentially satisfiable; they will not
    by themselves force "No conflict". You can plug a partonomy reasoner if
    needed.

Constraints within a single rule for the same leftOperand are treated
**conjunctively** (all must hold). Our *compatibility* test between two rules
checks, for each shared leftOperand L, that there exists at least one value
satisfying (all constraints on L in rule A) ∧ (all constraints on L in rule B).
In practice the quizzes have ≤1 constraint per operand per rule; if your data
uses multiple per L, this solver still works (with the above conjunction).

AMBIGUOUS (SAFE WITNESSES)
--------------------------
To prove "Ambiguous", we must exhibit a *safe witness* assignment for each
shared leftOperand L:
  v(L) such that  P allows v(L)  AND  N does not apply at v(L).
We construct candidates from:
  - any eq-values and items from isAnyOf/isNoneOf/isAllOf,
  - bounds in P (gteq/gt/lteq/lt/eq),
  - AND (important) "just-outside" values relative to the bounds in N.
    Examples:
      N has lteq B  -> consider B+ε
      N has lt   B  -> consider B    (since not < B)
      N has gteq A  -> consider A-ε
      N has gt   A  -> consider A    (since not > A)
We support ε steps for xsd:integer, xsd:decimal, xsd:date, and xsd:dateTime.

If we cannot *constructively* produce a witness while we *do* find overlap,
the pair will be reported as "Conflict" (not Ambiguous). You can extend or
tighten witness generation as needed.


==================================================
USAGE
==================================================

  pip install rdflib
  python odrl_quiz_solver.py

Configuration flags are near the top ("CONFIG"):

  - QUIZ_URLS: list of Turtle files (the 20 current ones are included)
  - OUTPUT_MODE: "summary" (single verdict per file) or "pairs" (every pair)
  - REQUIRE_SAME_ASSIGNEE / REQUIRE_SAME_TARGET: scope matching

The script downloads the ODRL vocabulary from:
  https://www.w3.org/ns/odrl/2/ODRL22.ttl
If unavailable, it still runs but action hierarchy reasoning is limited.

"""

from __future__ import annotations

import sys
import datetime
from decimal import Decimal
from typing import Dict, List, Optional, Set, Tuple

from rdflib import Graph, Namespace, URIRef, BNode, RDF, RDFS, Literal
from rdflib.namespace import DCTERMS, XSD
from rdflib.collection import Collection


# ==================================================
# CONFIG
# ==================================================

ODRL = Namespace("http://www.w3.org/ns/odrl/2/")
ODRL_TTL_URL = "https://www.w3.org/ns/odrl/2/ODRL22.ttl"

# The 20 current quizzes (00-01..00-10, 01-01..01-10).
# Feel free to add/remove files here.
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
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-09.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-10.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-02-01.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-02-02.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-02-03.ttl",
]

# Output mode:
#   "summary" -> single verdict per quiz file (with one representative proof)
#   "pairs"   -> print every analyzed pair (useful for debugging)
OUTPUT_MODE = "summary"

# Scope matching: require same assignee/target to compare rules.
REQUIRE_SAME_ASSIGNEE = True
REQUIRE_SAME_TARGET   = True


# ==================================================
# UTILS
# ==================================================

def qname(g: Graph, term) -> str:
    """Pretty printer for QName or literal, with safe fallback."""
    if isinstance(term, URIRef):
        try:
            return g.namespace_manager.normalizeUri(term)
        except Exception:
            return str(term)
    if isinstance(term, Literal):
        return str(term)  # includes ^^datatype in rdflib's __str__
    if isinstance(term, BNode):
        return f"_:{term}"
    return str(term)

def to_python_literal(lit: Literal):
    """Convert rdflib Literal -> native Python object where possible."""
    try:
        return lit.toPython()
    except Exception:
        return lit

def as_list(g: Graph, node) -> Optional[List]:
    """
    If 'node' is the head of an RDF Collection, return its Python list.
    Otherwise return None. (We ignore plain RDF Seq/Bag/Alt here.)
    """
    try:
        return list(Collection(g, node))
    except Exception:
        return None

def rt_closure(edges: Dict[URIRef, Set[URIRef]]) -> Dict[URIRef, Set[URIRef]]:
    """
    Compute reflexive–transitive closure over a directed graph.

    For each node 'a', we produce the set of nodes reachable from 'a' by
    following zero-or-more edges (hence reflexive). This is used for both:
      - odrl:includedIn (action hierarchy)
      - rdfs:subClassOf (actions and classes)
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
        seen.add(a)  # reflexive
        closure[a] = seen
    return closure


# ==================================================
# HIERARCHIES (actions, classes)
# ==================================================

class ActionHierarchy:
    """
    Encapsulates "action overlap":
      - equality
      - odrl:includedIn*
      - rdfs:subClassOf*  (some quizzes define custom actions via subclass)
    """
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
    """
    rdfs:subClassOf* closure for class membership checks (isA).

    We don't attempt OWL reasoning; we use:
       x rdf:type T   and   T ⊑* C   =>   x isA C
    """
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


# ==================================================
# MODEL (Rules and Constraints)
# ==================================================

class Constraint:
    """
    Reified ODRL constraint:
        node:  the blank node of the constraint
        left:  odrl:leftOperand   (URI)
        op:    odrl:operator      (URI)
        right: odrl:rightOperand  (URI | Literal | BNode(list))
    """
    def __init__(self, node, left: URIRef, op: URIRef, right):
        self.node = node
        self.left = left
        self.op = op
        self.right = right


class Rule:
    """
    Flattened rule for checking pairs:
        kind:   'permission' | 'prohibition' | 'obligation'
        policy: policy IRI (subject of odrl:Set)
        node:   blank node of the rule
        assignee, action, target
        constraints: [Constraint]
    """
    def __init__(self, kind: str, policy, node, assignee, action, target, constraints: List[Constraint]):
        self.kind = kind
        self.policy = policy
        self.node = node
        self.assignee = assignee
        self.action = action
        self.target = target
        self.constraints = constraints


# ==================================================
# EXTRACTION (rules + constraints)
# ==================================================

def _constraints(g: Graph, node) -> List[Constraint]:
    """Collect constraints attached to a rule bnode."""
    res: List[Constraint] = []
    for c in g.objects(node, ODRL.constraint):
        left  = next(g.objects(c, ODRL.leftOperand), None)
        op    = next(g.objects(c, ODRL.operator), None)
        right = next(g.objects(c, ODRL.rightOperand), None)
        if isinstance(left, URIRef) and isinstance(op, URIRef) and right is not None:
            res.append(Constraint(c, left, op, right))
    return res


def extract_rules(g: Graph) -> List[Rule]:
    """
    Extract rules from a graph. We accept both:
      - odrl:prohibition (standard)
      - odrl:prohibited  (used in these quizzes)
    """
    out: List[Rule] = []
    kinds = [
        (ODRL.permission,  'permission'),
        (ODRL.prohibition, 'prohibition'),
        (ODRL.prohibited,  'prohibition'),  # alias in the quiz set
        (ODRL.obligation,  'obligation'),
    ]
    for pol in g.subjects(RDF.type, ODRL.Set):
        for pred, kind in kinds:
            for node in g.objects(pol, pred):
                assignees = list(g.objects(node, ODRL.assignee)) or [None]
                actions   = [a for a in g.objects(node, ODRL.action) if isinstance(a, URIRef)]
                targets   = list(g.objects(node, ODRL.target)) or [None]
                cons      = _constraints(g, node)
                for a in assignees:
                    for act in actions:
                        for t in targets:
                            out.append(Rule(kind, pol, node, a, act, t, cons))
    return out


# ==================================================
# OPERATOR SEMANTICS
# ==================================================

def _right_items(g: Graph, term) -> Optional[List]:
    """Return Python list if 'term' is an RDF list head, else None."""
    return as_list(g, term)


def _cmp_literals(a: Literal, b: Literal) -> Optional[int]:
    """
    Compare two typed literals of the same Python type.
    Return -1/0/1 or None if incomparable.
    """
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
    """Stringify rightOperand (handles RDF lists nicely)."""
    items = _right_items(g, v)
    if items is not None:
        return "(" + ", ".join(qname(g, i) for i in items) + ")"
    return qname(g, v)


def pair_compatible(g: Graph, TH: TypeHierarchy, c1: Constraint, c2: Constraint) -> Tuple[bool, str]:
    """
    Is there *some* value v such that v satisfies both c1 and c2?
    We return (True/False, human_reason).

    This is used to prove that two rules' constraints overlap on a given
    leftOperand. If multiple constraints per side exist on the same L,
    we test pairs and look for at least one "compatible" pair. In most
    quiz data there's at most one constraint per side per L.
    """
    op1, v1 = c1.op, c1.right
    op2, v2 = c2.op, c2.right
    L1 = _right_items(g, v1)
    L2 = _right_items(g, v2)

    # Equality / inequality (URIs or literals)
    if op1 == ODRL.eq and op2 == ODRL.eq:
        ok = (v1 == v2)
        return ok, f"eq {qname(g,v1)} & eq {qname(g,v2)}"
    if (op1 == ODRL.eq and op2 == ODRL.neq) or (op1 == ODRL.neq and op2 == ODRL.eq):
        x = v1 if op1 == ODRL.eq else v2
        y = v2 if op1 == ODRL.eq else v1
        ok = (x != y)
        return ok, f"eq {qname(g,x)} & neq {qname(g,y)}"

    # List/set combinations
    if op1 == ODRL.eq and op2 == ODRL.isAnyOf and L2 is not None:
        return (v1 in L2), f"eq {qname(g,v1)} ∧ isAnyOf {_valuestr(g,v2)}"
    if op2 == ODRL.eq and op1 == ODRL.isAnyOf and L1 is not None:
        return (v2 in L1), f"isAnyOf {_valuestr(g,v1)} ∧ eq {qname(g,v2)}"

    if op1 == ODRL.eq and op2 == ODRL.isNoneOf and L2 is not None:
        return (v1 not in L2), f"eq {qname(g,v1)} ∧ isNoneOf {_valuestr(g,v2)}"
    if op2 == ODRL.eq and op1 == ODRL.isNoneOf and L1 is not None:
        return (v2 not in L1), f"isNoneOf {_valuestr(g,v1)} ∧ eq {qname(g,v2)}"

    if op1 == ODRL.isAnyOf and op2 == ODRL.isAnyOf and L1 and L2:
        inter = set(L1) & set(L2)
        return (len(inter) > 0), f"isAnyOf ∩ isAnyOf = { {qname(g,x) for x in inter} }"

    # isAnyOf vs isNoneOf (existence of an item in L1 not present in L2)
    if op1 == ODRL.isAnyOf and op2 == ODRL.isNoneOf and L1 and L2:
        diff = [x for x in L1 if x not in set(L2)]
        return (len(diff) > 0), f"isAnyOf {_valuestr(g,v1)} vs isNoneOf {_valuestr(g,v2)}"
    if op2 == ODRL.isAnyOf and op1 == ODRL.isNoneOf and L1 and L2:
        diff = [x for x in L2 if x not in set(L1)]
        return (len(diff) > 0), f"isNoneOf {_valuestr(g,v1)} vs isAnyOf {_valuestr,g(v2)}"

    # isA (URIs with rdf:type)
    if op1 == ODRL.eq and op2 == ODRL.isA and isinstance(v1, URIRef) and isinstance(v2, URIRef):
        ok = TH.is_instance_of(g, v1, v2)
        return ok, f"eq({qname(g,v1)}) ∧ isA({qname(g,v2)}) via rdf:type/subClassOf*"
    if op2 == ODRL.eq and op1 == ODRL.isA and isinstance(v2, URIRef) and isinstance(v1, URIRef):
        ok = TH.is_instance_of(g, v2, v1)
        return ok, f"isA({qname(g,v1)}) ∧ eq({qname(g,v2)}) via rdf:type/subClassOf*"

    # Numeric/date: eq vs inequality
    if isinstance(v1, Literal) and isinstance(v2, Literal):
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

    # Numeric/date: inequality vs inequality -> check interval intersection
    if (op1 in (ODRL.lt, ODRL.lteq, ODRL.gt, ODRL.gteq) and
        op2 in (ODRL.lt, ODRL.lteq, ODRL.gt, ODRL.gteq) and
        isinstance(v1, Literal) and isinstance(v2, Literal)):

        # Normalize to combined [low, high] bounds.
        def update_low(lo_v, lo_inc, val, inclusive):
            if lo_v is None or to_python_literal(val) > to_python_literal(lo_v):
                return val, inclusive
            return lo_v, lo_inc

        def update_high(hi_v, hi_inc, val, inclusive):
            if hi_v is None or to_python_literal(val) < to_python_literal(hi_v):
                return val, inclusive
            return hi_v, hi_inc

        lo_v = None; lo_inc = False
        hi_v = None; hi_inc = False

        if op1 in (ODRL.gt, ODRL.gteq):
            lo_v, lo_inc = update_low(lo_v, lo_inc, v1, op1 == ODRL.gteq)
        if op1 in (ODRL.lt, ODRL.lteq):
            hi_v, hi_inc = update_high(hi_v, hi_inc, v1, op1 == ODRL.lteq)

        if op2 in (ODRL.gt, ODRL.gteq):
            lo_v, lo_inc = update_low(lo_v, lo_inc, v2, op2 == ODRL.gteq)
        if op2 in (ODRL.lt, ODRL.lteq):
            hi_v, hi_inc = update_high(hi_v, hi_inc, v2, op2 == ODRL.lteq)

        ok = True
        if lo_v is not None and hi_v is not None:
            lo = to_python_literal(lo_v); hi = to_python_literal(hi_v)
            if lo > hi:
                ok = False
            elif lo == hi and not (lo_inc and hi_inc):
                ok = False

        return ok, f"range intersection of {qname(g,op1)} {v1} & {qname(g,op2)} {v2}"

    # Partonomy: treated as potentially satisfiable absent explicit data.
    if (c1.op, c2.op) in ((ODRL.hasPart, ODRL.isPartOf), (ODRL.isPartOf, ODRL.hasPart)):
        return True, f"{qname(g,c1.op)} vs {qname(g,c2.op)} assumed satisfiable (no partonomy)"

    # Fallback: we couldn't construct a compatible witness.
    return False, f"{qname(g,c1.op)} vs {qname(g,c2.op)}: no compatible witness found"


# ==================================================
# WITNESSES (for "Ambiguous")
# ==================================================

def _holds(g: Graph, TH: TypeHierarchy, c: Constraint, v) -> Optional[bool]:
    """
    Check whether a *candidate value* v satisfies a single constraint c.
    Return True/False, or None if unknown (e.g., incomparable datatypes).
    """
    op, right = c.op, c.right
    lst = _right_items(g, right)

    # Equality/inequality
    if op == ODRL.eq:   return v == right
    if op == ODRL.neq:  return v != right

    # Set/list-based
    if op == ODRL.isAnyOf and lst is not None:  return v in lst
    if op == ODRL.isNoneOf and lst is not None: return v not in lst
    if op == ODRL.isAllOf and lst is not None:  return (len(lst) == 1 and v == lst[0])

    # Class membership
    if op == ODRL.isA and isinstance(v, URIRef) and isinstance(right, URIRef):
        return TH.is_instance_of(g, v, right)

    # Numeric/date ranges
    if op in (ODRL.lt, ODRL.lteq, ODRL.gt, ODRL.gteq) and isinstance(right, Literal) and isinstance(v, Literal):
        sign = _cmp_literals(v, right)
        if sign is None: return None
        if op == ODRL.lt:   return sign < 0
        if op == ODRL.lteq: return sign <= 0
        if op == ODRL.gt:   return sign > 0
        if op == ODRL.gteq: return sign >= 0

    # Unknown / unsupported combination
    return None


def _step_like(right: Literal, direction: str) -> Optional[Literal]:
    """
    Produce a value just above/below 'right' with the same datatype:
      - xsd:integer  -> step by 1
      - xsd:decimal  -> step by 1e-4 (adjust as needed)
      - xsd:date     -> step by 1 day
      - xsd:dateTime -> step by 1 second
    """
    dt = right.datatype
    py = to_python_literal(right)

    if isinstance(py, int) and (dt == XSD.integer or dt is None):
        return Literal(py + (1 if direction == "up" else -1), datatype=XSD.integer)

    if isinstance(py, Decimal) or dt == XSD.decimal:
        base = py if isinstance(py, Decimal) else Decimal(str(py))
        eps = Decimal("0.0001")
        return Literal(base + (eps if direction == "up" else -eps), datatype=XSD.decimal)

    if isinstance(py, datetime.date) and not isinstance(py, datetime.datetime) and dt == XSD.date:
        delta = datetime.timedelta(days=1)
        return Literal(py + (delta if direction == "up" else -delta), datatype=XSD.date)

    if isinstance(py, datetime.datetime) and dt == XSD.dateTime:
        delta = datetime.timedelta(seconds=1)
        return Literal(py + (delta if direction == "up" else -delta), datatype=XSD.dateTime)

    return None


def _candidates_from_P_bounds(g: Graph, r: Rule, L: URIRef) -> List[Literal]:
    """
    Candidate numeric/date values drawn from the Permission/Obligation side.
    We include exact bounds and "just inside" for open bounds where relevant.
    """
    cand: List[Literal] = []
    for c in r.constraints:
        if c.left != L or not isinstance(c.right, Literal):
            continue
        if c.op in (ODRL.gteq, ODRL.lteq, ODRL.eq):
            cand.append(c.right)
        elif c.op == ODRL.gt:
            s = _step_like(c.right, "up")
            if s: cand.append(s)
        elif c.op == ODRL.lt:
            s = _step_like(c.right, "down")
            if s: cand.append(s)
    return cand


def _candidates_from_N_bounds(g: Graph, rN: Rule, L: URIRef) -> List[Literal]:
    """
    Candidate numeric/date values derived from the Prohibition side,
    *outside* the prohibited range (so likely "safe"):

      N : lteq B   ->  choose B+ε
      N : lt   B   ->  choose B        (not < B)
      N : gteq A   ->  choose A-ε
      N : gt   A   ->  choose A        (not > A)
    """
    cand: List[Literal] = []
    for c in rN.constraints:
        if c.left != L or not isinstance(c.right, Literal):
            continue
        if c.op == ODRL.lteq:
            s = _step_like(c.right, "up")
            if s: cand.append(s)
        elif c.op == ODRL.lt:
            cand.append(c.right)
        elif c.op == ODRL.gteq:
            s = _step_like(c.right, "down")
            if s: cand.append(s)
        elif c.op == ODRL.gt:
            cand.append(c.right)
    return cand


def _candidate_values_for_L(g: Graph, TH: TypeHierarchy, rP: Rule, rN: Rule, L: URIRef) -> List:
    """
    Candidates for a shared leftOperand L when searching for "safe" P-only witnesses.
    Sources:
      - explicit eq-values and list items (both sides)
      - numeric/date candidates from P bounds
      - numeric/date candidates *outside* N bounds
    """
    cand: List = []

    # Discrete candidates from eq/lists (both rules)
    for r in (rP, rN):
        for c in r.constraints:
            if c.left != L: continue
            if c.op == ODRL.eq:
                cand.append(c.right)
            else:
                lst = _right_items(g, c.right)
                if lst:
                    cand.extend(lst)

    # Numeric/date candidates from both sides
    cand.extend(_candidates_from_P_bounds(g, rP, L))
    cand.extend(_candidates_from_N_bounds(g, rN, L))

    # De-duplicate (preserve order); include datatype to avoid merging 1 and "1"
    seen = set()
    uniq = []
    for v in cand:
        key = (v, getattr(v, 'datatype', None))
        if key in seen:
            continue
        seen.add(key)
        uniq.append(v)
    return uniq


def _allowed_by_rule_on_L(g: Graph, TH: TypeHierarchy, r: Rule, L: URIRef, v) -> bool:
    """
    Does candidate value 'v' satisfy ALL constraints on leftOperand L in rule r?
    If the rule has no constraints on L -> True (unconstrained).
    Unknown checks (None) are treated permissively here for witness search.
    """
    relevant = [c for c in r.constraints if c.left == L]
    if not relevant:
        return True
    for c in relevant:
        verdict = _holds(g, TH, c, v)
        if verdict is False:
            return False
    return True


def _pair_ambiguous_check(g: Graph, TH: TypeHierarchy, rP: Rule, rN: Rule) -> Tuple[bool, Optional[Dict[URIRef, object]], str]:
    """
    Try to *construct* an assignment (witness) showing that there is a
    "safe" world where P holds but N does not apply.

    For every shared leftOperand L, pick a value v(L) such that:
      - v(L) satisfies P's constraints on L, and
      - v(L) does NOT satisfy N's constraints on L (or N has none on L).

    If we can do this for all shared L, we return Ambiguous with the witness map.
    """
    byL_P: Dict[URIRef, List[Constraint]] = {}
    byL_N: Dict[URIRef, List[Constraint]] = {}
    for c in rP.constraints: byL_P.setdefault(c.left, []).append(c)
    for c in rN.constraints: byL_N.setdefault(c.left, []).append(c)

    shared = set(byL_P.keys()) & set(byL_N.keys())
    if not shared:
        # Without shared operands, we cannot distinguish "safe" vs "conflicting" states constructively.
        return False, None, "no shared operands"

    witness: Dict[URIRef, object] = {}

    for L in shared:
        found = False
        for v in _candidate_values_for_L(g, TH, rP, rN, L):
            okP = _allowed_by_rule_on_L(g, TH, rP, L, v)
            okN = _allowed_by_rule_on_L(g, TH, rN, L, v)
            if okP and not okN:
                witness[L] = v
                found = True
                break
        if not found:
            return False, None, f"no P-only value for {qname(g,L)}"

    return True, witness, "constructed P-only witness"


# ==================================================
# CONSTRAINT OVERLAP (existence of *some* conflicting world)
# ==================================================

def constraints_overlap(g: Graph, TH: TypeHierarchy, r1: Rule, r2: Rule) -> Tuple[bool, str]:
    """
    Do r1 and r2 have at least one *shared* leftOperand L whose constraints
    are simultaneously satisfiable? If an operand appears only on one side,
    we treat it as unconstrained on the other side (hence not preventing overlap).
    """
    byL1: Dict[URIRef, List[Constraint]] = {}
    byL2: Dict[URIRef, List[Constraint]] = {}

    for c in r1.constraints: byL1.setdefault(c.left, []).append(c)
    for c in r2.constraints: byL2.setdefault(c.left, []).append(c)

    shared = set(byL1.keys()) & set(byL2.keys())
    if not shared:
        return True, "no shared leftOperand constraints"

    reasons = []
    for L in shared:
        ok_any = False
        whyL = "no compatible pair"
        # Look for *some* pair (c1,c2) on L that can hold together.
        for c1 in byL1[L]:
            for c2 in byL2[L]:
                ok, why = pair_compatible(g, TH, c1, c2)
                if ok:
                    ok_any = True
                    whyL = f"{qname(g,L)}: {why}"
                    break
            if ok_any:
                break
        if not ok_any:
            return False, f"incompatible on {qname(g,L)} ({whyL})"
        reasons.append(whyL)

    return True, " ; ".join(reasons)


# ==================================================
# PAIR ANALYSIS AND SUMMARY
# ==================================================

def same_scope(a: Rule, b: Rule) -> bool:
    """Scope test: require same assignee/target if configured so."""
    if REQUIRE_SAME_ASSIGNEE and a.assignee != b.assignee: return False
    if REQUIRE_SAME_TARGET   and a.target   != b.target:   return False
    return True


class PairResult:
    """Result record for a single pair (X vs N) of rules."""
    def __init__(self, kind: str, status: str, a: Rule, b: Rule,
                 why_action: str, why_constraints: str,
                 safe_witness: Optional[Dict[URIRef, object]] = None,
                 safe_reason: Optional[str] = None):
        self.kind = kind  # 'perm-vs-prohib' | 'duty-blocked'
        self.status = status  # 'No conflict' | 'Conflict' | 'Ambiguous'
        self.a, self.b = a, b
        self.why_action = why_action
        self.why_constraints = why_constraints
        self.safe_witness = safe_witness
        self.safe_reason = safe_reason


def analyze_pairs(g_vocab: Graph, gq: Graph) -> List[PairResult]:
    """
    Compute pairwise results:
      - Permission vs Prohibition
      - Obligation vs Prohibition
    Only pairs passing same_scope() are considered.
    """
    rules = extract_rules(gq)
    AH = ActionHierarchy(g_vocab, gq)
    TH = TypeHierarchy(g_vocab, gq)

    perms  = [r for r in rules if r.kind == 'permission']
    prohib = [r for r in rules if r.kind == 'prohibition']
    duties = [r for r in rules if r.kind == 'obligation']

    results: List[PairResult] = []

    def eval_pair(kind: str, X: Rule, N: Rule):
        okA, whyA = AH.overlap(X.action, N.action)
        if not okA:
            return  # cannot conflict if actions disjoint
        okC, whyC = constraints_overlap(gq, TH, X, N)
        if not okC:
            results.append(PairResult(kind, "No conflict", X, N, whyA, whyC))
            return
        # Try to prove Ambiguous by constructing a "P-only" witness.
        amb, wit, whyW = _pair_ambiguous_check(gq, TH, X, N)
        if amb:
            results.append(PairResult(kind, "Ambiguous", X, N, whyA, whyC, wit, whyW))
        else:
            results.append(PairResult(kind, "Conflict", X, N, whyA, whyC))

    for p in perms:
        for n in prohib:
            if same_scope(p, n):
                eval_pair('perm-vs-prohib', p, n)

    for d in duties:
        for n in prohib:
            if same_scope(d, n):
                eval_pair('duty-blocked', d, n)

    return results


def summarize_pairs(pairs: List[PairResult]) -> Tuple[str, Optional[PairResult], int, int]:
    """
    Reduce many pairwise results to a single verdict per quiz file.

    Priority: Ambiguous > Conflict > No conflict.
    Return (overall_status, exemplar_pair, total_pairs, num_elided_pairs).
    The exemplar is chosen from the highest-priority bucket.
    """
    total = len(pairs)
    if total == 0:
        return "No conflict", None, 0, 0

    # Prefer Ambiguous; then Conflict; else No conflict
    for want in ("Ambiguous", "Conflict", "No conflict"):
        bucket = [p for p in pairs if p.status == want]
        if bucket:
            exemplar = bucket[0]
            return want, exemplar, total, total - 1

    # Should not happen
    return "No conflict", pairs[0], total, total - 1


# ==================================================
# PRINTING (proof-style)
# ==================================================

def rule_fact_lines(g: Graph, r: Rule) -> List[str]:
    pol = qname(g, r.policy)
    bn  = qname(g, r.node)
    ass = qname(g, r.assignee) if r.assignee else "?"
    act = qname(g, r.action)
    tgt = qname(g, r.target) if r.target else "?"

    lines = []
    lines.append(f"{pol} {r.kind}: {bn}")
    lines.append(f"  {bn} odrl:assignee {ass} ;")
    lines.append(f"  {bn} odrl:action  {act} ;")
    lines.append(f"  {bn} odrl:target  {tgt} .")
    for c in r.constraints:
        lines.append(
            f"  {bn} odrl:constraint [ "
            f"odrl:leftOperand {qname(g,c.left)} ; "
            f"odrl:operator {qname(g,c.op)} ; "
            f"odrl:rightOperand {_valuestr(g,c.right)} ] ."
        )
    return lines


def print_pairs(name: str, gq: Graph, pairs: List[PairResult]) -> None:
    """Verbose (debug) mode: print every pair."""
    if not pairs:
        print(f"\n=== {name} ===")
        print("No conflicts detected.")
        return

    print(f"\n=== {name} ===")
    for i, pr in enumerate(pairs, start=1):
        label = {"Conflict": "CONFLICT", "Ambiguous": "AMBIGUOUS", "No conflict": "NO CONFLICT"}[pr.status]
        kind  = "Permission vs Prohibition" if pr.kind == "perm-vs-prohib" else "Obligation blocked by Prohibition"
        print(f"[{i:02d}] {label}: {kind}")
        for r in (pr.a, pr.b):
            for line in rule_fact_lines(gq, r):
                print("  " + line)
        print(f"  Action overlap: {pr.why_action}")
        print(f"  Constraint assessment: {pr.why_constraints}")
        ass = qname(gq, pr.a.assignee) if pr.a.assignee else "?"
        tgt = qname(gq, pr.a.target)   if pr.a.target   else "?"
        print(f"  Same assignee/target: {ass} / {tgt}")
        if pr.status == "Ambiguous" and pr.safe_witness:
            ws = "; ".join(f"{qname(gq,L)} = {qname(gq,v)}" for L, v in pr.safe_witness.items())
            print(f"  Safe witness (permission but not prohibited): {ws}")
            print(f"  Witness reasoning: {pr.safe_reason}")
        print("  Conclusion:", pr.status, "\n")


def print_summary(name: str, gq: Graph, pairs: List[PairResult]) -> None:
    """Compact mode: one verdict per file + one exemplar proof."""
    overall, exemplar, total, elided = summarize_pairs(pairs)

    print(f"\n=== {name} ===")
    print(f"Overall: {overall} ({total} pair(s) analyzed)")

    if exemplar:
        pr = exemplar
        label = {"Conflict": "CONFLICT", "Ambiguous": "AMBIGUOUS", "No conflict": "NO CONFLICT"}[pr.status]
        kind  = "Permission vs Prohibition" if pr.kind == "perm-vs-prohib" else "Obligation blocked by Prohibition"
        print(f"[example] {label}: {kind}")
        for r in (pr.a, pr.b):
            for line in rule_fact_lines(gq, r):
                print("  " + line)
        print(f"  Action overlap: {pr.why_action}")
        print(f"  Constraint assessment: {pr.why_constraints}")
        ass = qname(gq, pr.a.assignee) if pr.a.assignee else "?"
        tgt = qname(gq, pr.a.target)   if pr.a.target   else "?"
        print(f"  Same assignee/target: {ass} / {tgt}")
        if pr.status == "Ambiguous" and pr.safe_witness:
            ws = "; ".join(f"{qname(gq,L)} = {qname(gq,v)}" for L, v in pr.safe_witness.items())
            print(f"  Safe witness (permission but not prohibited): {ws}")
            print(f"  Witness reasoning: {pr.safe_reason}")
        if elided > 0:
            print(f"... ({elided} other pair(s) elided)")


# ==================================================
# MAIN
# ==================================================

def print_report(name: str, g_vocab: Graph, quiz_url: str) -> None:
    """Load one quiz file, analyze, and print in the selected mode."""
    gq = Graph()
    gq.namespace_manager.bind("odrl", ODRL, override=True)
    gq.namespace_manager.bind("dct", DCTERMS, override=False)
    gq.namespace_manager.bind("rdfs", RDFS, override=False)

    # Parse the quiz file (Turtle)
    gq.parse(quiz_url, format="turtle")

    pairs = analyze_pairs(g_vocab, gq)

    if OUTPUT_MODE == "pairs":
        print_pairs(name, gq, pairs)
    else:
        print_summary(name, gq, pairs)


def main() -> int:
    # Load ODRL vocabulary (for odrl:includedIn and classes).
    gv = Graph()
    try:
        gv.parse(ODRL_TTL_URL, format="turtle")
    except Exception as e:
        sys.stderr.write(f"[warn] Could not fetch ODRL22.ttl ({e}). Proceeding without it (reduced action/type reasoning).\n")
        gv = Graph()
    gv.namespace_manager.bind("odrl", ODRL, override=True)
    gv.namespace_manager.bind("rdfs", RDFS, override=False)

    # Analyze each quiz file.
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

