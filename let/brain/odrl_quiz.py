#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
ODRL quiz solver with:
  • Conflict / Ambiguous / No conflict classification
  • Logical Constraints (or, xone, and, andSequence)
  • Operators: eq, neq, lt, lteq, gt, gteq, isAnyOf, isNoneOf, isAllOf, isA
  • Clause reasoning + P-only witnesses for Ambiguous
  • EYE-style proof printout
  • Robust parsing (constraints via odrl:constraint OR logical props attached directly)
  • Fallback action hierarchy if ODRL22.ttl is unavailable

This program is designed to evaluate the SolidLabResearch ODRL test quizzes:
  - 00-01 … 00-10
  - 01-01 … 01-10
  - 02-01 … 02-03

It prints ONE verdict per quiz file (Ambiguous > Conflict > No conflict),
plus a single representative “proof block”. Switch OUTPUT_MODE to "pairs"
to see all pairwise checks.

---------------------------------------------------------------------------
HOW THE REASONER WORKS (short version)
---------------------------------------------------------------------------
1) Each ODRL rule (permission, prohibition, obligation) is parsed into a
   list of constraint *expressions*. Expressions can be:
     - Atom: (leftOperand, operator, rightOperand)
     - Logical: Or([...]), Xone([...]), And([...]), AndSeq([...])

2) We expand each rule to a list of *clauses*. A clause is a *conjunction*
   of atoms, grouped by leftOperand:
       Clause = { leftOperandURI : [Atom, Atom, ...], ... }
   Expansion rules (distribute AND over OR):
     - Atom        → [ {L: [Atom]} ]
     - And/AndSeq  → cross-product merge of the children’s clause lists
     - Or/Xone     → union (concatenate) of the children’s clause lists
   (xone behaves like or for overlap detection; that’s conservative and
    is sufficient for these quizzes.)

3) A Permission-clause (P) and Prohibition-clause (N) *overlap* if:
     - either is unconstrained (applies everywhere), OR
     - they share at least one leftOperand L and the atoms about L are
       jointly satisfiable (there exists a v(L) satisfying all atoms).
   If no P-clause/N-clause pair overlaps, the file verdict is "No conflict".

4) If *some* pair overlaps, we attempt to build a P-only witness on a shared
   operand L: a value v such that P’s atoms on L hold but N’s atoms on L fail.
   - If we can, that pair is Ambiguous; if any pair is Ambiguous, the file verdict is Ambiguous.
   - Otherwise, we have Conflict.

---------------------------------------------------------------------------
DESIGN CHOICES (kept explicit and test-friendly)
---------------------------------------------------------------------------
• Scope: we compare only rules having the SAME assignee and SAME target.
  (Change REQUIRE_SAME_* if you need different scoping.)
• Actions: overlap by equality, odrl:includedIn*, rdfs:subClassOf*.
  A fallback action hierarchy provides common inclusions when ODRL22.ttl
  isn’t available (e.g., read/distribute/reproduce ⊑ use).
• Operators: 
  - Literals (numbers/dates/datetimes): eq, neq, lt, lteq, gt, gteq
    handled via equality or interval intersection.
  - Lists/URIs: isAnyOf, isNoneOf, isAllOf (single-valued approximation),
    eq; isA via rdf:type and rdfs:subClassOf*.
• Ambiguous witnesses: for discrete values we try eq and list items; for
  numeric/date boundaries we try values just inside P’s range or just outside
  N’s range (using small ε steps).
"""

from __future__ import annotations

import sys
import datetime
from decimal import Decimal
from typing import Dict, List, Optional, Set, Tuple

from rdflib import Graph, Namespace, URIRef, BNode, RDF, RDFS, Literal
from rdflib.namespace import DCTERMS, XSD
from rdflib.collection import Collection


# ======================================================================
# CONFIG
# ======================================================================

ODRL = Namespace("http://www.w3.org/ns/odrl/2/")
ODRL_TTL_URL = "https://www.w3.org/ns/odrl/2/ODRL22.ttl"

# Logical-constraint properties (bracket notation is safest across rdflib versions)
ODRL_OR          = ODRL['or']
ODRL_AND         = ODRL['and']
ODRL_XONE        = ODRL['xone']
ODRL_ANDSEQUENCE = ODRL['andSequence']

# Full quiz list (00-01..00-10, 01-01..01-10, 02-01..02-03)
QUIZ_URLS = [
    # 00-*
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
    # 01-*
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
    # 02-*
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-02-01.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-02-02.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-02-03.ttl",
]

# Output style:
#   "summary" -> one verdict per quiz file (with one representative proof)
#   "pairs"   -> print every pair (useful for debugging)
OUTPUT_MODE = "summary"

# Scope: compare only rules with same assignee & same target
REQUIRE_SAME_ASSIGNEE = True
REQUIRE_SAME_TARGET   = True


# ======================================================================
# UTILITIES
# ======================================================================

def qname(g: Graph, term) -> str:
    """QName-ish pretty-printer with safe fallbacks."""
    if isinstance(term, URIRef):
        try:
            return g.namespace_manager.normalizeUri(term)
        except Exception:
            return str(term)
    if isinstance(term, Literal):
        return str(term)
    if isinstance(term, BNode):
        return f"_:{term}"
    return str(term)

def to_python_literal(lit: Literal):
    """rdflib Literal -> Python object where possible."""
    try:
        return lit.toPython()
    except Exception:
        return lit

def as_list(g: Graph, node) -> Optional[List]:
    """If `node` is the head of an RDF Collection, return a Python list; else None."""
    try:
        return list(Collection(g, node))
    except Exception:
        return None

def rt_closure(edges: Dict[URIRef, Set[URIRef]]) -> Dict[URIRef, Set[URIRef]]:
    """Reflexive–transitive closure over a directed graph {s:{o,…}}."""
    closure: Dict[URIRef, Set[URIRef]] = {}
    nodes = set(edges.keys()) | {y for ys in edges.values() for y in ys}
    for a in nodes:
        seen = set(); stack = [a]
        while stack:
            x = stack.pop()
            if x in seen: continue
            seen.add(x)
            for y in edges.get(x, ()):
                if y not in seen: stack.append(y)
        seen.add(a)  # reflexive
        closure[a] = seen
    return closure

def _valuestr(g: Graph, v) -> str:
    """Stringify a rightOperand (show lists nicely)."""
    items = as_list(g, v)
    return "(" + ", ".join(qname(g, i) for i in items) + ")" if items is not None else qname(g, v)


# ======================================================================
# HIERARCHIES (actions & classes)
# ======================================================================

class ActionHierarchy:
    """
    Action overlap via: equality, odrl:includedIn*, rdfs:subClassOf*.
    Fallback edges are provided if ODRL22.ttl is unavailable.
    """
    FALLBACK_EDGES = [
        (ODRL.read,       ODRL.use),
        (ODRL.distribute, ODRL.use),
        (ODRL.reproduce,  ODRL.use),
        (ODRL.present,    ODRL.use),
        (ODRL.play,       ODRL.use),
        (ODRL.print,      ODRL.use),
        (ODRL.display,    ODRL.use),
        (ODRL.stream,     ODRL.use),
    ]

    def __init__(self, g_vocab: Graph, g_quiz: Graph):
        edges: Dict[URIRef, Set[URIRef]] = {}
        def add(s, o):
            if isinstance(s, URIRef) and isinstance(o, URIRef):
                edges.setdefault(s, set()).add(o)

        has_includedIn = False
        for s, _, o in g_vocab.triples((None, ODRL.includedIn, None)): add(s, o); has_includedIn = True
        for s, _, o in g_quiz.triples((None, ODRL.includedIn, None)): add(s, o); has_includedIn = True
        for s, _, o in g_quiz.triples((None, RDFS.subClassOf, None)): add(s, o)

        if not has_includedIn:
            for s, o in self.FALLBACK_EDGES: add(s, o)

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
    """rdfs:subClassOf* for isA checks (URIs only)."""
    def __init__(self, g_vocab: Graph, g_quiz: Graph):
        edges: Dict[URIRef, Set[URIRef]] = {}
        def add(s, o):
            if isinstance(s, URIRef) and isinstance(o, URIRef):
                edges.setdefault(s, set()).add(o)
        for s, _, o in g_vocab.triples((None, RDFS.subClassOf, None)): add(s, o)
        for s, _, o in g_quiz.triples((None, RDFS.subClassOf, None)): add(s, o)
        self.closure = rt_closure(edges)

    def is_instance_of(self, g: Graph, x: URIRef, C: URIRef) -> bool:
        for T in g.objects(x, RDF.type):
            if isinstance(T, URIRef) and C in self.closure.get(T, {T}):
                return True
        return False


# ======================================================================
# MODEL & PARSING (atoms + logical constraints)
# ======================================================================

class Atom:
    """Atomic constraint: leftOperand / operator / rightOperand."""
    def __init__(self, left: URIRef, op: URIRef, right):
        self.left, self.op, self.right = left, op, right

class Expr:  # base class (Atom or Logical)
    pass

class Or(Expr):
    def __init__(self, kids: List[Expr]): self.kids = kids
class And(Expr):
    def __init__(self, kids: List[Expr]): self.kids = kids
class Xone(Expr):
    def __init__(self, kids: List[Expr]): self.kids = kids
class AndSeq(Expr):
    def __init__(self, kids: List[Expr]): self.kids = kids

class Rule:
    """
    Flattened rule for comparison:
      • kind: 'permission' | 'prohibition' | 'obligation'
      • policy: policy IRI (subject of odrl:Set)
      • node:   blank node of the rule
      • assignee, action, target
      • exprs:  list[Expr] — top-level constraints (implicitly ANDed)
    """
    def __init__(self, kind: str, policy, node, assignee, action, target, exprs: List[Expr]):
        self.kind, self.policy, self.node = kind, policy, node
        self.assignee, self.action, self.target = assignee, action, target
        self.exprs = exprs


def _parse_expr(g: Graph, cnode) -> Optional[Expr]:
    """
    Parse a constraint node into an Expr:
      • Atom if it has leftOperand/operator/rightOperand
      • Or/And/Xone/AndSeq if it has odrl:or/and/xone/andSequence
        whose value is an RDF list of constraint nodes.
    """
    left  = next(g.objects(cnode, ODRL.leftOperand), None)
    op    = next(g.objects(cnode, ODRL.operator), None)
    right = next(g.objects(cnode, ODRL.rightOperand), None)

    # Atomic constraint?
    if isinstance(left, URIRef) and isinstance(op, URIRef) and right is not None:
        return Atom(left, op, right)

    # Logical constraint on this node? (bracket-notation props)
    for prop, Ctor in ((ODRL_OR, Or), (ODRL_AND, And), (ODRL_XONE, Xone), (ODRL_ANDSEQUENCE, AndSeq)):
        for coll in g.objects(cnode, prop):
            kids: List[Expr] = []
            for it in (as_list(g, coll) or []):
                e = _parse_expr(g, it)
                if e is not None:
                    kids.append(e)
            if kids:
                return Ctor(kids)

    # Unknown/unsupported shape: ignore to stay robust
    return None


def _parse_rule_exprs(g: Graph, rnode) -> List[Expr]:
    """
    Collect (and parse) all constraints for a rule node, robustly:

    1) Standard: rnode odrl:constraint _:c .   (ODRL LogicalConstraint or Atom)
       → parse _:c with _parse_expr

    2) Defensive: some encodings attach logical ops directly to the rule:
           rnode odrl:or ( _:a _:b ) .
       → read those too.

    3) If we parse nothing but we *do* see any logical props or constraint
       triples under rnode, record a neutral And([]) to ensure the rule is
       NOT treated as "unconstrained". (This avoids false
       "both clauses unconstrained" in case of exotic shapes.)
    """
    exprs: List[Expr] = []
    saw_any_struct = False

    # 1) Standard: odrl:constraint -> node -> parse
    for c in g.objects(rnode, ODRL.constraint):
        saw_any_struct = True
        e = _parse_expr(g, c)
        if e is not None:
            exprs.append(e)

    # 2) Defensive: logicals directly on the rule bnode
    for prop, Ctor in ((ODRL_OR, Or), (ODRL_AND, And), (ODRL_XONE, Xone), (ODRL_ANDSEQUENCE, AndSeq)):
        for coll in g.objects(rnode, prop):
            saw_any_struct = True
            kids: List[Expr] = []
            for it in (as_list(g, coll) or []):
                e = _parse_expr(g, it)
                if e is not None:
                    kids.append(e)
            if kids:
                exprs.append(Ctor(kids))

    # 3) Sanity: if we saw structure but parsed nothing, keep a tautology And([])
    if not exprs and saw_any_struct:
        exprs.append(And([]))

    return exprs


def extract_rules(g: Graph) -> List[Rule]:
    """
    Extract rules. We accept both odrl:prohibition and odrl:prohibited
    (the quizzes commonly use the latter).
    """
    out: List[Rule] = []
    KINDS = [
        (ODRL.permission,  'permission'),
        (ODRL.prohibition, 'prohibition'),
        (ODRL.prohibited,  'prohibition'),  # alias
        (ODRL.obligation,  'obligation'),
    ]
    for pol in g.subjects(RDF.type, ODRL.Set):
        for pred, kind in KINDS:
            for rnode in g.objects(pol, pred):
                assignees = list(g.objects(rnode, ODRL.assignee)) or [None]
                actions   = [a for a in g.objects(rnode, ODRL.action) if isinstance(a, URIRef)]
                targets   = list(g.objects(rnode, ODRL.target)) or [None]
                exprs     = _parse_rule_exprs(g, rnode)
                for a in assignees:
                    for act in actions:
                        for t in targets:
                            out.append(Rule(kind, pol, rnode, a, act, t, exprs))
    return out


# ======================================================================
# CLAUSE EXPANSION
# ======================================================================

# A "clause" is a conjunction (AND-list) of Atoms grouped by leftOperand:
#    Clause = { leftOperandURI : [Atom, Atom, ...], ... }
Clause = Dict[URIRef, List[Atom]]

def _merge_and(c1: Clause, c2: Clause) -> Clause:
    """Merge two clauses under logical AND (concatenate atom lists)."""
    r: Clause = {k: v[:] for k, v in c1.items()}
    for L, atoms in c2.items():
        r.setdefault(L, []).extend(atoms)
    return r

def _expand_expr(e: Expr) -> List[Clause]:
    """
    Expand an Expr into a list of *conjunctive* clauses:
      • Atom      → [ {L: [Atom]} ]
      • And/AndSeq→ cross-product merge of children’s clauses
      • Or/Xone   → union (concat) of children’s clause lists
        (XOR exact-one is approximated by union for overlap detection,
         which is conservative and sufficient for these quizzes.)
    """
    if isinstance(e, Atom):
        return [{ e.left: [e] }]

    if isinstance(e, And) or isinstance(e, AndSeq):
        if not e.kids:  # empty And behaves as a tautology
            return [ {} ]
        acc: List[Clause] = [ {} ]
        for kid in e.kids:
            parts = _expand_expr(kid)
            new_acc: List[Clause] = []
            for a in acc:
                for p in parts:
                    new_acc.append(_merge_and(a, p))
            acc = new_acc
        return acc

    if isinstance(e, Or) or isinstance(e, Xone):
        out: List[Clause] = []
        for kid in e.kids:
            out.extend(_expand_expr(kid))
        return out

    return [ {} ]  # defensive fallback

def expand_rule_to_clauses(r: Rule) -> List[Clause]:
    """
    A rule has a top-level *list* of Exprs that are ANDed.
    Expand each Expr to clauses, then merge across the list.
    """
    clauses: List[Clause] = [ {} ]
    for e in r.exprs:
        parts = _expand_expr(e)
        new_clauses: List[Clause] = []
        for c in clauses:
            for p in parts:
                new_clauses.append(_merge_and(c, p))
        clauses = new_clauses
    return clauses


# ======================================================================
# OPERATOR SEMANTICS (per leftOperand)
# ======================================================================

def _right_items(g: Graph, term) -> Optional[List]:
    return as_list(g, term)

def _cmp_literals(a: Literal, b: Literal) -> Optional[int]:
    """Compare typed literals of the same Python type. Return -1/0/1 or None."""
    try:
        pa, pb = to_python_literal(a), to_python_literal(b)
        if type(pa) is type(pb):
            if pa < pb: return -1
            if pa > pb: return 1
            return 0
    except Exception:
        pass
    return None

def _satisfiable_on_operand(g: Graph, TH: TypeHierarchy, atoms: List[Atom]) -> bool:
    """
    Is there *some* value v that satisfies ALL atoms for this operand?

    Supported:
      • Literals: eq, neq, lt, lteq, gt, gteq (interval reasoning)
      • Lists/URIs: isAnyOf, isNoneOf, isAllOf, eq
      • isA (URIs; via rdf:type and rdfs:subClassOf*)
    """
    if not atoms:
        return True

    # ---- Literals: either equality or intersection of a [low, high] interval ----
    lit_atoms = [a for a in atoms if isinstance(a.right, Literal)]
    if lit_atoms:
        eqs = [a.right for a in lit_atoms if a.op == ODRL.eq]
        if eqs:
            # Single equality candidate must satisfy all inequalities
            v = eqs[0]
            for e in eqs[1:]:
                if _cmp_literals(v, e) != 0:
                    return False
            for a in lit_atoms:
                if a.op == ODRL.eq: continue
                sign = _cmp_literals(v, a.right)
                if sign is None: return False
                if a.op == ODRL.lt   and not (sign < 0):  return False
                if a.op == ODRL.lteq and not (sign <= 0): return False
                if a.op == ODRL.gt   and not (sign > 0):  return False
                if a.op == ODRL.gteq and not (sign >= 0): return False
        else:
            # Build intersected interval [lo, hi] with inclusivity flags
            lo_v = None; lo_inc = False
            hi_v = None; hi_inc = False
            for a in lit_atoms:
                if a.op in (ODRL.gt, ODRL.gteq):
                    if lo_v is None or to_python_literal(a.right) > to_python_literal(lo_v):
                        lo_v, lo_inc = a.right, (a.op == ODRL.gteq)
                if a.op in (ODRL.lt, ODRL.lteq):
                    if hi_v is None or to_python_literal(a.right) < to_python_literal(hi_v):
                        hi_v, hi_inc = a.right, (a.op == ODRL.lteq)
            if lo_v is not None and hi_v is not None:
                lo, hi = to_python_literal(lo_v), to_python_literal(hi_v)
                if lo > hi: return False
                if lo == hi and not (lo_inc and hi_inc): return False

    # ---- URIs / lists: filter a candidate set (best-effort) ----
    cand: Optional[Set] = None
    for a in atoms:
        if a.op == ODRL.eq and isinstance(a.right, URIRef):
            cand = {a.right} if cand is None else (cand & {a.right})
            if cand == set(): return False
        elif a.op == ODRL.isAnyOf and _right_items(g, a.right) is not None:
            opts = set(_right_items(g, a.right))
            cand = opts if cand is None else (cand & opts)
            if not cand: return False
        elif a.op == ODRL.isNoneOf and _right_items(g, a.right) is not None:
            forb = set(_right_items(g, a.right))
            if cand is not None:
                cand = {x for x in cand if x not in forb}
                if not cand: return False
            # if cand is None, satisfiable in principle (choose any x ∉ forb)
        elif a.op == ODRL.isAllOf and _right_items(g, a.right) is not None:
            # Single-valued approximation: satisfiable only if exactly one item
            items = _right_items(g, a.right)
            if len(items) != 1: return False
            one = items[0]
            cand = {one} if cand is None else (cand & {one})
            if not cand: return False
        elif a.op == ODRL.isA and isinstance(a.right, URIRef):
            if cand is not None:
                keep = {x for x in cand if isinstance(x, URIRef) and TH.is_instance_of(g, x, a.right)}
                cand = keep
                if not cand: return False
            # else: unknown instance universe → assume satisfiable

    return True


# ======================================================================
# AMBIGUITY WITNESS (P-only value on a shared operand)
# ======================================================================

def _holds_atom(g: Graph, TH: TypeHierarchy, a: Atom, v) -> Optional[bool]:
    """Evaluate a single atom on a *candidate* value v (True/False/None)."""
    op, right = a.op, a.right
    lst = _right_items(g, right)

    if op == ODRL.eq:   return v == right
    if op == ODRL.neq:  return v != right
    if op == ODRL.isAnyOf and lst is not None:  return v in lst
    if op == ODRL.isNoneOf and lst is not None: return v not in lst
    if op == ODRL.isAllOf and lst is not None:  return (len(lst) == 1 and v == lst[0])

    if op == ODRL.isA and isinstance(v, URIRef) and isinstance(right, URIRef):
        return TH.is_instance_of(g, v, right)

    if op in (ODRL.lt, ODRL.lteq, ODRL.gt, ODRL.gteq) and isinstance(right, Literal) and isinstance(v, Literal):
        sign = _cmp_literals(v, right)
        if sign is None: return None
        if op == ODRL.lt:   return sign < 0
        if op == ODRL.lteq: return sign <= 0
        if op == ODRL.gt:   return sign > 0
        if op == ODRL.gteq: return sign >= 0

    return None

def _step_like(right: Literal, direction: str) -> Optional[Literal]:
    """
    Produce a value just above/below `right` with the same datatype:
      • xsd:integer  → ±1
      • xsd:decimal  → ±1e-4
      • xsd:date     → ±1 day
      • xsd:dateTime → ±1 second
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

def _candidates_from_bounds_P(g: Graph, p_atoms: List[Atom]) -> List[Literal]:
    """Candidate numeric/date values from P bounds (inside P’s range)."""
    out: List[Literal] = []
    for a in p_atoms:
        if not isinstance(a.right, Literal): continue
        if a.op in (ODRL.gteq, ODRL.lteq, ODRL.eq): out.append(a.right)
        elif a.op == ODRL.gt:
            s = _step_like(a.right, "up");  out.append(s) if s else None
        elif a.op == ODRL.lt:
            s = _step_like(a.right, "down"); out.append(s) if s else None
    return [x for x in out if x is not None]

def _candidates_outside_bounds_N(g: Graph, n_atoms: List[Atom]) -> List[Literal]:
    """Candidate numeric/date values OUTSIDE N’s range (likely P-only)."""
    out: List[Literal] = []
    for a in n_atoms:
        if not isinstance(a.right, Literal): continue
        if a.op == ODRL.lteq:
            s = _step_like(a.right, "up");  out.append(s) if s else None
        elif a.op == ODRL.lt:
            out.append(a.right)  # equals boundary (not <)
        elif a.op == ODRL.gteq:
            s = _step_like(a.right, "down"); out.append(s) if s else None
        elif a.op == ODRL.gt:
            out.append(a.right)  # equals boundary (not >)
    return [x for x in out if x is not None]

def _p_only_on_operand(g: Graph, TH: TypeHierarchy, p_atoms: List[Atom], n_atoms: List[Atom]) -> Tuple[bool, Optional[object]]:
    """
    Is there a value v that satisfies ALL P atoms on this operand but
    fails at least one N atom (so N does not apply on this operand)?
    """
    cand: List = []
    for A in p_atoms + n_atoms:
        if A.op == ODRL.eq:
            cand.append(A.right)
        else:
            L = _right_items(g, A.right)
            if L: cand.extend(L)
    cand.extend(_candidates_from_bounds_P(g, p_atoms))
    cand.extend(_candidates_outside_bounds_N(g, n_atoms))

    # deduplicate
    seen = set(); uniq = []
    for v in cand:
        key = (v, getattr(v, 'datatype', None))
        if key in seen: continue
        seen.add(key); uniq.append(v)

    for v in uniq:
        okP = all(_holds_atom(g, TH, A, v) is not False for A in p_atoms)
        okN = all(_holds_atom(g, TH, A, v) is not False for A in n_atoms)
        if okP and not okN:
            return True, v
    return False, None


# ======================================================================
# CLAUSE OVERLAP & PAIR ANALYSIS
# ======================================================================

def same_scope(a: Rule, b: Rule) -> bool:
    """Require same assignee/target if configured so."""
    if REQUIRE_SAME_ASSIGNEE and a.assignee != b.assignee: return False
    if REQUIRE_SAME_TARGET   and a.target   != b.target:   return False
    return True

def _clauses_overlap(g: Graph, TH: TypeHierarchy, pc: Clause, nc: Clause) -> Tuple[bool, str, bool, Optional[str]]:
    """
    Do permission-clause pc and prohibition-clause nc overlap?

    Overlap iff:
      • both empty (unconstrained) → overlap
      • one empty → overlap (unconstrained side applies everywhere)
      • else: there exists at least one SHARED leftOperand, and the
        combined atoms on that operand are jointly satisfiable.

    Returns:
      (overlap_bool, why_text, ambiguous_for_this_pair, witness_text|None)
    """
    if not pc and not nc:
        return True, "both clauses unconstrained", False, None
    if not pc:
        return True, "permission clause unconstrained", False, None
    if not nc:
        return True, "prohibition clause unconstrained", False, None

    shared = set(pc.keys()) & set(nc.keys())
    if not shared:
        return False, "no shared leftOperand in selected branches", False, None

    # All shared operands must be individually satisfiable
    for L in shared:
        atoms = (pc.get(L, []) or []) + (nc.get(L, []) or [])
        if not _satisfiable_on_operand(g, TH, atoms):
            return False, f"incompatible on {qname(g, L)}", False, None

    # Try to witness Ambiguous: some shared L where P-only value exists
    for L in shared:
        okPonly, v = _p_only_on_operand(g, TH, pc.get(L, []), nc.get(L, []))
        if okPonly:
            return True, f"overlap on {', '.join(qname(g, L) for L in shared)}", True, f"{qname(g, L)} = {qname(g, v)}"

    return True, f"overlap on {', '.join(qname(g, L) for L in shared)}", False, None

class PairResult:
    def __init__(self, kind: str, status: str, a: Rule, b: Rule, why_action: str, details: str):
        self.kind, self.status = kind, status              # 'perm-vs-prohib'|'duty-blocked' ; status string
        self.a, self.b = a, b
        self.why_action = why_action
        self.details = details

def analyze_pairs(g_vocab: Graph, gq: Graph) -> List[PairResult]:
    """
    Compare:
      • Permission vs Prohibition
      • Obligation vs Prohibition
    under the *same scope* (assignee/target).
    """
    AH = ActionHierarchy(g_vocab, gq)
    TH = TypeHierarchy(g_vocab, gq)

    rules = extract_rules(gq)
    perms  = [r for r in rules if r.kind == 'permission']
    prohib = [r for r in rules if r.kind == 'prohibition']
    duties = [r for r in rules if r.kind == 'obligation']

    results: List[PairResult] = []

    def eval(kind: str, X: Rule, N: Rule):
        okA, whyA = AH.overlap(X.action, N.action)
        if not okA:
            return  # cannot conflict if actions disjoint

        pclauses = expand_rule_to_clauses(X) or [ {} ]
        nclauses = expand_rule_to_clauses(N) or [ {} ]

        any_overlap = False
        any_ambig   = False
        reasons     = []

        for pc in pclauses:
            for nc in nclauses:
                ok, why, amb_here, wit = _clauses_overlap(gq, TH, pc, nc)
                if ok:
                    any_overlap = True
                    if amb_here:
                        any_ambig = True
                        reasons.append(f"branch overlap: {why}; safe witness: {wit}")
                    else:
                        reasons.append(f"branch overlap: {why}")
                else:
                    reasons.append(f"branch non-overlap: {why}")

        if any_overlap:
            status = "Ambiguous" if any_ambig else "Conflict"
        else:
            status = "No conflict"

        results.append(PairResult(kind, status, X, N, whyA, "; ".join(reasons)))

    for p in perms:
        for n in prohib:
            if same_scope(p, n):
                eval('perm-vs-prohib', p, n)

    for d in duties:
        for n in prohib:
            if same_scope(d, n):
                eval('duty-blocked', d, n)

    return results


# ======================================================================
# PRINTING
# ======================================================================

def rule_fact_lines(g: Graph, r: Rule) -> List[str]:
    """
    EYE-style facts (concise). We print rule header + subject/verb/object lines.
    For logical constraints, the explanatory "Details" shows which branches
    overlapped or were incompatible.
    """
    pol = qname(g, r.policy); bn = qname(g, r.node)
    ass = qname(g, r.assignee) if r.assignee else "?"
    act = qname(g, r.action);  tgt = qname(g, r.target) if r.target else "?"

    return [
        f"{pol} {r.kind}: {bn}",
        f"  {bn} odrl:assignee {ass} ;",
        f"  {bn} odrl:action  {act} ;",
        f"  {bn} odrl:target  {tgt} .",
    ]

def summarize_pairs(pairs: List[PairResult]) -> Tuple[str, Optional[PairResult], int, int]:
    """
    File-level summary: Ambiguous > Conflict > No conflict.
    Return (overall_status, exemplar_pair, total_pairs, num_elided).
    """
    if not pairs:
        return "No conflict", None, 0, 0
    for want in ("Ambiguous", "Conflict", "No conflict"):
        bucket = [p for p in pairs if p.status == want]
        if bucket:
            return want, bucket[0], len(pairs), len(pairs) - 1
    return "No conflict", pairs[0], len(pairs), len(pairs) - 1

def print_pairs(name: str, gq: Graph, pairs: List[PairResult]) -> None:
    """Verbose mode: all pairs."""
    print(f"\n=== {name} ===")
    if not pairs:
        print("No conflicts detected.")
        return
    for i, pr in enumerate(pairs, start=1):
        label = {"Conflict":"CONFLICT","Ambiguous":"AMBIGUOUS","No conflict":"NO CONFLICT"}[pr.status]
        kind  = "Permission vs Prohibition" if pr.kind == "perm-vs-prohib" else "Obligation blocked by Prohibition"
        print(f"[{i:02d}] {label}: {kind}")
        for r in (pr.a, pr.b):
            for line in rule_fact_lines(gq, r):
                print("  " + line)
        print(f"  Action overlap: {pr.why_action}")
        print(f"  Details: {pr.details}")
        print()

def print_summary(name: str, g_vocab: Graph, url: str) -> None:
    """Summary mode: one verdict per file + exemplar proof."""
    gq = Graph()
    gq.namespace_manager.bind("odrl", ODRL, override=True)
    gq.namespace_manager.bind("dct", DCTERMS, override=False)
    gq.namespace_manager.bind("rdfs", RDFS, override=False)

    gq.parse(url, format="turtle")
    pairs = analyze_pairs(g_vocab, gq)

    overall, exemplar, total, elided = summarize_pairs(pairs)

    print(f"\n=== {name} ===")
    print(f"Overall: {overall} ({total} pair(s) analyzed)")

    if exemplar:
        pr = exemplar
        label = {"Conflict":"CONFLICT","Ambiguous":"AMBIGUOUS","No conflict":"NO CONFLICT"}[pr.status]
        kind  = "Permission vs Prohibition" if pr.kind == "perm-vs-prohib" else "Obligation blocked by Prohibition"
        print(f"[example] {label}: {kind}")
        for r in (pr.a, pr.b):
            for line in rule_fact_lines(gq, r):
                print("  " + line)
        print(f"  Action overlap: {pr.why_action}")
        print(f"  Details: {pr.details}")
        if elided > 0:
            print(f"... ({elided} other pair(s) elided)")

def print_report(name: str, g_vocab: Graph, url: str) -> None:
    """Dispatch to the selected output mode."""
    if OUTPUT_MODE == "pairs":
        gq = Graph()
        gq.namespace_manager.bind("odrl", ODRL, override=True)
        gq.namespace_manager.bind("dct", DCTERMS, override=False)
        gq.namespace_manager.bind("rdfs", RDFS, override=False)
        gq.parse(url, format="turtle")
        pairs = analyze_pairs(g_vocab, gq)
        print_pairs(name, gq, pairs)
    else:
        print_summary(name, g_vocab, url)


# ======================================================================
# MAIN
# ======================================================================

def main() -> int:
    # Load ODRL vocabulary for action hierarchy and (optional) classes.
    gv = Graph()
    try:
        gv.parse(ODRL_TTL_URL, format="turtle")
    except Exception as e:
        sys.stderr.write(
            "[warn] Could not fetch ODRL22.ttl "
            f"({e}). Using fallback action hierarchy (read/distribute/reproduce ⊑ use, etc.).\n"
        )
        gv = Graph()
    gv.namespace_manager.bind("odrl", ODRL, override=True)
    gv.namespace_manager.bind("rdfs", RDFS, override=False)

    # Run all quizzes
    for url in QUIZ_URLS:
        name = url.rsplit("/", 1)[-1]
        try:
            print_report(name, gv, url)
        except Exception as e:
            print(f"\n=== {name} ===")
            print(f"Error: {e}")
    return 0

if __name__ == "__main__":
    sys.exit(main())

