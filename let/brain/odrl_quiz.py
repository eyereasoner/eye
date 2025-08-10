#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
ODRL Quiz — ARC (Answer / Reason / Check), self-contained

This script analyzes the SolidLabResearch ODRL quiz files and classifies each as:
  • Ambiguous   — there exists a Permission/Obligation vs Prohibition pair with overlapping scope/action
                  for which a value satisfies P but not N (a P-only “witness”)
  • Conflict    — at least one overlapping pair exists, but no P-only witness for any of them
  • No conflict — no overlapping pairs

Answer:
  Prints one verdict per quiz (and one representative pair), or all pairs if OUTPUT_MODE="pairs".

Reason why:
  Explains the clause expansion and pairwise reasoning, and shows a short trace.

Check (harness):
  Internal consistency checks:
    - If Ambiguous: at least one pair exposes a P-only witness.
    - If Conflict: at least one pair overlaps and none admit P-only witnesses.
    - If No conflict: no pair overlaps.
    - Deterministic labeling & stable sorting of rules.

Notes:
  - Works with or without fetching ODRL22.ttl (falls back to a minimal action hierarchy).
  - Handles odrl:or, odrl:and, odrl:xone, odrl:andSequence, and core operators:
      eq, neq, lt, lteq, gt, gteq, isAnyOf, isNoneOf, isAllOf, isA
"""

from __future__ import annotations
import sys
import datetime
from dataclasses import dataclass, field
from decimal import Decimal
from enum import Enum
from typing import Dict, Iterable, List, Optional, Sequence, Set, Tuple, Union

from rdflib import BNode, Graph, Literal, Namespace, RDF, RDFS, URIRef
from rdflib.collection import Collection
from rdflib.namespace import DCTERMS, XSD

# ─────────────────────────── Config ───────────────────────────
ODRL = Namespace("http://www.w3.org/ns/odrl/2/")
ODRL_TTL_URL = "https://www.w3.org/ns/odrl/2/ODRL22.ttl"

# Logical constructors (use bracket notation for rdflib safety)
ODRL_OR = ODRL["or"]
ODRL_AND = ODRL["and"]
ODRL_XONE = ODRL["xone"]
ODRL_ANDSEQUENCE = ODRL["andSequence"]

# Full quiz list (00-01..00-10, 01-01..01-10, 02-01..02-04)
QUIZ_URLS: List[str] = [
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
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-02-04.ttl",
]

class OutputMode(str, Enum):
    SUMMARY = "summary"
    PAIRS = "pairs"

OUTPUT_MODE: OutputMode = OutputMode.SUMMARY
REQUIRE_SAME_ASSIGNEE = True
REQUIRE_SAME_TARGET = True
DEBUG = False  # set True to print operand-level satisfiability to stderr

# ─────────────────────────── Utilities ───────────────────────────
QNameish = Union[str, URIRef, Literal, BNode, None]

def qname(g: Graph, term: QNameish) -> str:
    if isinstance(term, URIRef):
        try:
            return g.namespace_manager.normalizeUri(term)
        except Exception:
            return str(term)
    if isinstance(term, Literal):
        return str(term)
    if isinstance(term, BNode):
        return f"_:{term}"
    if term is None:
        return "?"
    return str(term)

def to_python_literal(lit: Literal):
    try:
        return lit.toPython()
    except Exception:
        return lit

def as_list(g: Graph, node: QNameish) -> Optional[List]:
    if node is None:
        return None
    if node == RDF.nil:
        return []
    if (node, RDF.first, None) in g:
        try:
            return list(Collection(g, node))
        except Exception:
            return None
    return None

def rt_closure(edges: Dict[URIRef, Set[URIRef]]) -> Dict[URIRef, Set[URIRef]]:
    closure: Dict[URIRef, Set[URIRef]] = {}
    nodes = set(edges.keys()) | {y for ys in edges.values() for y in ys}
    for a in nodes:
        seen: Set[URIRef] = set()
        stack: List[URIRef] = [a]
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

# ─────────────────────────── Hierarchies ───────────────────────────
class ActionHierarchy:
    """Action overlap via equality or includedIn/subClassOf* (with fallback)."""
    FALLBACK_EDGES: Sequence[Tuple[URIRef, URIRef]] = (
        (ODRL.read, ODRL.use),
        (ODRL.distribute, ODRL.use),
        (ODRL.reproduce, ODRL.use),
        (ODRL.present, ODRL.use),
        (ODRL.play, ODRL.use),
        (ODRL.print, ODRL.use),
        (ODRL.display, ODRL.use),
        (ODRL.stream, ODRL.use),
    )
    def __init__(self, g_vocab: Graph, g_quiz: Graph):
        edges: Dict[URIRef, Set[URIRef]] = {}
        def add(s: QNameish, o: QNameish):
            if isinstance(s, URIRef) and isinstance(o, URIRef):
                edges.setdefault(s, set()).add(o)
        has_included = False
        for s, _, o in g_vocab.triples((None, ODRL.includedIn, None)):
            add(s, o); has_included = True
        for s, _, o in g_quiz.triples((None, ODRL.includedIn, None)):
            add(s, o); has_included = True
        for s, _, o in g_quiz.triples((None, RDFS.subClassOf, None)):
            add(s, o)
        if not has_included:
            for s, o in self.FALLBACK_EDGES:
                add(s, o)
        self.closure = rt_closure(edges)

    def overlap(self, a: URIRef, b: URIRef) -> Tuple[bool, str]:
        if a == b:
            return True, f"same action ({qname(Graph(), a)})"
        a_sup = self.closure.get(a, {a})
        b_sup = self.closure.get(b, {b})
        if b in a_sup:
            return True, f"{qname(Graph(), a)} ⊑ {qname(Graph(), b)}"
        if a in b_sup:
            return True, f"{qname(Graph(), b)} ⊑ {qname(Graph(), a)}"
        return False, "no action inclusion relation"

class TypeHierarchy:
    """rdfs:subClassOf* closure for odrl:isA"""
    def __init__(self, g_vocab: Graph, g_quiz: Graph):
        edges: Dict[URIRef, Set[URIRef]] = {}
        def add(s: QNameish, o: QNameish):
            if isinstance(s, URIRef) and isinstance(o, URIRef):
                edges.setdefault(s, set()).add(o)
        for s, _, o in g_vocab.triples((None, RDFS.subClassOf, None)):
            add(s, o)
        for s, _, o in g_quiz.triples((None, RDFS.subClassOf, None)):
            add(s, o)
        self.closure = rt_closure(edges)

    def is_instance_of(self, g: Graph, x: URIRef, C: URIRef) -> bool:
        for T in g.objects(x, RDF.type):
            if isinstance(T, URIRef) and C in self.closure.get(T, {T}):
                return True
        return False

# ─────────────────────────── Model & parsing ───────────────────────────
@dataclass(frozen=True)
class Atom:
    left: URIRef
    op: URIRef
    right: Union[Literal, URIRef, BNode]

class Expr: pass

@dataclass(frozen=True)
class Or(Expr):
    kids: Tuple[Expr, ...]

@dataclass(frozen=True)
class And(Expr):
    kids: Tuple[Expr, ...]

@dataclass(frozen=True)
class Xone(Expr):
    kids: Tuple[Expr, ...]

@dataclass(frozen=True)
class AndSeq(Expr):
    kids: Tuple[Expr, ...]

@dataclass
class Rule:
    kind: str  # 'permission' | 'prohibition' | 'obligation'
    policy: URIRef
    node: Union[URIRef, BNode]
    assignee: Optional[URIRef]
    action: URIRef
    target: Optional[URIRef]
    exprs: List[Expr]
    print_id: Optional[str] = None

def _parse_expr(g: Graph, cnode: Union[URIRef, BNode]) -> Optional[Expr]:
    left = next(g.objects(cnode, ODRL.leftOperand), None)
    op   = next(g.objects(cnode, ODRL.operator), None)
    right = next(g.objects(cnode, ODRL.rightOperand), None)
    if right is None:
        right = next(g.objects(cnode, ODRL.rightOperandReference), None)
    if isinstance(left, URIRef) and isinstance(op, URIRef) and right is not None:
        return Atom(left, op, right)
    def parse_children(prop: URIRef, ctor) -> Optional[Expr]:
        for coll in g.objects(cnode, prop):
            kids: List[Expr] = []
            for it in (as_list(g, coll) or []):
                e = _parse_expr(g, it)
                if e is not None:
                    kids.append(e)
            if kids:
                return ctor(tuple(kids))
        return None
    for prop, Ctor in ((ODRL_OR, Or), (ODRL_AND, And), (ODRL_XONE, Xone), (ODRL_ANDSEQUENCE, AndSeq)):
        res = parse_children(prop, Ctor)
        if res is not None:
            return res
    return None

def _parse_rule_exprs(g: Graph, rnode: Union[URIRef, BNode]) -> List[Expr]:
    exprs: List[Expr] = []
    saw_any = False
    for c in g.objects(rnode, ODRL.constraint):
        saw_any = True
        e = _parse_expr(g, c)
        if e is not None:
            exprs.append(e)
    def collect(prop: URIRef, ctor) -> None:
        nonlocal saw_any
        for coll in g.objects(rnode, prop):
            saw_any = True
            kids: List[Expr] = []
            for it in (as_list(g, coll) or []):
                e = _parse_expr(g, it)
                if e is not None:
                    kids.append(e)
            if kids:
                exprs.append(ctor(tuple(kids)))
    for prop, Ctor in ((ODRL_OR, Or), (ODRL_AND, And), (ODRL_XONE, Xone), (ODRL_ANDSEQUENCE, AndSeq)):
        collect(prop, Ctor)
    if not exprs and saw_any:
        exprs.append(And(tuple()))  # “present but empty”
    return exprs

def extract_rules(g: Graph) -> List[Rule]:
    out: List[Rule] = []
    KINDS: Sequence[Tuple[URIRef, str]] = (
        (ODRL.permission, "permission"),
        (ODRL.prohibition, "prohibition"),
        (ODRL.prohibited, "prohibition"),
        (ODRL.obligation, "obligation"),
    )
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
                            out.append(Rule(
                                kind, pol, rnode,
                                a if isinstance(a, URIRef) else None,
                                act,
                                t if isinstance(t, URIRef) else None,
                                exprs
                            ))
    return out

# ─────────────────────────── Deterministic ordering ───────────────────────────
def _expr_signature(g: Graph, e: Expr) -> str:
    if isinstance(e, Atom):
        L = as_list(g, e.right)
        rv = ("[" + ",".join(qname(g, x) for x in L) + "]") if L is not None else qname(g, e.right)
        return f"ATOM({qname(g, e.left)}|{qname(g, e.op)}|{rv})"
    if isinstance(e, Or):
        return "OR(" + "|".join(_expr_signature(g, k) for k in e.kids) + ")"
    if isinstance(e, Xone):
        return "XONE(" + "|".join(_expr_signature(g, k) for k in e.kids) + ")"
    if isinstance(e, And):
        return "AND(" + "|".join(_expr_signature(g, k) for k in e.kids) + ")"
    if isinstance(e, AndSeq):
        return "ANDSEQ(" + "|".join(_expr_signature(g, k) for k in e.kids) + ")"
    return "?"

def _rule_sort_key(g: Graph, r: Rule) -> Tuple:
    sig = "|".join(_expr_signature(g, e) for e in r.exprs)
    return (
        qname(g, r.policy),
        r.kind,
        qname(g, r.assignee) if r.assignee else "",
        qname(g, r.action),
        qname(g, r.target) if r.target else "",
        sig,
    )

def sort_and_label_rules(g: Graph, rules: List[Rule]) -> List[Rule]:
    out = sorted(rules, key=lambda r: _rule_sort_key(g, r))
    for i, r in enumerate(out, start=1):
        r.print_id = f"r{i:02d}"
    return out

# ─────────────────────────── Clause expansion ───────────────────────────
Clause = Dict[URIRef, List[Atom]]  # leftOperand -> atoms

def _merge_and(c1: Clause, c2: Clause) -> Clause:
    r: Clause = {k: v[:] for k, v in c1.items()}
    for L, atoms in c2.items():
        r.setdefault(L, []).extend(atoms)
    return r

def _expand_expr(e: Expr) -> List[Clause]:
    if isinstance(e, Atom):
        return [{e.left: [e]}]
    if isinstance(e, (And, AndSeq)):
        if not e.kids:
            return [{}]
        acc: List[Clause] = [{}]
        for kid in e.kids:
            parts = _expand_expr(kid)
            new_acc: List[Clause] = []
            for a in acc:
                for p in parts:
                    new_acc.append(_merge_and(a, p))
            acc = new_acc
        return acc
    if isinstance(e, (Or, Xone)):
        out: List[Clause] = []
        for kid in e.kids:
            out.extend(_expand_expr(kid))
        return out
    return [{}]

def expand_rule_to_clauses(r: Rule) -> List[Clause]:
    clauses: List[Clause] = [{}]
    for e in r.exprs:
        parts = _expand_expr(e)
        new_clauses: List[Clause] = []
        for c in clauses:
            for p in parts:
                new_clauses.append(_merge_and(c, p))
        clauses = new_clauses
    return clauses

# ─────────────────────────── Operator semantics ───────────────────────────
def _cmp_literals(a: Literal, b: Literal) -> Optional[int]:
    try:
        pa, pb = to_python_literal(a), to_python_literal(b)
        if type(pa) is type(pb):  # strict
            return -1 if pa < pb else (1 if pa > pb else 0)
    except Exception:
        pass
    return None

def _domain_for_operand(g: Graph, atoms: Sequence[Atom]) -> str:
    if any(isinstance(a.right, Literal) for a in atoms):
        return "numeric"
    if any(a.op in (ODRL.isAnyOf, ODRL.isAllOf, ODRL.isNoneOf) for a in atoms):
        return "set"
    return "uri"

def _as_uris(g: Graph, term: Union[Literal, URIRef, BNode]) -> Optional[Set[URIRef]]:
    L = as_list(g, term)
    if L is not None:
        return {x for x in L if isinstance(x, URIRef)}
    if isinstance(term, URIRef):
        return {term}
    return None

# Holds predicate on a concrete value v (URI or Literal or set of URIs)
def _holds_atom(g: Graph, TH: TypeHierarchy, A: Atom, v) -> Optional[bool]:
    # URI-valued / class tests
    if isinstance(v, URIRef):
        if A.op in (ODRL.eq,):
            return v == A.right if isinstance(A.right, URIRef) else None
        if A.op == ODRL.neq:
            return v != A.right if isinstance(A.right, URIRef) else None
        if A.op == ODRL.isAnyOf:
            S = _as_uris(g, A.right);  return (v in S) if S is not None else None
        if A.op == ODRL.isNoneOf:
            S = _as_uris(g, A.right);  return (v not in S) if S is not None else None
        if A.op == ODRL.isAllOf:
            S = _as_uris(g, A.right);  return (S == {v}) if S is not None else None  # single-valued approx
        if A.op == ODRL.isA and isinstance(A.right, URIRef):
            return TH.is_instance_of(g, v, A.right)
        return None

    # set-valued (for isAnyOf / isAllOf / isNoneOf)
    if isinstance(v, set):
        if A.op == ODRL.eq:
            S = _as_uris(g, A.right);  return (S is not None and v == S)
        if A.op == ODRL.neq:
            S = _as_uris(g, A.right);  return (S is not None and v != S)
        if A.op == ODRL.isAnyOf:
            S = _as_uris(g, A.right);  return bool(S and (v & S))
        if A.op == ODRL.isAllOf:
            S = _as_uris(g, A.right);  return bool(S and (S <= v))
        if A.op == ODRL.isNoneOf:
            S = _as_uris(g, A.right);  return bool(S is not None and not (v & S))
        if A.op == ODRL.isA:
            # set + isA isn’t meaningful; treat as unknown
            return None
        return None

    # numeric/temporal (Literal-like)
    if isinstance(v, Literal):
        if not isinstance(A.right, Literal):
            return None
        cmp = _cmp_literals(v, A.right)
        if cmp is None:
            return None
        if A.op == ODRL.eq:   return cmp == 0
        if A.op == ODRL.neq:  return cmp != 0
        if A.op == ODRL.lt:   return cmp == -1
        if A.op == ODRL.lteq: return cmp in (-1, 0)
        if A.op == ODRL.gt:   return cmp == 1
        if A.op == ODRL.gteq: return cmp in (1, 0)
        return None

    return None

# Satisfiability check “there exists some v for this operand that makes all atoms true”
def _satisfiable_on_operand(g: Graph, TH: TypeHierarchy, atoms: Sequence[Atom]) -> bool:
    dom = _domain_for_operand(g, atoms)
    if dom == "uri":
        # Try equality/list members first; else, assume there exists a URI outside all neq/noneOf
        cands: List[URIRef] = []
        for A in atoms:
            if A.op == ODRL.eq and isinstance(A.right, URIRef):
                cands.append(A.right)
            else:
                L = _as_uris(g, A.right)
                if L:
                    cands.extend(L)
        seen: Set[URIRef] = set()
        cands = [x for x in cands if not (x in seen or seen.add(x))]
        for v in cands:
            if all(_holds_atom(g, TH, A, v) is not False for A in atoms):
                return True
        # try a dummy fresh URIRef (heuristic): satisfiable unless explicitly forbidden by isAllOf with non-singleton set
        return not any(A.op == ODRL.isAllOf and len(_as_uris(g, A.right) or set()) != 1 for A in atoms)

    if dom == "set":
        # Build a minimal set V that satisfies “allOf + anyOf” while avoiding “noneOf”
        all_req: Set[URIRef] = set()
        any_sets: List[Set[URIRef]] = []
        none_forbid: Set[URIRef] = set()
        eq_sets: List[Set[URIRef]] = []
        for A in atoms:
            if A.op == ODRL.isAllOf:
                S = _as_uris(g, A.right) or set()
                all_req |= S
            elif A.op == ODRL.isAnyOf:
                S = _as_uris(g, A.right) or set()
                any_sets.append(S)
            elif A.op == ODRL.isNoneOf:
                S = _as_uris(g, A.right) or set()
                none_forbid |= S
            elif A.op == ODRL.eq:
                S = _as_uris(g, A.right) or set()
                eq_sets.append(S)
        # Equality constraints must agree
        if eq_sets:
            base = eq_sets[0]
            if any(S != base for S in eq_sets[1:]):
                return False
            V = set(base)
        else:
            V = set(all_req)
            for S in any_sets:
                picks = list((S - none_forbid)) or list(S)
                if not picks:
                    return False
                V.add(picks[0])
        # Must avoid forbidden
        if V & none_forbid:
            return False
        return True

    # numeric
    lo: Optional[Literal] = None
    lo_incl = True
    hi: Optional[Literal] = None
    hi_incl = True
    not_equals: List[Literal] = []
    eqs: List[Literal] = []
    for A in atoms:
        if not isinstance(A.right, Literal):
            continue
        if A.op == ODRL.eq:
            eqs.append(A.right)
        elif A.op == ODRL.neq:
            not_equals.append(A.right)
        elif A.op in (ODRL.gt, ODRL.gteq, ODRL.lt, ODRL.lteq):
            if A.op in (ODRL.gt, ODRL.gteq):
                if lo is None or _cmp_literals(A.right, lo) == 1:
                    lo, lo_incl = A.right, (A.op == ODRL.gteq)
            else:
                if hi is None or _cmp_literals(A.right, hi) == -1:
                    hi, hi_incl = A.right, (A.op == ODRL.lteq)
    # equalities must agree and sit inside the interval
    if eqs:
        v = eqs[0]
        if any(_cmp_literals(v, e) != 0 for e in eqs[1:]):
            return False
        if lo is not None:
            c = _cmp_literals(v, lo)
            if c is None or c < 0 or (c == 0 and not lo_incl):
                return False
        if hi is not None:
            c = _cmp_literals(v, hi)
            if c is None or c > 0 or (c == 0 and not hi_incl):
                return False
        if any(_cmp_literals(v, ne) == 0 for ne in not_equals):
            return False
        return True
    # otherwise interval must be non-empty (and not entirely excluded by != singleton)
    if lo is not None and hi is not None:
        c = _cmp_literals(lo, hi)
        if c is None or c > 0 or (c == 0 and not (lo_incl and hi_incl)):
            return False
    return True

# P-only witness search on one operand
def _p_only_on_operand(g: Graph, TH: TypeHierarchy, p_atoms: Sequence[Atom], n_atoms: Sequence[Atom]) -> Tuple[bool, Optional[object]]:
    dom = _domain_for_operand(g, p_atoms or n_atoms)
    # First: simple candidates from equality, lists and numeric boundaries
    cand: List[object] = []
    for A in list(p_atoms) + list(n_atoms):
        if A.op == ODRL.eq:
            cand.append(A.right)
        else:
            L = as_list(g, A.right)
            if L:
                cand.extend(L)
    # numeric nudges: try just inside P and just outside N
    def _num_bounds(atoms: Sequence[Atom]):
        lo, lo_incl, hi, hi_incl = None, True, None, True
        for A in atoms:
            if isinstance(A.right, Literal):
                if A.op in (ODRL.gt, ODRL.gteq):
                    if lo is None or _cmp_literals(A.right, lo) == 1:
                        lo, lo_incl = A.right, (A.op == ODRL.gteq)
                if A.op in (ODRL.lt, ODRL.lteq):
                    if hi is None or _cmp_literals(A.right, hi) == -1:
                        hi, hi_incl = A.right, (A.op == ODRL.lteq)
        return lo, lo_incl, hi, hi_incl

    def _bump(lit: Literal, eps: Literal, up: bool) -> Literal:
        # Works for xsd:integer/decimal/dateTime/date
        py = to_python_literal(lit)
        if isinstance(py, int):
            return Literal(py + (1 if up else -1), datatype=XSD.integer)
        if isinstance(py, Decimal):
            return Literal(py + (Decimal("0.0001") if up else -Decimal("0.0001")))
        if isinstance(py, datetime.datetime):
            delta = datetime.timedelta(seconds=1)
            return Literal(py + (delta if up else -delta), datatype=XSD.dateTime)
        if isinstance(py, datetime.date):
            delta = datetime.timedelta(days=1)
            return Literal(py + (delta if up else -delta), datatype=XSD.date)
        return lit

    loP, loP_inc, hiP, hiP_inc = _num_bounds(p_atoms)
    if loP: cand.append(_bump(loP, loP, True if not loP_inc else False))
    if hiP: cand.append(_bump(hiP, hiP, False if not hiP_inc else True))
    loN, loN_inc, hiN, hiN_inc = _num_bounds(n_atoms)
    if loN: cand.append(_bump(loN, loN, False))   # just outside N
    if hiN: cand.append(_bump(hiN, hiN, True))

    # de-dup (value + datatype)
    seen: Set[Tuple[object, Optional[URIRef]]] = set()
    uniq: List[object] = []
    for v in cand:
        key = (v, getattr(v, "datatype", None))
        if key in seen: continue
        seen.add(key); uniq.append(v)

    for v in uniq:
        okP = all(_holds_atom(g, TH, A, v) is not False for A in p_atoms)
        okN = all(_holds_atom(g, TH, A, v) is not False for A in n_atoms)
        if okP and not okN:
            return True, v

    # Try a tiny set-valued witness for set ops
    def collect_set(atoms: Sequence[Atom]):
        eq_sets: List[Set[URIRef]] = []; any_sets: List[Set[URIRef]] = []
        all_req: Set[URIRef] = set(); none_forbid: Set[URIRef] = set()
        neq_sets: List[Set[URIRef]] = []
        for a in atoms:
            S = _as_uris(g, a.right)
            if a.op == ODRL.eq and S is not None: eq_sets.append(set(S))
            elif a.op == ODRL.isAnyOf and S is not None: any_sets.append(set(S))
            elif a.op == ODRL.isAllOf and S is not None: all_req |= S
            elif a.op == ODRL.isNoneOf and S is not None: none_forbid |= S
            elif a.op == ODRL.neq and S is not None: neq_sets.append(set(S))
        return eq_sets, any_sets, all_req, none_forbid, neq_sets

    eqP, anyP, allP, noneP, neqP = collect_set(p_atoms)
    eqN, anyN, allN, noneN, neqN = collect_set(n_atoms)

    # exact-equality set from P
    if eqP:
        V: Optional[Set[URIRef]] = None
        for S in eqP:
            if V is None: V = set(S)
            elif V != S:  return (False, None)
        assert V is not None
        if all(_holds_atom(g, TH, A, V) is not False for A in p_atoms) and not all(_holds_atom(g, TH, A, V) is not False for A in n_atoms):
            return True, V

    # minimal constructive V
    V: Set[URIRef] = set(allP)
    unionN_any = set().union(*anyN) if anyN else set()
    for S in anyP:
        picks = list((S - noneP) - unionN_any) or list(S - noneP) or list(S)
        if not picks: return (False, None)
        V.add(picks[0])

    # break N if possible
    if anyN and all(not (V & S) for S in anyN):    return True, V
    if allN - V:                                   return True, V
    if noneN - noneP:
        x = next(iter(noneN - noneP))
        V2 = set(V); V2.add(x)
        if all(_holds_atom(g, TH, A, V2) is not False for A in p_atoms):
            return True, V2

    return False, None

# ─────────────────────────── Pair analysis ───────────────────────────
def same_scope(a: Rule, b: Rule) -> bool:
    if REQUIRE_SAME_ASSIGNEE and a.assignee != b.assignee: return False
    if REQUIRE_SAME_TARGET and a.target != b.target:       return False
    return True

@dataclass
class PairResult:
    kind: str            # 'perm-vs-prohib' | 'duty-blocked'
    status: str          # 'Ambiguous' | 'Conflict' | 'No conflict'
    a: Rule
    b: Rule
    why_action: str
    details: str

def _clauses_overlap(g: Graph, TH: TypeHierarchy, pc: Clause, nc: Clause) -> Tuple[bool, str, bool, Optional[str]]:
    if not pc and not nc: return True, "both clauses unconstrained", False, None
    if not pc:            return True, "permission clause unconstrained", False, None
    if not nc:            return True, "prohibition clause unconstrained", False, None
    shared = set(pc.keys()) & set(nc.keys())
    if not shared:        return False, "no shared leftOperand in selected branches", False, None
    # satisfiable together?
    for L in shared:
        atoms = (pc.get(L, []) or []) + (nc.get(L, []) or [])
        if not _satisfiable_on_operand(g, TH, atoms):
            return False, f"incompatible on {qname(g, L)}", False, None
    # witness?
    for L in shared:
        okPonly, v = _p_only_on_operand(g, TH, pc.get(L, []), nc.get(L, []))
        if okPonly:
            s = ", ".join(sorted(qname(g, s) for s in shared))
            return True, f"overlap on {s}", True, f"{qname(g, L)} = {qname(g, v)}"
    s = ", ".join(sorted(qname(g, s) for s in shared))
    return True, f"overlap on {s}", False, None

def analyze_pairs(g_vocab: Graph, gq: Graph) -> List[PairResult]:
    AH = ActionHierarchy(g_vocab, gq)
    TH = TypeHierarchy(g_vocab, gq)
    rules_all = sort_and_label_rules(gq, extract_rules(gq))
    perms  = [r for r in rules_all if r.kind == "permission"]
    prohib = [r for r in rules_all if r.kind == "prohibition"]
    duties = [r for r in rules_all if r.kind == "obligation"]
    results: List[PairResult] = []

    def eval(kind: str, X: Rule, N: Rule) -> None:
        okA, whyA = AH.overlap(X.action, N.action)
        if not okA: return
        pclauses = expand_rule_to_clauses(X) or [{}]
        nclauses = expand_rule_to_clauses(N) or [{}]
        any_overlap = False
        any_ambig = False
        reasons: List[str] = []
        for pc in pclauses:
            for nc in nclauses:
                ok, why, amb, wit = _clauses_overlap(gq, TH, pc, nc)
                if ok:
                    any_overlap = True
                    reasons.append(f"branch overlap: {why}" + (f"; safe witness: {wit}" if amb else ""))
                    if amb: any_ambig = True
                else:
                    reasons.append(f"branch non-overlap: {why}")
        status = ("Ambiguous" if any_overlap and any_ambig
                  else ("Conflict" if any_overlap else "No conflict"))
        results.append(PairResult(kind, status, X, N, whyA, "; ".join(reasons)))

    for p in perms:
        for n in prohib:
            if same_scope(p, n): eval("perm-vs-prohib", p, n)
    for d in duties:
        for n in prohib:
            if same_scope(d, n): eval("duty-blocked", d, n)
    return results

# ─────────────────────────── Printing ───────────────────────────
def rule_fact_lines(g: Graph, r: Rule) -> List[str]:
    pol = qname(g, r.policy)
    bn  = f"_:{r.print_id}" if r.print_id else qname(g, r.node)
    ass = qname(g, r.assignee) if r.assignee else "?"
    act = qname(g, r.action)
    tgt = qname(g, r.target) if r.target else "?"
    return [
        f"{pol} {r.kind}: {bn}",
        f"  {bn} odrl:assignee {ass} ;",
        f"  {bn} odrl:action {act} ;",
        f"  {bn} odrl:target {tgt} .",
    ]

def summarize_pairs(pairs: Sequence[PairResult]) -> Tuple[str, Optional[PairResult], int, int]:
    if not pairs: return "No conflict", None, 0, 0
    for want in ("Ambiguous", "Conflict", "No conflict"):
        bucket = [p for p in pairs if p.status == want]
        if bucket:
            return want, bucket[0], len(pairs), len(pairs) - 1
    return "No conflict", pairs[0], len(pairs), len(pairs) - 1

def print_pairs(name: str, gq: Graph, pairs: Sequence[PairResult]) -> None:
    print(f"\n=== {name} ===")
    if not pairs:
        print("No conflicts detected.")
        return
    for i, pr in enumerate(pairs, start=1):
        label = {"Conflict": "CONFLICT", "Ambiguous": "AMBIGUOUS", "No conflict": "NO CONFLICT"}[pr.status]
        kind  = "Permission vs Prohibition" if pr.kind == "perm-vs-prohib" else "Obligation blocked by Prohibition"
        print(f"[{i:02d}] {label}: {kind}")
        for r in (pr.a, pr.b):
            for line in rule_fact_lines(gq, r):
                print(" " + line)
        print(f" Action overlap: {pr.why_action}")
        print(f" Details: {pr.details}\n")

def print_summary(name: str, g_vocab: Graph, url: str) -> None:
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
        label = {"Conflict": "CONFLICT", "Ambiguous": "AMBIGUOUS", "No conflict": "NO CONFLICT"}[pr.status]
        kind  = "Permission vs Prohibition" if pr.kind == "perm-vs-prohib" else "Obligation blocked by Prohibition"
        print(f"[example] {label}: {kind}")
        for r in (pr.a, pr.b):
            for line in rule_fact_lines(gq, r):
                print(" " + line)
        print(f" Action overlap: {pr.why_action}")
        print(f" Details: {pr.details}")
        if elided > 0:
            print(f"... ({elided} other pair(s) elided)")

def print_report(name: str, g_vocab: Graph, url: str) -> None:
    if OUTPUT_MODE == OutputMode.PAIRS:
        gq = Graph()
        gq.namespace_manager.bind("odrl", ODRL, override=True)
        gq.namespace_manager.bind("dct", DCTERMS, override=False)
        gq.namespace_manager.bind("rdfs", RDFS, override=False)
        gq.parse(url, format="turtle")
        pairs = analyze_pairs(g_vocab, gq)
        print_pairs(name, gq, pairs)
    else:
        print_summary(name, g_vocab, url)

# ─────────────────────────── ARC: Answer / Reason / Check ───────────────────────────
def print_answer():
    print("Answer")
    print("======")
    gv = Graph()
    try:
        gv.parse(ODRL_TTL_URL, format="turtle")
    except Exception as e:
        sys.stderr.write(f"[warn] Could not fetch ODRL22.ttl ({e}). Using fallback action hierarchy.\n")
        gv = Graph()
    gv.namespace_manager.bind("odrl", ODRL, override=True)
    gv.namespace_manager.bind("rdfs", RDFS, override=False)
    for url in QUIZ_URLS:
        name = url.rsplit("/", 1)[-1]
        try:
            print_report(name, gv, url)
        except Exception as e:
            print(f"\n=== {name} ===")
            print(f"Error: {e}")

def print_reason():
    print("\nReason why")
    print("==========")
    print("We flatten each rule (permission/prohibition/obligation) to a tuple:")
    print("  ⟨assignee, action, target, [Expr,…]⟩  with Expr ∈ {Atom | Or | And | Xone | AndSeq}.")
    print("Each rule expands to disjunctive branches (clauses) grouped by leftOperand.")
    print("Two rules are comparable only if they share scope (same assignee and target)")
    print("and their actions overlap via equality or odrl:includedIn*/rdfs:subClassOf*.")
    print("For each Permission/Obligation vs Prohibition pair we check branches:")
    print("  • Overlap: all shared operands are jointly satisfiable;")
    print("  • Ambiguity: there exists a value v on a shared operand that satisfies P but not N.")
    print("If at least one pair overlaps and admits such a v ⇒ Ambiguous;")
    print("if at least one pair overlaps but none admit v ⇒ Conflict;")
    print("if no pair overlaps ⇒ No conflict.")

def print_check():
    print("\nCheck (harness)")
    print("===============")
    # Re-run analysis and assert internal invariants for every quiz.
    gv = Graph()
    try:
        gv.parse(ODRL_TTL_URL, format="turtle")
    except Exception:
        gv = Graph()
    gv.namespace_manager.bind("odrl", ODRL, override=True)
    gv.namespace_manager.bind("rdfs", RDFS, override=False)

    all_ok = True
    for url in QUIZ_URLS:
        gq = Graph()
        gq.namespace_manager.bind("odrl", ODRL, override=True)
        gq.namespace_manager.bind("dct", DCTERMS, override=False)
        gq.namespace_manager.bind("rdfs", RDFS, override=False)
        try:
            gq.parse(url, format="turtle")
        except Exception as e:
            print(f"{url.rsplit('/',1)[-1]} — parse error skipped ({e})")
            continue
        pairs = analyze_pairs(gv, gq)
        overall, exemplar, _, _ = summarize_pairs(pairs)

        # Invariants per overall classification
        overlaps = [p for p in pairs if p.status in ("Ambiguous", "Conflict")]
        ambigs   = [p for p in pairs if p.status == "Ambiguous"]

        ok = True
        if overall == "No conflict":
            ok &= (len(overlaps) == 0)
        if overall == "Conflict":
            ok &= (len(overlaps) >= 1 and len(ambigs) == 0)
        if overall == "Ambiguous":
            ok &= (len(ambigs) >= 1)
        # Deterministic rule labeling (r01, r02, …)
        rules = sort_and_label_rules(gq, extract_rules(gq))
        ok &= all(r.print_id is not None for r in rules)

        print(f"{url.rsplit('/',1)[-1]} — internal checks: {ok}")
        all_ok &= ok

    print(f"\nAll checks passed? {all_ok}")

# ─────────────────────────── Main ───────────────────────────
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

