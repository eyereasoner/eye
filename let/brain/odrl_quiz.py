#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
===============================================================================
ODRL QUIZ SOLVER — DETERMINISTIC, SPEC-ALIGNED, AND EXPLAINABLE
===============================================================================

PURPOSE
-------
This script reads "ODRL quiz" RDF/Turtle files and determines whether the rules
inside them are in **Conflict**, **Ambiguous**, or **No conflict** — exactly as
intended by the ODRL 2.2 specification. The results are printed in a proof-style
format similar to the EYE reasoner: each case shows the involved rules, why their
actions overlap, and how their constraints interact.

The tool is built to be:
- **Deterministic**: same inputs → same sorted, labeled, human-readable output.
- **Spec-aligned**: operators and semantics match ODRL 2.2 (and quiz assumptions).
- **Explainable**: each verdict is justified with branch-by-branch reasoning.

-------------------------------------------------------------------------------
WHAT IT ANALYZES
-------------------------------------------------------------------------------
- **Policy kinds**: Permission, Prohibition (also `odrl:prohibited`), Obligation.
- **Logical constructors**: `odrl:and`, `odrl:andSequence`, `odrl:or`, `odrl:xone`.
- **Constraint operators** (ODRL core):
    Equality / inequality:       `odrl:eq`, `odrl:neq`
    Order comparisons (literals):`odrl:lt`, `odrl:lteq`, `odrl:gt`, `odrl:gteq`
    Set membership:               `odrl:isAnyOf`, `odrl:isAllOf`, `odrl:isNoneOf`
    Class membership:              `odrl:isA` (with rdfs:subClassOf* closure)
- **Constraint domains**:
    1. Numeric / temporal (literals like numbers, xsd:date, xsd:dateTime)
    2. Set-valued (true mathematical sets of URIs)
    3. Single-valued URI (with special handling for isAllOf/isAnyOf semantics)
- **Action overlap detection**:
    - Same action → overlap
    - Or via action hierarchy: reflexive-transitive closure over `odrl:includedIn`
      and `rdfs:subClassOf`. If missing, uses a quiz-specific fallback hierarchy.

-------------------------------------------------------------------------------
HOW IT REASONS
-------------------------------------------------------------------------------
1. **Parse**:
   - Read all policies in the quiz file.
   - Flatten into `Rule` objects: each has kind, assignee, action, target, and
     a list of constraint/logical expressions.

2. **Normalize logic**:
   - Expand nested `odrl:and`, `odrl:or`, etc. into **disjunctive normal form**
     over *operands*, producing "clauses" (operand → list of Atoms).
   - Each clause is a branch of possible values.

3. **Compare rules**:
   - Only compare if scope matches (assignee/target) and actions overlap.
   - Compare:
       * Permission ↔ Prohibition
       * Obligation ↔ Prohibition
   - For each branch pair, check:
       * If constraints on shared operands are **satisfiable together**.
       * If so, check for a "P-only witness" value that satisfies the
         permission/obligation but not the prohibition.

4. **Classify outcome**:
   - If no branch pair overlaps → **No conflict**
   - If at least one overlaps and any has a P-only witness → **Ambiguous**
   - If at least one overlaps and none have a P-only witness → **Conflict**

-------------------------------------------------------------------------------
OUTPUT
-------------------------------------------------------------------------------
- **summary** mode (default):
    Prints one representative pair per quiz file plus the overall result.
- **pairs** mode:
    Prints all analyzed pairs and their branch overlap reasoning.

Format per pair:
    [label] {Permission vs Prohibition | Obligation blocked by Prohibition}
      <rule facts with stable IDs>
      Action overlap: <explanation>
      Details: <branch-by-branch reasoning, safe witnesses>

-------------------------------------------------------------------------------
DETERMINISTIC PRINTING
-------------------------------------------------------------------------------
- Rules are sorted by stable keys: policy name, kind, assignee, action, target,
  and normalized expression signatures.
- Each rule gets a `_:` blank node ID like `_:r01`, `_:r02` for reproducibility.

-------------------------------------------------------------------------------
VOCABULARY HANDLING
-------------------------------------------------------------------------------
- Tries to fetch ODRL 2.2 vocabulary (ODRL22.ttl) to get the action hierarchy.
- If unavailable, uses a minimal fallback mapping (`read ⊑ use`, etc.) so
  quizzes still behave as intended.

-------------------------------------------------------------------------------
IMPLEMENTATION NOTES
-------------------------------------------------------------------------------
- Written with `rdflib` for RDF parsing and namespace management.
- Refactored to use Python dataclasses, enums, and type hints for clarity.
- Many helpers for safe RDF list handling, literal comparison, and domain-aware
  satisfiability checking.
- Debug mode (`DEBUG = True`) prints internal operand reasoning to stderr.

-------------------------------------------------------------------------------
USAGE
-------------------------------------------------------------------------------
    $ python3 odrl_quiz_refactor.py
      (will download vocabulary if possible and process all QUIZ_URLS)

Adjust:
    - QUIZ_URLS: list of quiz file URLs
    - OUTPUT_MODE: 'summary' or 'pairs'
    - REQUIRE_SAME_ASSIGNEE / REQUIRE_SAME_TARGET: scope matching rules
===============================================================================
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

# ---------------------------------------------------------------------
# CONFIG
# ---------------------------------------------------------------------

ODRL = Namespace("http://www.w3.org/ns/odrl/2/")
ODRL_TTL_URL = "https://www.w3.org/ns/odrl/2/ODRL22.ttl"

# Logical props (use bracket notation for rdflib safety)
ODRL_OR = ODRL["or"]
ODRL_AND = ODRL["and"]
ODRL_XONE = ODRL["xone"]
ODRL_ANDSEQUENCE = ODRL["andSequence"]

# Quizzes to run (00-*, 01-*, 02-*)
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

# Output mode: one exemplar per file (set to OutputMode.PAIRS for full list)
OUTPUT_MODE: OutputMode = OutputMode.SUMMARY

# Require same assignee/target (scope)
REQUIRE_SAME_ASSIGNEE = True
REQUIRE_SAME_TARGET = True

# Toggle debug prints inside satisfiability (stderr)
DEBUG = False

# ---------------------------------------------------------------------
# Utils
# ---------------------------------------------------------------------

QNameish = Union[str, URIRef, Literal, BNode, None]


def qname(g: Graph, term: QNameish) -> str:
    """Pretty QName/URI/literal for proof output."""
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
    """Convert rdflib Literal to a native Python value when possible."""
    try:
        return lit.toPython()
    except Exception:
        return lit


# Strict RDF collection detection to avoid mis-parsing

def as_list(g: Graph, node: QNameish) -> Optional[List]:
    """
    Strictly detect RDF Collections:
      • If node == rdf:nil → return []
      • If there is (node, rdf:first, _) → return the list
      • Otherwise → return None (node is NOT a list)
    """
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
    """Reflexive-transitive closure of a directed graph given as adjacency sets."""
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
            for y in edges.get(x, ()):  # type: ignore[arg-type]
                if y not in seen:
                    stack.append(y)
        seen.add(a)  # reflexive
        closure[a] = seen
    return closure


# ---------------------------------------------------------------------
# Hierarchies (actions and classes)
# ---------------------------------------------------------------------

class ActionHierarchy:
    """
    Encapsulates action inclusion reasoning.
    If ODRL22.ttl (or the quiz file) contains `odrl:includedIn` edges, we
    build the closure over them (+ any `rdfs:subClassOf`). Otherwise, we
    activate a minimal fallback to simulate common-sense inclusions used by
    the quizzes (e.g., `read ⊑ use`, `distribute ⊑ use`, ...).
    """

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

        def add(s: QNameish, o: QNameish) -> None:
            if isinstance(s, URIRef) and isinstance(o, URIRef):
                edges.setdefault(s, set()).add(o)

        has_included = False
        # includedIn from vocab and from the quiz itself
        for s, _, o in g_vocab.triples((None, ODRL.includedIn, None)):
            add(s, o)
            has_included = True
        for s, _, o in g_quiz.triples((None, ODRL.includedIn, None)):
            add(s, o)
            has_included = True
        # subclasses also imply inclusion
        for s, _, o in g_quiz.triples((None, RDFS.subClassOf, None)):
            add(s, o)
        # fallback if vocabulary isn't available
        if not has_included:
            for s, o in self.FALLBACK_EDGES:
                add(s, o)
        self.closure = rt_closure(edges)

    def overlap(self, a: URIRef, b: URIRef) -> Tuple[bool, str]:
        if a == b:
            return True, f"same action ({a})"
        a_sup = self.closure.get(a, {a})
        b_sup = self.closure.get(b, {b})
        if b in a_sup:
            return True, f"{a} ⊑ {b} via includedIn/subClassOf*"
        if a in b_sup:
            return True, f"{b} ⊑ {a} via includedIn/subClassOf*"
        return False, "no action inclusion relation"


class TypeHierarchy:
    """Simple `rdfs:subClassOf*` instance checking for `odrl:isA`."""

    def __init__(self, g_vocab: Graph, g_quiz: Graph):
        edges: Dict[URIRef, Set[URIRef]] = {}

        def add(s: QNameish, o: QNameish) -> None:
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


# ---------------------------------------------------------------------
# Model & parsing
# ---------------------------------------------------------------------

@dataclass(frozen=True)
class Atom:
    """A single atomic constraint: leftOperand OP rightOperand."""

    left: URIRef
    op: URIRef
    right: Union[Literal, URIRef, BNode]


class Expr:
    pass


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
    """
    A flattened rule: kind + (assignee, action, target) + a list of expressions
    attached either at the rule level (grouping) or as child constraints.
    """

    kind: str
    policy: URIRef
    node: Union[URIRef, BNode]
    assignee: Optional[URIRef]
    action: URIRef
    target: Optional[URIRef]
    exprs: List[Expr]
    print_id: Optional[str] = None


def _parse_expr(g: Graph, cnode: Union[URIRef, BNode]) -> Optional[Expr]:
    """Parse an `odrl:constraint` node or a nested logical expression node."""
    left = next(g.objects(cnode, ODRL.leftOperand), None)
    op = next(g.objects(cnode, ODRL.operator), None)

    # accept rightOperand OR rightOperandReference
    right = next(g.objects(cnode, ODRL.rightOperand), None)
    if right is None:
        right = next(g.objects(cnode, ODRL.rightOperandReference), None)

    if isinstance(left, URIRef) and isinstance(op, URIRef) and right is not None:
        return Atom(left, op, right)  # type: ignore[arg-type]

    # Otherwise: it's a logical node (or/and/xone/andSequence)
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

    for prop, Ctor in (
        (ODRL_OR, Or),
        (ODRL_AND, And),
        (ODRL_XONE, Xone),
        (ODRL_ANDSEQUENCE, AndSeq),
    ):
        node = parse_children(prop, Ctor)
        if node is not None:
            return node

    return None


def _parse_rule_exprs(g: Graph, rnode: Union[URIRef, BNode]) -> List[Expr]:
    """
    Collect top-level constraints and top-level logical expressions under a
    rule node into a list of expressions; empty And([]) marks "present but empty".
    """
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

    for prop, Ctor in (
        (ODRL_OR, Or),
        (ODRL_AND, And),
        (ODRL_XONE, Xone),
        (ODRL_ANDSEQUENCE, AndSeq),
    ):
        collect(prop, Ctor)

    if not exprs and saw_any:
        exprs.append(And(tuple()))
    return exprs


def extract_rules(g: Graph) -> List[Rule]:
    """
    Turn a policy set into a flat list of Rules (one per assignee×action×target).
    Each rule carries its expressions (constraints/logical forms).
    """
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
                actions = [a for a in g.objects(rnode, ODRL.action) if isinstance(a, URIRef)]
                targets = list(g.objects(rnode, ODRL.target)) or [None]
                exprs = _parse_rule_exprs(g, rnode)
                for a in assignees:
                    for act in actions:
                        for t in targets:
                            out.append(
                                Rule(kind, pol, rnode, a if isinstance(a, URIRef) else None, act, t if isinstance(t, URIRef) else None, exprs)
                            )
    return out


# ---------------------------------------------------------------------
# Deterministic ordering (stable ids in printouts)
# ---------------------------------------------------------------------


def _expr_signature(g: Graph, e: Expr) -> str:
    """Produce a stable textual signature to drive sorting of rules."""
    if isinstance(e, Atom):
        lst = as_list(g, e.right)
        rv = (
            "[" + ",".join(qname(g, x) for x in lst) + "]" if lst is not None else qname(g, e.right)
        )
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
    """Stable sort of rules + assign printable ids _:r01, _:r02, …"""
    out = sorted(rules, key=lambda r: _rule_sort_key(g, r))
    for i, r in enumerate(out, start=1):
        r.print_id = f"r{i:02d}"
    return out


# ---------------------------------------------------------------------
# Clause expansion (logical normal form over operands)
# ---------------------------------------------------------------------

Clause = Dict[URIRef, List[Atom]]  # leftOperand -> list of Atoms


def _merge_and(c1: Clause, c2: Clause) -> Clause:
    r: Clause = {k: v[:] for k, v in c1.items()}
    for L, atoms in c2.items():
        r.setdefault(L, []).extend(atoms)
    return r


def _expand_expr(e: Expr) -> List[Clause]:
    """Expand one Expr into a list of operand->atoms maps (conjunctive clauses)."""
    if isinstance(e, Atom):
        return [{e.left: [e]}]
    if isinstance(e, (And, AndSeq)):
        if not e.kids:  # explicit but empty And → single empty clause
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
    """Expand the rule's expressions; each result is a conjunctive clause."""
    clauses: List[Clause] = [{}]
    for e in r.exprs:
        parts = _expand_expr(e)
        new_clauses: List[Clause] = []
        for c in clauses:
            for p in parts:
                new_clauses.append(_merge_and(c, p))
        clauses = new_clauses
    return clauses


# ---------------------------------------------------------------------
# Operator semantics (per operand — domain-sensitive)
# ---------------------------------------------------------------------


def _cmp_literals(a: Literal, b: Literal) -> Optional[int]:
    """3-way compare for rdflib Literals of the *same* Python type, else None."""
    try:
        pa, pb = to_python_literal(a), to_python_literal(b)
        if type(pa) is type(pb):  # noqa: E721 (intentional strict type equality)
            return -1 if pa < pb else (1 if pa > pb else 0)
    except Exception:
        pass
    return None


def _domain_for_operand(g: Graph, atoms: Sequence[Atom]) -> str:
    """
    Pick reasoning domain for one operand from its atoms:
      - 'numeric' if any rightOperand is a Literal (date/number)
      - 'set'     if any operator is one of {isAnyOf, isAllOf, isNoneOf}
      - 'uri'     otherwise (single-valued URI)
    """
    if any(isinstance(a.right, Literal) for a in atoms):
        return "numeric"
    if any(a.op in (ODRL.isAnyOf, ODRL.isAllOf, ODRL.isNoneOf) for a in atoms):
        return "set"
    return "uri"


def _as_uris(g: Graph, term: Union[Literal, URIRef, BNode]) -> Optional[Set[URIRef]]:
    """Normalize a rightOperand to a set of URIRefs (from IRI or RDF list)."""
    L = as_list(g, term)
    if L is not None:
        return {x for x in L if isinstance(x, URIRef)}
    if isinstance(term, URIRef):
        return {term}
    return None


def _satisfiable_on_operand(g: Graph, TH: TypeHierarchy, atoms: Sequence[Atom]) -> bool:
    """
    Check if there exists *some* value for this operand that satisfies *all*
    atoms in the list, under the selected domain semantics.
    """
    if not atoms:
        return True

    dom = _domain_for_operand(g, atoms)

    # ---------- NUMERIC/TEMPORAL ----------
    if dom == "numeric":
        eqs = [a.right for a in atoms if a.op == ODRL.eq and isinstance(a.right, Literal)]
        neqs = [a.right for a in atoms if a.op == ODRL.neq and isinstance(a.right, Literal)]
        rng = [a for a in atoms if a.op in (ODRL.lt, ODRL.lteq, ODRL.gt, ODRL.gteq) and isinstance(a.right, Literal)]

        if eqs:
            v = eqs[0]
            for e in eqs[1:]:
                if _cmp_literals(v, e) != 0:
                    return False
            for n in neqs:
                if _cmp_literals(v, n) == 0:
                    return False
            for a in rng:
                sign = _cmp_literals(v, a.right)
                if sign is None:
                    return False
                if a.op == ODRL.lt and not (sign < 0):
                    return False
                if a.op == ODRL.lteq and not (sign <= 0):
                    return False
                if a.op == ODRL.gt and not (sign > 0):
                    return False
                if a.op == ODRL.gteq and not (sign >= 0):
                    return False
            return True

        lo_v = hi_v = None
        lo_inc = hi_inc = False
        for a in rng:
            if a.op in (ODRL.gt, ODRL.gteq):
                if lo_v is None or to_python_literal(a.right) > to_python_literal(lo_v):
                    lo_v, lo_inc = a.right, (a.op == ODRL.gteq)
            if a.op in (ODRL.lt, ODRL.lteq):
                if hi_v is None or to_python_literal(a.right) < to_python_literal(hi_v):
                    hi_v, hi_inc = a.right, (a.op == ODRL.lteq)
        if lo_v is not None and hi_v is not None:
            lo, hi = to_python_literal(lo_v), to_python_literal(hi_v)
            if lo > hi:
                return False
            if lo == hi and not (lo_inc and hi_inc):
                return False
        return True

    # ---------- SET (multi-valued) ----------
    if dom == "set":
        eq_sets: List[Set[URIRef]] = []
        any_sets: List[Set[URIRef]] = []
        all_required: Set[URIRef] = set()
        none_forbid: Set[URIRef] = set()
        isa_classes: List[URIRef] = []
        neq_sets: List[Set[URIRef]] = []

        for a in atoms:
            if a.op == ODRL.isA and isinstance(a.right, URIRef):
                isa_classes.append(a.right)
                continue
            S = _as_uris(g, a.right)
            if S is None:
                return False
            if a.op == ODRL.eq:
                eq_sets.append(set(S))
            elif a.op == ODRL.isAllOf:
                all_required |= S
            elif a.op == ODRL.isAnyOf:
                any_sets.append(set(S))
            elif a.op == ODRL.isNoneOf:
                none_forbid |= S
            elif a.op == ODRL.neq:
                neq_sets.append(set(S))
            else:
                return False

        if eq_sets:
            V: Optional[Set[URIRef]] = None
            for S in eq_sets:
                if V is None:
                    V = set(S)
                elif V != S:
                    return False
            assert V is not None
            if not all_required <= V:
                return False
            if V & none_forbid:
                return False
            for S in any_sets:
                if not (V & S):
                    return False
            for S in neq_sets:
                if V == S:
                    return False
            for C in isa_classes:
                if not all(TypeHierarchy.is_instance_of.__get__(object(), TypeHierarchy)(g, x, C) for x in V):
                    return False
            return True

        if all_required & none_forbid:
            return False
        for S in any_sets:
            if not (S - none_forbid):
                return False
        return True

    # ---------- URI (single-valued) ----------
    eq_vals: List[URIRef] = []
    neq_vals: Set[URIRef] = set()
    any_sets2: List[Set[URIRef]] = []
    all_sets: List[Set[URIRef]] = []
    none_sets: List[Set[URIRef]] = []
    isa_classes2: List[URIRef] = []

    for a in atoms:
        if a.op == ODRL.isA and isinstance(a.right, URIRef):
            isa_classes2.append(a.right)
            continue
        S = _as_uris(g, a.right)
        if S is None:
            return False
        if a.op == ODRL.eq:
            if len(S) != 1:
                return False
            eq_vals.append(next(iter(S)))
        elif a.op == ODRL.neq:
            if len(S) != 1:
                return False
            neq_vals.add(next(iter(S)))
        elif a.op == ODRL.isAnyOf:
            any_sets2.append(set(S))
        elif a.op == ODRL.isNoneOf:
            none_sets.append(set(S))
        elif a.op == ODRL.isAllOf:
            all_sets.append(set(S))
        else:
            return False

    if eq_vals:
        v = eq_vals[0]
        if any(x != v for x in eq_vals):
            return False
        if v in neq_vals:
            return False
        for S in any_sets2:
            if v not in S:
                return False
        for S in none_sets:
            if v in S:
                return False
        for S in all_sets:
            if not (len(S) == 1 and v in S):
                return False
        for C in isa_classes2:
            TH_inst = TypeHierarchy.is_instance_of.__get__(TH, TypeHierarchy)
            if not TH_inst(g, v, C):
                return False
        return True

    # No equality: try to *choose* a value
    if any(len(S) > 1 for S in all_sets):
        # Single-valued operand cannot satisfy a multi-element isAllOf
        return False
    fixed: Optional[URIRef] = None
    if all_sets:
        S = next(iter(all_sets))
        if len(S) == 1:
            fixed = next(iter(S))
        else:
            return False
    candidates: List[URIRef] = []
    if fixed:
        candidates.append(fixed)
    for S in any_sets2:
        for x in S:
            if x not in candidates:
                candidates.append(x)
    for v in candidates or [None]:
        if v is None:  # open-world fallback (choose a fresh value)
            return True
        if v in neq_vals:
            continue
        if any(v in S for S in none_sets):
            continue
        return True
    return True


# ---------------------------------------------------------------------
# Ambiguity witness search (per operand)
# ---------------------------------------------------------------------


def _step_like(right: Literal, direction: str) -> Optional[Literal]:
    """
    Tiny helper for date/number candidates: step just inside/outside a bound
    to construct values like "gteq X" → try X; "gt X" → try X+ε; etc.
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


def _holds_atom(g: Graph, TH: TypeHierarchy, a: Atom, v: object) -> Optional[bool]:
    """
    Evaluate one atom on a *candidate* value v.
    Supports: set-valued candidates (Python set of URIRefs), single URI, literal.
    """
    op, right = a.op, a.right
    lst = as_list(g, right)

    # ---- Set-valued candidate (URIs) ----
    if isinstance(v, (set, frozenset)) and all(isinstance(x, URIRef) for x in v):
        S = (
            {x for x in (lst or []) if isinstance(x, URIRef)}
            if lst is not None
            else ({right} if isinstance(right, URIRef) else None)
        )
        if S is None:
            return None
        if op == ODRL.eq:
            return set(v) == S
        if op == ODRL.neq:
            return set(v) != S
        if op == ODRL.isAllOf:
            return S <= set(v)
        if op == ODRL.isAnyOf:
            return bool(set(v) & S)
        if op == ODRL.isNoneOf:
            return not (set(v) & S)
        if op == ODRL.isA:
            return all(TH.is_instance_of(g, x, right) for x in v if isinstance(x, URIRef))
        return None

    # ---- Single-URI candidate ----
    if isinstance(v, URIRef):
        if op == ODRL.eq and isinstance(right, URIRef):
            return v == right
        if op == ODRL.neq and isinstance(right, URIRef):
            return v != right
        if op == ODRL.isA and isinstance(right, URIRef):
            return TH.is_instance_of(g, v, right)
        if lst is not None:
            if op == ODRL.isAnyOf:
                return v in lst
            if op == ODRL.isNoneOf:
                return v not in lst
            if op == ODRL.isAllOf:
                return len(lst) == 1 and v == lst[0]

    # ---- Numeric/date literal candidate ----
    if isinstance(v, Literal) and isinstance(right, Literal):
        sign = _cmp_literals(v, right)
        if sign is None:
            return None
        if op == ODRL.eq:
            return sign == 0
        if op == ODRL.neq:
            return sign != 0
        if op == ODRL.lt:
            return sign < 0
        if op == ODRL.lteq:
            return sign <= 0
        if op == ODRL.gt:
            return sign > 0
        if op == ODRL.gteq:
            return sign >= 0

    return None


def _candidates_from_bounds_P(g: Graph, p_atoms: Sequence[Atom]) -> List[Literal]:
    """Candidate literals that are *inside* P's range (or equal)."""
    out: List[Literal] = []
    for a in p_atoms:
        if not isinstance(a.right, Literal):
            continue
        if a.op in (ODRL.gteq, ODRL.lteq, ODRL.eq):
            out.append(a.right)
        elif a.op == ODRL.gt:
            s = _step_like(a.right, "up")
            if s:
                out.append(s)
        elif a.op == ODRL.lt:
            s = _step_like(a.right, "down")
            if s:
                out.append(s)
    return [x for x in out if x is not None]


def _candidates_outside_bounds_N(g: Graph, n_atoms: Sequence[Atom]) -> List[Literal]:
    """Candidate literals that are *just outside* N's range (to make N fail)."""
    out: List[Literal] = []
    for a in n_atoms:
        if not isinstance(a.right, Literal):
            continue
        if a.op == ODRL.lteq:
            s = _step_like(a.right, "up")
            if s:
                out.append(s)
        elif a.op == ODRL.lt:
            out.append(a.right)
        elif a.op == ODRL.gteq:
            s = _step_like(a.right, "down")
            if s:
                out.append(s)
        elif a.op == ODRL.gt:
            out.append(a.right)
    return [x for x in out if x is not None]


def _p_only_on_operand(
    g: Graph, TH: TypeHierarchy, p_atoms: Sequence[Atom], n_atoms: Sequence[Atom]
) -> Tuple[bool, Optional[object]]:
    """
    Try to build a **P-only** witness: a value that satisfies the permission/obligation
    atoms but not the prohibition atoms, for this operand only.
    Used to decide Ambiguity when branches otherwise overlap.
    """
    # First try literals/URIs harvested from eq/items/bounds
    cand: List[object] = []
    for A in list(p_atoms) + list(n_atoms):
        if A.op == ODRL.eq:
            cand.append(A.right)
        else:
            L = as_list(g, A.right)
            if L:
                cand.extend(L)
    cand.extend(_candidates_from_bounds_P(g, p_atoms))
    cand.extend(_candidates_outside_bounds_N(g, n_atoms))
    seen: Set[Tuple[object, Optional[URIRef]]] = set()
    uniq: List[object] = []
    for v in cand:
        key = (v, getattr(v, "datatype", None))
        if key in seen:
            continue
        seen.add(key)
        uniq.append(v)
    for v in uniq:
        okP = all(_holds_atom(g, TH, A, v) is not False for A in p_atoms)
        okN = all(_holds_atom(g, TH, A, v) is not False for A in n_atoms)
        if okP and not okN:
            return True, v

    # Then try a small set-valued witness for set ops
    def collect_set(atoms: Sequence[Atom]):
        eq_sets: List[Set[URIRef]] = []
        any_sets: List[Set[URIRef]] = []
        all_req: Set[URIRef] = set()
        none_forbid: Set[URIRef] = set()
        isa: List[URIRef] = []
        neq_sets: List[Set[URIRef]] = []
        for a in atoms:
            if a.op == ODRL.isA and isinstance(a.right, URIRef):
                isa.append(a.right)
                continue
            S = _as_uris(g, a.right)
            if S is None:
                return None
            if a.op == ODRL.eq:
                eq_sets.append(set(S))
            elif a.op == ODRL.isAnyOf:
                any_sets.append(set(S))
            elif a.op == ODRL.isAllOf:
                all_req |= S
            elif a.op == ODRL.isNoneOf:
                none_forbid |= S
            elif a.op == ODRL.neq:
                neq_sets.append(set(S))
        return eq_sets, any_sets, all_req, none_forbid, isa, neq_sets

    P = collect_set(p_atoms)
    N = collect_set(n_atoms)
    if P is None or N is None:
        return False, None
    eqP, anyP, allP, noneP, isaP, neqP = P
    eqN, anyN, allN, noneN, isaN, neqN = N

    # Try exact equality from P
    if eqP:
        V: Optional[Set[URIRef]] = None
        for S in eqP:
            if V is None:
                V = set(S)
            elif V != S:
                return False, None
        assert V is not None
        if (
            all(_holds_atom(g, TH, A, V) is not False for A in p_atoms)
            and not all(_holds_atom(g, TH, A, V) is not False for A in n_atoms)
        ):
            return True, V

    # Minimal constructive V
    V = set(allP)
    unionN_any = set().union(*anyN) if anyN else set()
    for S in anyP:
        picks = list((S - noneP) - unionN_any) or list(S - noneP) or list(S)
        if not picks:
            return False, None
        V.add(picks[0])
    # Break N if possible:
    if anyN:
        for S in anyN:
            if not (V & S):
                return True, V
    if allN - V:
        return True, V
    if noneN - noneP:
        x = next(iter(noneN - noneP))
        V2 = set(V)
        V2.add(x)
        if all(_holds_atom(g, TH, A, V2) is not False for A in p_atoms):
            return True, V2
    return False, None


# ---------------------------------------------------------------------
# Clause overlap & pair analysis
# ---------------------------------------------------------------------


def same_scope(a: Rule, b: Rule) -> bool:
    """Require same assignee and target (as configured) to compare rules."""
    if REQUIRE_SAME_ASSIGNEE and a.assignee != b.assignee:
        return False
    if REQUIRE_SAME_TARGET and a.target != b.target:
        return False
    return True


@dataclass
class PairResult:
    kind: str
    status: str
    a: Rule
    b: Rule
    why_action: str
    details: str


def _clauses_overlap(
    g: Graph, TH: TypeHierarchy, pc: Clause, nc: Clause
) -> Tuple[bool, str, bool, Optional[str]]:
    """
    Decide if two *clauses* (expanded branch selections) overlap.
    Returns: (overlap?, explanation, ambiguous?, optional_witness_str)
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

    # Satisfiability on all shared operands
    for L in shared:
        atoms = (pc.get(L, []) or []) + (nc.get(L, []) or [])
        if not _satisfiable_on_operand(g, TH, atoms):
            return False, f"incompatible on {qname(g, L)}", False, None

    # Try to witness P-only on some shared operand for Ambiguity
    for L in shared:
        okPonly, v = _p_only_on_operand(g, TH, pc.get(L, []), nc.get(L, []))
        if okPonly:
            s = ", ".join(sorted(qname(g, s) for s in shared))
            return True, f"overlap on {s}", True, f"{qname(g, L)} = {qname(g, v)}"

    s = ", ".join(sorted(qname(g, s) for s in shared))
    return True, f"overlap on {s}", False, None


def analyze_pairs(g_vocab: Graph, gq: Graph) -> List[PairResult]:
    """
    Compute pairwise interactions: (permission vs prohibition) and
    (obligation blocked by prohibition) under same scope & overlapping actions.
    """
    AH = ActionHierarchy(g_vocab, gq)
    TH = TypeHierarchy(g_vocab, gq)
    rules_all = sort_and_label_rules(gq, extract_rules(gq))

    perms = [r for r in rules_all if r.kind == "permission"]
    prohib = [r for r in rules_all if r.kind == "prohibition"]
    duties = [r for r in rules_all if r.kind == "obligation"]

    results: List[PairResult] = []

    def eval(kind: str, X: Rule, N: Rule) -> None:
        okA, whyA = AH.overlap(X.action, N.action)
        if not okA:
            return

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
                    reasons.append(
                        f"branch overlap: {why}" + (f"; safe witness: {wit}" if amb else "")
                    )
                    if amb:
                        any_ambig = True
                else:
                    reasons.append(f"branch non-overlap: {why}")

        status = (
            "Ambiguous" if any_overlap and any_ambig else ("Conflict" if any_overlap else "No conflict")
        )
        results.append(PairResult(kind, status, X, N, whyA, "; ".join(reasons)))

    for p in perms:
        for n in prohib:
            if same_scope(p, n):
                eval("perm-vs-prohib", p, n)

    for d in duties:
        for n in prohib:
            if same_scope(d, n):
                eval("duty-blocked", d, n)

    return results


# ---------------------------------------------------------------------
# Printing
# ---------------------------------------------------------------------


def rule_fact_lines(g: Graph, r: Rule) -> List[str]:
    """Print the essential triples of a rule in a compact, stable way."""
    pol = qname(g, r.policy)
    bn = f"_:{r.print_id}" if r.print_id else qname(g, r.node)
    ass = qname(g, r.assignee) if r.assignee else "?"
    act = qname(g, r.action)
    tgt = qname(g, r.target) if r.target else "?"
    return [
        f"{pol} {r.kind}: {bn}",
        f"  {bn} odrl:assignee {ass} ;",
        f"  {bn} odrl:action  {act} ;",
        f"  {bn} odrl:target  {tgt} .",
    ]


def summarize_pairs(pairs: Sequence[PairResult]) -> Tuple[str, Optional[PairResult], int, int]:
    """
    Choose one representative pair to print:
      Ambiguous > Conflict > No conflict (if multiple exist).
    """
    if not pairs:
        return "No conflict", None, 0, 0
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
        kind = (
            "Permission vs Prohibition" if pr.kind == "perm-vs-prohib" else "Obligation blocked by Prohibition"
        )
        print(f"[{i:02d}] {label}: {kind}")
        for r in (pr.a, pr.b):
            for line in rule_fact_lines(gq, r):
                print("  " + line)
        print(f"  Action overlap: {pr.why_action}")
        print(f"  Details: {pr.details}\n")


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
        kind = (
            "Permission vs Prohibition" if pr.kind == "perm-vs-prohib" else "Obligation blocked by Prohibition"
        )
        print(f"[example] {label}: {kind}")
        for r in (pr.a, pr.b):
            for line in rule_fact_lines(gq, r):
                print("  " + line)
        print(f"  Action overlap: {pr.why_action}")
        print(f"  Details: {pr.details}")
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


# ---------------------------------------------------------------------
# DEBUG (prints to stderr inside satisfiability if turned on)
# ---------------------------------------------------------------------


def _valuestr(g: Graph, v: object) -> str:
    items = as_list(g, v)
    if items is not None:
        return "(" + ", ".join(qname(g, i) for i in items) + ")"
    return qname(g, v)  # type: ignore[arg-type]


def _dbg_atoms(g: Graph, atoms: Sequence[Atom]) -> List[str]:
    return [f"{qname(g, a.left)} {qname(g, a.op)} {_valuestr(g, a.right)}" for a in atoms]


# ---------------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------------


def main() -> int:
    gv = Graph()
    try:
        gv.parse(ODRL_TTL_URL, format="turtle")
    except Exception as e:  # pragma: no cover - network variability
        # Quizzes still work without the vocabulary; we install a small fallback
        # action hierarchy (see ActionHierarchy) to preserve intended overlaps.
        sys.stderr.write(
            "[warn] Could not fetch ODRL22.ttl " f"({e}). Using fallback action hierarchy.\n"
        )
        gv = Graph()
    gv.namespace_manager.bind("odrl", ODRL, override=True)
    gv.namespace_manager.bind("rdfs", RDFS, override=False)

    for url in QUIZ_URLS:
        name = url.rsplit("/", 1)[-1]
        try:
            print_report(name, gv, url)
        except Exception as e:  # pragma: no cover - one quiz file failing shouldn't stop all
            print(f"\n=== {name} ===")
            print(f"Error: {e}")
    return 0


if __name__ == "__main__":  # pragma: no cover
    sys.exit(main())

