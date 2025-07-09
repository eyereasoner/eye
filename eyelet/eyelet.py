#!/usr/bin/env python3
"""eyelet.py – a pocket‑sized, EYE‑style RDF reasoner in pure Python.

Features
--------
* Forward, backward and answer rules (\``log:implies``, ``log:isImpliedBy``,
  ``log:impliesAnswer``) parsed from ordinary Turtle/N3 graphs.
* Back‑tracking proof engine for backward rules, semi‑naïve materialisation
  for forward rules.
* Selected built‑ins from the W3C SWAP vocabularies (``math:``, ``time:``).
* Optional proof‐tree output (``--proof``) and deterministic blank node IDs
  for regression testing (``--nt``).

This module is *self‑contained*: only **rdflib** is required.  Being a teaching
/ exploration tool, correctness and clarity trump ultimate performance – but
with a couple of heuristics (goal ordering + occurs check) it comfortably
handles the classic benchmarks (BMB, Socrates, etc.).
"""
from __future__ import annotations

# -----------------------------------------------------------------------------
# Imports & typing
# -----------------------------------------------------------------------------

import itertools
import sys
import uuid
# --- deterministic blank‑node IDs (must come *before* rdflib imports) ---
_uuid_counter = itertools.count()

def _det_uuid4():
    """Return deterministic UUIDs 0,1,2,… instead of random ones (testing)."""
    return uuid.UUID(int=next(_uuid_counter))

uuid.uuid4 = _det_uuid4  # monkey‑patch for *all* rdflib BNodes

import re
from dataclasses import dataclass
from datetime import date, datetime, time as _time, timedelta
from typing import Dict, List, Optional, Tuple, Union, Set, Iterable

from rdflib import BNode, Graph, Literal, Namespace, URIRef
from rdflib.collection import Collection
from rdflib.namespace import RDF, XSD
# ---- rdflib Turtle patch: always print list heads as collections ----------
from rdflib.plugins.serializers.turtle import TurtleSerializer, OBJECT
from rdflib.term import BNode as _BNode

_node_name = next((n for n in ("_serialize_node", "_serialize_term", "label")
                   if hasattr(TurtleSerializer, n)), None)

if _node_name and not hasattr(TurtleSerializer, "_eyelet_patch"):
    _orig_node = getattr(TurtleSerializer, _node_name)

    def _mark_done(s, head):
        stk = [head]
        while stk:
            n = stk.pop()
            if not isinstance(n, _BNode) or n in s._serialized:
                continue
            s.subjectDone(n)
            nxt = s.store.value(n, RDF.rest)
            if nxt and nxt != RDF.nil:
                stk.append(nxt)
            fst = s.store.value(n, RDF.first)
            if isinstance(fst, _BNode):
                stk.append(fst)

    def _node_lists(self, node, position):            # monkey-patch
        if isinstance(node, _BNode):
            try:
                items = list(Collection(self.store, node))
            except Exception:
                items = []
            if items:                                 # list head
                _mark_done(self, node)
                inner = " ".join(_orig_node(self, i, position) for i in items)
                return f"( {inner} )"
        return _orig_node(self, node, position)

    setattr(TurtleSerializer, _node_name, _node_lists)
    TurtleSerializer._eyelet_patch = True

    # ---- patch subject serializer so list heads in *subject* position
    #      are printed as “( … )” and rdf:first/rest arcs are suppressed ----
    if not hasattr(TurtleSerializer, "_eyelet_subject_patch"):
        _orig_s_sq = TurtleSerializer.s_squared          # type: ignore[attr-defined]

        def _s_squared_lists(self: TurtleSerializer, subj):             # type: ignore
            if not isinstance(subj, _BNode):
                return _orig_s_sq(self, subj)
            try:
                items = list(Collection(self.store, subj))
            except Exception:
                return _orig_s_sq(self, subj)
            if not items:                        # not a list head
                return _orig_s_sq(self, subj)

            _mark_done(self, subj)

            # gather predicates other than rdf:first/rest
            props = self.buildPredicateHash(subj)
            props.pop(RDF.first, None)
            props.pop(RDF.rest,  None)
            if not props:
                return True                      # nothing to print

            # now we know we have something to say – print subject collection
            self.write("\n" + self.indent() + "( ")
            for i, itm in enumerate(items):
                if i:
                    self.write(" ")
                self.path(itm, OBJECT)
            self.write(" )")

            plist = self.sortProperties(props)
            self.write(" ")
            self.verb(plist[0])
            self.objectList(props[plist[0]])
            for p in plist[1:]:
                self.write(" ;\n" + self.indent(1))
                self.verb(p, newline=True)
                self.objectList(props[p])
            self.write(" .")
            return True

        TurtleSerializer.s_squared = _s_squared_lists   # type: ignore[assignment]
        TurtleSerializer._eyelet_subject_patch = True

# Public type aliases ---------------------------------------------------------
Term:   type = Union[URIRef, BNode, Literal, Tuple]  # a term or a Python tuple
Triple: type = Tuple[Term, Term, Term]
Subst:  type = Dict[URIRef, Term]                    # variable → term mapping

# -----------------------------------------------------------------------------
# Namespaces
# -----------------------------------------------------------------------------

LOG   = Namespace("http://www.w3.org/2000/10/swap/log#")
MATH  = Namespace("http://www.w3.org/2000/10/swap/math#")
TIME  = Namespace("http://www.w3.org/2000/10/swap/time#")
LIST  = Namespace("http://www.w3.org/2000/10/swap/list#")
PROOF = Namespace("http://www.w3.org/2000/10/swap/reason#")
VAR   = Namespace("http://www.w3.org/2000/10/swap/var#")

# -----------------------------------------------------------------------------
# Conversion helpers (RDF ↔︎ Python tuples) (RDF ↔︎ Python tuples)
# -----------------------------------------------------------------------------

def _to_python(node: Term, g: Graph, _memo=None) -> Term:
    """Convert an RDF list to a Python tuple (memoised & id‑stable)."""
    if _memo is None:
        _memo = {}
    if isinstance(node, (URIRef, Literal)):
        return node
    if isinstance(node, BNode):
        if node in _memo:
            return _memo[node]
        if (node, RDF.first, None) in g:
            elements: List[Term] = []
            cur: Optional[BNode | URIRef] = node
            while cur and cur != RDF.nil:
                elements.append(_to_python(g.value(cur, RDF.first), g, _memo))
                cur = g.value(cur, RDF.rest)
            tpl: Tuple = tuple(elements)
            _memo[node] = tpl
            return tpl
    return node

def _from_python(term: Term, g: Graph) -> Term:
    """Convert a Python tuple back into an RDF list (shared nodes allowed)."""
    if not isinstance(term, tuple):
        return term
    if len(term) == 0:
        return RDF.nil
    head = BNode()
    Collection(g, head, [_from_python(x, g) for x in term])
    return head

# -----------------------------------------------------------------------------
# Unification & substitution
# -----------------------------------------------------------------------------

def is_var(t: Term) -> bool:
    """Is *t* a log:variables (URIs under ``var:``)?"""
    return isinstance(t, URIRef) and str(t).startswith(str(VAR))

def subst_term(t: Term, σ: Subst) -> Term:
    """Recursively substitute variables in *t* according to σ (Greek sigma)."""
    if isinstance(t, tuple):
        return tuple(subst_term(x, σ) for x in t)
    return σ.get(t, t) if is_var(t) else t

# ---------- occurs‑check to avoid infinite terms -----------------------------

def _occurs(v: URIRef, x: Term, σ: Subst) -> bool:
    """Return *True* iff *v* appears (directly or via σ) in *x*."""
    if v == x:
        return True
    if is_var(x) and x in σ:
        return _occurs(v, σ[x], σ)
    if isinstance(x, tuple):
        return any(_occurs(v, xi, σ) for xi in x)
    return False

def _extend(v: URIRef, x: Term, σ: Subst) -> Optional[Subst]:
    """Bind *v → x* (copying σ) unless inconsistent or cyclic."""
    if v in σ:
        return σ if σ[v] == x else None
    if _occurs(v, x, σ):
        return None
    σ2 = dict(σ)
    σ2[v] = x
    return σ2

def unify_term(a: Term, b: Term, σ: Subst) -> Optional[Subst]:
    """Syntactic (rational‑tree) unification with occurs‑check."""
    a, b = subst_term(a, σ), subst_term(b, σ)
    if a == b:
        return σ
    if is_var(a):
        return _extend(a, b, σ)
    if is_var(b):
        return _extend(b, a, σ)
    if isinstance(a, tuple) and isinstance(b, tuple) and len(a) == len(b):
        for aa, bb in zip(a, b):
            σ = unify_term(aa, bb, σ)
            if σ is None:
                return None
        return σ
    return None

def unify_triple(pat: Triple, fact: Triple, σ: Subst) -> Optional[Subst]:
    """Unify triple *pat* with triple *fact* under substitution σ."""
    for p, f in zip(pat, fact):
        σ = unify_term(p, f, σ)
        if σ is None:
            return None
    return σ

# -----------------------------------------------------------------------------
# Built‑in predicate support (math:, time:)
# -----------------------------------------------------------------------------

_DUR_RE = re.compile(
    r"^P(?:(?P<years>\d+)Y)?(?:(?P<months>\d+)M)?(?:(?P<days>\d+)D)?"
    r"(?:T(?:(?P<hours>\d+)H)?(?:(?P<minutes>\d+)M)?(?:(?P<seconds>\d+)S)?)?$"
)

def _duration_to_days(lit: Literal) -> Optional[float]:
    if not (isinstance(lit, Literal) and lit.datatype == XSD.duration):
        return None
    m = _DUR_RE.match(str(lit))
    if not m:
        return None
    p = {k: int(v) if v else 0 for k, v in m.groupdict().items()}
    days = p["years"] * 365 + p["months"] * 30 + p["days"]
    secs = p["hours"] * 3600 + p["minutes"] * 60 + p["seconds"]
    return days + secs / 86400.0

def _num(lit: Literal) -> Optional[float]:
    if not isinstance(lit, Literal):
        return None
    if lit.datatype == XSD.duration:
        return _duration_to_days(lit)
    try:
        return float(lit)
    except Exception:
        return None

def _iso_to_py(lit: Literal):
    if not isinstance(lit, Literal):
        return None
    if lit.datatype == XSD.dateTime:
        try:
            return datetime.fromisoformat(str(lit))
        except ValueError:
            return None
    if lit.datatype == XSD.date:
        try:
            return date.fromisoformat(str(lit))
        except ValueError:
            return None
    if lit.datatype == XSD.time:
        try:
            return _time.fromisoformat(str(lit))
        except ValueError:
            return None
    return None

def _td_to_dec(td: timedelta) -> Literal:
    """Convert timedelta → xsd:decimal (days)."""
    return Literal(td.total_seconds() / 86400.0, datatype=XSD.decimal)

def _align_dt(a_dt, b_dt):
    """Align *date* / *time* / *datetime* operands so they can be subtracted."""
    if isinstance(a_dt, _time) and isinstance(b_dt, _time):
        today = date.today()
        return (datetime.combine(today, a_dt), datetime.combine(today, b_dt))
    if isinstance(a_dt, _time) and isinstance(b_dt, (date, datetime)):
        a_dt = datetime.combine(b_dt.date() if isinstance(b_dt, datetime) else b_dt, a_dt)
    if isinstance(b_dt, _time) and isinstance(a_dt, (date, datetime)):
        b_dt = datetime.combine(a_dt.date() if isinstance(a_dt, datetime) else a_dt, b_dt)
    if isinstance(a_dt, date) and not isinstance(a_dt, datetime):
        a_dt = datetime.combine(a_dt, _time(0, 0))
    if isinstance(b_dt, date) and not isinstance(b_dt, datetime):
        b_dt = datetime.combine(b_dt, _time(0, 0))
    return a_dt, b_dt

def eval_builtin(pred: URIRef, args: Tuple, σ: Subst) -> Optional[Subst]:
    """Evaluate supported built‑ins; return an extended σ or *None*."""
    # math:sum ------------------------------------------------------------------
    if pred == MATH.sum:
        (lst,), F = args[:-1], args[-1]
        lst_val = subst_term(lst, σ)
        if isinstance(lst_val, tuple):
            nums = [_num(x) for x in lst_val]
            if all(n is not None for n in nums):
                return unify_term(F, Literal(sum(nums), datatype=XSD.decimal), σ)
        return None

    # math:product --------------------------------------------------------------
    if pred == MATH.product:
        (lst,), F = args[:-1], args[-1]
        lst_val = subst_term(lst, σ)
        if isinstance(lst_val, tuple):
            nums = [_num(x) for x in lst_val]
            if all(n is not None for n in nums):
                product = 1
                for n in nums:
                    product *= n
                return unify_term(F, Literal(product, datatype=XSD.decimal), σ)
        return None

    # math:quotient -------------------------------------------------------------
    if pred == MATH.quotient:
        (pair,), F = args[:-1], args[-1]
        if isinstance(pair, tuple) and len(pair) == 2:
            a_raw, b_raw = (subst_term(x, σ) for x in pair)
            a_num, b_num = _num(a_raw), _num(b_raw)
            if a_num is not None and b_num not in (None, 0):
                return unify_term(F, Literal(a_num / b_num, datatype=XSD.decimal), σ)
        return None

    # math:difference ---------------------------------------------------------
    if pred == MATH.difference:
        (pair,), F = args[:-1], args[-1]
        if not (isinstance(pair, tuple) and len(pair) == 2):
            return None
        a_raw, b_raw = (subst_term(z, σ) for z in pair)
        a_num, b_num = _num(a_raw), _num(b_raw)
        if a_num is not None and b_num is not None:
            return unify_term(F, Literal(a_num - b_num, datatype=XSD.decimal), σ)
        a_dt, b_dt = _iso_to_py(a_raw), _iso_to_py(b_raw)
        if a_dt is not None and b_dt is not None:
            a_dt, b_dt = _align_dt(a_dt, b_dt)
            return unify_term(F, _td_to_dec(a_dt - b_dt), σ)
        return None

    # math:greaterThan --------------------------------------------------------
    if pred == MATH.greaterThan:
        F, A = args
        f, a = _num(subst_term(F, σ)), _num(subst_term(A, σ))
        return σ if (f is not None and a is not None and f > a) else None

    # time:localTime ----------------------------------------------------------
    if pred == TIME.localTime:
        _subj, obj = args
        now = datetime.now().isoformat(timespec="seconds")
        return unify_term(obj, Literal(now, datatype=XSD.dateTime), σ)

    # list:append -------------------------------------------------------------
    if pred == LIST.append:
        lists_term, result_term = args
        lists_val  = subst_term(lists_term,  σ)
        result_val = subst_term(result_term, σ)
        if not isinstance(lists_val, tuple):
            return None

        def _concat(ts):
            acc = []
            for t in ts:
                if not isinstance(t, tuple):
                    return None
                acc.extend(t)
            return tuple(acc)

        # all pieces ground ⇒ single concat
        if all(isinstance(li, tuple) for li in lists_val):
            return unify_term(result_term, _concat(lists_val), σ)

        # exactly two vars + ground result ⇒ enumerate every split
        if isinstance(result_val, tuple) and len(lists_val) == 2:
            L1_var, L2_var = lists_term
            R = result_val
            sols = []
            for i in range(len(R) + 1):
                head, tail = R[:i], R[i:]
                σ1 = unify_term(L1_var, tuple(head), σ)
                if σ1 is None:
                    continue
                σ2 = unify_term(L2_var, tuple(tail), σ1)
                if σ2 is not None:
                    sols.append(σ2)
            return sols or None

        # partial concat when result still a var
        cat = _concat(lists_val)
        if cat is not None and is_var(result_term):
            return unify_term(result_term, cat, σ)

        return None

    return None

# -----------------------------------------------------------------------------
# Rule representation
# -----------------------------------------------------------------------------

def _make_rule_term(g: Graph, body: List[Triple], head: List[Triple], pred: URIRef = LOG.implies) -> BNode:
    """Create a *single* provenance blank node that reifies the rule."""

    def _pat_list(pats: Iterable[Triple]) -> BNode:
        items: List[BNode] = []
        for s, p, o in pats:
            tbn = BNode()
            g.add(
                (
                    tbn,
                    LOG.triple,
                    Collection(
                        g,
                        BNode(),
                        [_from_python(s, g), _from_python(p, g), _from_python(o, g)],
                    ).uri,
                )
            )
            items.append(tbn)
        return Collection(g, BNode(), items).uri

    body_l = _pat_list(body)
    head_l = _pat_list(head)
    b_bn = BNode(); g.add((b_bn, LOG.graph, body_l))
    h_bn = BNode(); g.add((h_bn, LOG.graph, head_l))
    rule_bn = BNode()
    g.add((rule_bn, LOG.triple, Collection(g, BNode(), [b_bn, pred, h_bn]).uri))
    return rule_bn

@dataclass(slots=True, eq=False)
class Rule:
    """Holds the *compiled* (body/head lists) plus bookkeeping info."""

    head: List[Triple]
    body: List[Triple]
    kind: str  # "forward" | "backward" | "answer"
    term: BNode  # provenance term used in proof output

# -----------------------------------------------------------------------------
# Reasoner core
# -----------------------------------------------------------------------------

class Reasoner:
    """Tiny yet surprisingly capable reasoner."""

    def __init__(self, *, proof: bool = False):
        self.data: Graph = Graph()
        self.rules: List[Rule] = []
        self.proof = proof
        if proof:
            self.data.bind("proof", PROOF)
        self._proofd: Set[Rule] = set()  # which rules already recorded

    # ---------------------------------------------------------------------
    # Utility
    # ---------------------------------------------------------------------

    def _sort(self, triples):
        """Deterministic order."""
        return sorted(triples, key=lambda t: (str(t[0]), str(t[1]), str(t[2])))

    # ---------------------------------------------------------------------
    # Loading & parsing rules
    # ---------------------------------------------------------------------

    def load(self, *files: str | bytes):
        """Parse Turtle/N3 files and collect facts / rules."""
        for p in files:
            g = Graph(); g.parse(p)
            # copy prefixes for pretty output
            for pfx, uri in g.namespace_manager.namespaces():
                self.data.bind(pfx or "", uri, replace=False)
            self._extract_rules(g)
            self.data += g  # keep only the *facts* (rules were scrubbed)

    # ---------------------------------------------------------------------
    # Forward chaining
    # ---------------------------------------------------------------------

    def forward_chain(self, limit: int = 50):
        """Materialise until fixpoint or *limit* iterations."""
        changed = True
        while changed and limit:
            changed, limit = False, limit - 1
            for r in self.rules:
                if r.kind != "forward":
                    continue
                for env in self._match_body(r.body):
                    new = [
                        t
                        for t in self._apply(r.head, env)
                        if t not in self._iter_facts_any()
                    ]
                    if not new:
                        continue
                    for t in self._sort(new):
                        self._add_fact(t)
                    changed = True
                    if self.proof:
                        # record only the *first* materialisation of each
                        # forward rule – identical to previous behaviour
                        if r not in self._proofd:
                            self._record_proof(r, env, new)

    # -------------------------------------------------------------------------
    # Backward reasoning / query answering
    # -------------------------------------------------------------------------

    def answers(self) -> Graph:
        """Evaluate all ``log:impliesAnswer`` rules and return a result graph."""
        g = Graph(); g.namespace_manager = self.data.namespace_manager
        if self.proof:
            g.bind("proof", PROOF)

        for r in self.rules:
            if r.kind != "answer":
                continue
            for env in self.ask(r.body):
                for t in self._apply(r.head, env):
                    # add the answer triple to the *result* graph
                    g.add(_triple_to_rdflib(t, g))
                    # and, if requested, write a separate proof node
                    if self.proof:
                        self._record_answer_proof(r, env, t)

        # copy *all* proof nodes accumulated so far into the result graph
        if self.proof:
            _copy_proof_nodes(self.data, g)
        return g

    # Generic query interface ------------------------------------------------
    def ask(self, goals: List[Triple]):
        """Generator over all substitutions satisfying *goals*."""
        yield from self._prove(goals, {})

    # -------------------------------------------------------------------------
    # Internal engines
    # -------------------------------------------------------------------------

    # -- depth‑first backward prover ------------------------------------------
    def _prove(self, goals: List[Triple], σ: Subst):
        # --- simple goal‑ordering heuristic ----------------------------------
        goals = sorted(
            goals,
            key=lambda t: (
                3 - sum(1 for x in t if is_var(x)),  # fewer vars = earlier
                isinstance(t[1], URIRef),            # bound predicate first
            ),
        )
        if not goals:
            yield σ; return
        g0, *rest = goals
        g0 = self._subst(g0, σ)

        # Built‑ins ↔︎ log:magic predicates
        if isinstance(g0[1], URIRef) and (
            str(g0[1]).startswith(str(MATH))
            or str(g0[1]).startswith(str(TIME))
            or str(g0[1]).startswith(str(LIST))
        ):
            res = eval_builtin(g0[1], (g0[0], g0[2]), σ)
            if res:
                if isinstance(res, (list, tuple, set)):   # multi-solutions
                    for σ2 in res:
                        yield from self._prove(rest, σ2)
                else:
                    yield from self._prove(rest, res)
            return

        # Match against facts
        for fact in self._iter_facts(g0):
            σ2 = unify_triple(g0, fact, σ)
            if σ2:
                yield from self._prove(rest, σ2)

        # Match against backward rules
        for r in self.rules:
            if r.kind != "backward":
                continue
            for h in r.head:
                σ2 = unify_triple(g0, h, σ)
                if σ2 is None:
                    continue
                new = (
                    [self._subst(t, σ2) for t in r.body]
                    + [self._subst(t, σ2) for t in r.head if t is not h]
                    + rest
                )
                yield from self._prove(new, σ2)

    # -- graph / fact helpers -------------------------------------------------
    def _iter_facts(self, pat: Triple):
        """Yield *triples* matching *pat* (vars/tuples = wildcards)."""
        def _conv(x):  # None for wildcard, else the concrete node
            return None if (is_var(x) or isinstance(x, tuple)) else _from_python(x, self.data)

        triples = self.data.triples((_conv(pat[0]), _conv(pat[1]), _conv(pat[2])))
        for s, p, o in self._sort(triples):
            yield tuple(_to_python(x, self.data) for x in (s, p, o))

    def _iter_facts_any(self):
        """Iterate *all* facts (sorted for deterministic proofs)."""
        src = sorted(self.data, key=lambda t: (str(t[0]), str(t[1]), str(t[2])))
        for s, p, o in src:
            yield tuple(_to_python(x, self.data) for x in (s, p, o))

    # -- join helper for forward chaining -------------------------------------
    def _match_body(self, body: List[Triple]):
        body = sorted(
            body,
            key=lambda t: (
                3 - sum(1 for x in t if is_var(x)),  # heuristic as above
                isinstance(t[1], URIRef),
            ),
        )
        envs = [{}]
        for pat in body:
            nxt = []
            for σ in envs:
                gpat = self._subst(pat, σ)
                if isinstance(gpat[1], URIRef) and (
                    str(gpat[1]).startswith(str(MATH))
                    or str(gpat[1]).startswith(str(TIME))
                    or str(gpat[1]).startswith(str(LIST))
                ):
                    res = eval_builtin(gpat[1], (gpat[0], gpat[2]), σ)
                    if res:
                        if isinstance(res, (list, tuple, set)):
                            nxt.extend(res)
                        else:
                            nxt.append(res)
                    continue
                for fact in self._iter_facts(gpat):
                    σ2 = unify_triple(gpat, fact, σ)
                    if σ2:
                        nxt.append(σ2)
            envs = nxt
            if not envs:
                break
        yield from envs

    # -------------------------------------------------------------------------
    # Fact & proof bookkeeping
    # -------------------------------------------------------------------------

    def _subst(self, t: Triple, σ: Subst) -> Triple:
        return tuple(subst_term(x, σ) for x in t)

    def _apply(self, ts: List[Triple], σ: Subst):
        yield from (self._subst(t, σ) for t in ts)

    def _add_fact(self, t: Triple):
        s = _from_python(t[0], self.data) if isinstance(t[0], tuple) else t[0]
        p = _from_python(t[1], self.data) if isinstance(t[1], tuple) else t[1]
        o = _from_python(t[2], self.data) if isinstance(t[2], tuple) else t[2]
        self.data.add((s, p, o))

    def _record_proof(self, r: Rule, env: Subst, prod: List[Triple]):
        """
        Attach a single node describing how *r* produced *prod*.

        Used for **forward** rules only – therefore, we keep the historical
        behaviour of de-duplicating per-rule via ``self._proofd``.
        """
        # forward rules are de-duplicated
        self._proofd.add(r)
        node = BNode()
        self.data.add((node, PROOF.viaRule, Collection(self.data, BNode(), [r.term]).uri))
        used = _triples_to_list(
            self.data, [self._subst(t, env) for t in r.body]
        )
        self.data.add((node, PROOF.used, used))
        self.data.add((node, PROOF.produced, _triples_to_list(self.data, prod)))

    def _record_answer_proof(self, r: Rule, env: Subst, prod_triple: Triple):
        """
        Like ``_record_proof`` but we embed a *fresh* copy of the answer rule
        each time so the serializer can inline it as «[ … ]» instead of giving
        it a repeated blank-node label such as ``_:N00034``.
        """
        # --- make a *clone* of the rule description --------------------------
        rule_clone = _make_rule_term(self.data, r.body, r.head)
        # --- make a *clone* with the *answer* predicate ----------------------
        rule_clone = _make_rule_term(
            self.data, r.body, r.head, LOG.impliesAnswer
        )

        node = BNode()

        # provenance: which rule justified this answer
        self.data.add(
            (
                node,
                PROOF.viaRule,
                Collection(self.data, BNode(), [rule_clone]).uri,
            )
        )

        # what goals were used (body, after substitution)
        used = _triples_to_list(
            self.data, [self._subst(t, env) for t in r.body]
        )
        self.data.add((node, PROOF.used, used))

        # what was produced (just the single answer triple)
        self.data.add(
            (node, PROOF.produced, _triples_to_list(self.data, [prod_triple]))
        )

    # -------------------------------------------------------------------------
    # Rule extraction from raw graphs
    # -------------------------------------------------------------------------

    def _extract_rules(self, g: Graph):
        def _add(head: List[Triple], body: List[Triple], kind: str, pred: URIRef = LOG.implies):
            self.rules.append(Rule(head, body, kind, _make_rule_term(self.data, body, head, pred)))

        # forward rules -------------------------------------------------------
        for s, _, o in list(g.triples((None, LOG.implies, None))):
            _add(
                _graph_to_pats(g.value(o, LOG.graph), g),
                _graph_to_pats(g.value(s, LOG.graph), g),
                "forward",
                LOG.implies,
            )
            _scrub(s, g); _scrub(o, g)

        # backward rules ------------------------------------------------------
        for s, _, o in list(g.triples((None, LOG.isImpliedBy, None))):
            _add(
                _graph_to_pats(g.value(s, LOG.graph), g),
                _graph_to_pats(g.value(o, LOG.graph), g),
                "backward",
            )
            _scrub(s, g); _scrub(o, g)

        # answer rules --------------------------------------------------------
        for s, _, o in list(g.triples((None, LOG.impliesAnswer, None))):
            _add(
                _graph_to_pats(g.value(o, LOG.graph), g),
                _graph_to_pats(g.value(s, LOG.graph), g),
                "answer",
                LOG.impliesAnswer,
            )
            _scrub(s, g); _scrub(o, g)

        # canonical ordering (helps diff‑based tests)
        kind_rank = {"forward": 0, "backward": 1, "answer": 2}
        self.rules.sort(
            key=lambda r: (
                kind_rank[r.kind],
                tuple(sorted(str(t) for t in r.head)),
                tuple(sorted(str(t) for t in r.body)),
            )
        )

# -----------------------------------------------------------------------------
# Helper functions (graph munging)
# -----------------------------------------------------------------------------

def _graph_to_pats(list_node, g: Graph) -> List[Triple]:
    if list_node is None:
        return []
    pats: List[Triple] = []
    for tbn in Collection(g, list_node):
        inner = g.value(tbn, LOG.triple)
        if inner is None:
            continue
        parts = list(Collection(g, inner))
        if len(parts) >= 3:
            pats.append(tuple(_to_python(x, g) for x in parts[:3]))
    return pats

def _scrub(n, g: Graph):
    """Remove reified rule artefacts – leaves only *facts* in *g*."""
    todo = [n]
    while todo:
        u = todo.pop()
        for s, p, o in list(g.triples((u, None, None))):
            g.remove((s, p, o))
            if p in (LOG.graph, LOG.triple, RDF.first, RDF.rest):
                todo.append(o)

def _triple_bnode(g: Graph, tr: Triple):
    bn = BNode()
    g.add((bn, LOG.triple, Collection(g, BNode(), [_from_python(x, g) for x in tr]).uri))
    return bn

def _triples_to_list(g: Graph, tris: List[Triple]):
    return Collection(g, BNode(), [_triple_bnode(g, t) for t in tris]).uri

def _triple_to_rdflib(tr: Triple, g: Graph):
    return tuple(_from_python(x, g) if isinstance(x, tuple) else x for x in tr)

def _copy_proof_nodes(src: Graph, dst: Graph):
    q = [s for s, _, _ in src.triples((None, PROOF.viaRule, None))]
    dst += src.triples((None, PROOF.viaRule, None))
    seen: Set[BNode] = set(q)
    while q:
        n = q.pop(0)
        for s, p, o in sorted(src.triples((n, None, None)), key=lambda t: (str(t[1]), str(t[2]))):
            dst.add((s, p, o))
            if isinstance(o, BNode) and o not in seen:
                seen.add(o); q.append(o)

# -----------------------------------------------------------------------------
# CLI entry‑point
# -----------------------------------------------------------------------------

if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit("Usage: python eyelet.py [--proof] [--nt] file.ttl [more.ttl]")

    want_proof = "--proof" in sys.argv
    use_nt     = "--nt" in sys.argv
    files      = [f for f in sys.argv[1:] if f not in ("--proof", "--nt")]

    R = Reasoner(proof=want_proof)
    R.load(*files)
    R.forward_chain()

    g = R.answers()
    if use_nt:
        # Deterministic order – friendly for diff‑based regression tests
        out = g.serialize(format="nt")
        sys.stdout.write("".join(sorted(out.splitlines(keepends=True))))
    else:
        # rdflib leaves one-or-more blank lines at the end; strip them
        txt = g.serialize(format="turtle")
        sys.stdout.write(txt.rstrip() + "\n")
    print()

