#!/usr/bin/env python3
"""ocellus.py – a pocket‑sized, EYE‑style RDF reasoner in pure Python.

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

###############################################################################
# Imports & typing ############################################################
###############################################################################

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

# Public type aliases ---------------------------------------------------------
Term:   type = Union[URIRef, BNode, Literal, Tuple]  # a term or a Python tuple
Triple: type = Tuple[Term, Term, Term]
Subst:  type = Dict[URIRef, Term]                    # variable → term mapping

###############################################################################
# Namespaces ##################################################################
###############################################################################

LOG   = Namespace("http://www.w3.org/2000/10/swap/log#")
MATH  = Namespace("http://www.w3.org/2000/10/swap/math#")
TIME  = Namespace("http://www.w3.org/2000/10/swap/time#")
PROOF = Namespace("http://www.w3.org/2000/10/swap/reason#")
VAR   = Namespace("http://www.w3.org/2000/10/swap/var#")

###############################################################################
# Helpers: deterministic BNode generation (see top of file)
###############################################################################
# Conversion helpers (RDF ↔︎ Python tuples) ###################################
############################################################################### (RDF ↔︎ Python tuples) ###################################
###############################################################################

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
    head = BNode()
    Collection(g, head, [_from_python(x, g) for x in term])
    return head

###############################################################################
# Unification & substitution ###################################################
###############################################################################

def is_var(t: Term) -> bool:
    """Is *t* a log:variables (URIs under ``var:``)?"""
    return isinstance(t, URIRef) and str(t).startswith(str(VAR))


def subst_term(t: Term, σ: Subst) -> Term:  # noqa: D401 – σ = substitution
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

###############################################################################
# Built‑in predicate support (math:, time:) ###################################
###############################################################################

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
    except Exception:  # noqa: BLE001 – fall back
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


def _align_dt(a_dt, b_dt):  # noqa: D401 – helper
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

    return None

###############################################################################
# Rule representation #########################################################
###############################################################################


def _make_rule_term(g: Graph, body: List[Triple], head: List[Triple]) -> BNode:
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
    g.add((rule_bn, LOG.triple, Collection(g, BNode(), [b_bn, LOG.implies, h_bn]).uri))
    return rule_bn


@dataclass(slots=True, eq=False)
class Rule:
    """Holds the *compiled* (body/head lists) plus bookkeeping info."""

    head: List[Triple]
    body: List[Triple]
    kind: str  # "forward" | "backward" | "answer"
    term: BNode  # provenance term used in proof output

###############################################################################
# Reasoner core ###############################################################
###############################################################################

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
        """Deterministic order when proofs are requested (for diff‑ing)."""
        if not self.proof:
            return triples
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
    # Forward chaining -----------------------------------------------------

    def forward_chain(self, limit: int = 50):
        """Materialise until fixpoint or *limit* iterations."""
        changed = True
        while changed and limit:
            changed, limit = False, limit - 1
            for r in self.rules:
                if r.kind != "forward":
                    continue
                for env in self._match_body(r.body):
                    new = [t for t in self._apply(r.head, env) if t not in self._iter_facts_any()]
                    if not new:
                        continue
                    for t in self._sort(new):
                        self._add_fact(t)
                    changed = True
                    if self.proof and r not in self._proofd:
                        self._record_proof(r, env, new)

    # ---------------------------------------------------------------------
    # Backward reasoning / query answering --------------------------------

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
                    g.add(_triple_to_rdflib(t, g))
        if self.proof:
            _copy_proof_nodes(self.data, g)
        return g

    # Generic query interface ------------------------------------------------
    def ask(self, goals: List[Triple]):
        """Generator over all substitutions satisfying *goals*."""
        yield from self._prove(goals, {})

    # ---------------------------------------------------------------------
    # Internal engines #####################################################

    # -- depth‑first backward prover ---------------------------------------
    def _prove(self, goals: List[Triple], σ: Subst):
        # *** simple goal‑ordering heuristic ********************************
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
            str(g0[1]).startswith(str(MATH)) or str(g0[1]).startswith(str(TIME))
        ):
            σ2 = eval_builtin(g0[1], (g0[0], g0[2]), σ)
            if σ2:
                yield from self._prove(rest, σ2)
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

    # -- graph / fact helpers ----------------------------------------------
    def _iter_facts(self, pat: Triple):
        """Yield *triples* matching *pat* (vars/tuples = wildcards)."""
        def _conv(x):  # None for wildcard, else the concrete node
            return None if (is_var(x) or isinstance(x, tuple)) else _from_python(x, self.data)

        triples = self.data.triples((_conv(pat[0]), _conv(pat[1]), _conv(pat[2])))
        for s, p, o in self._sort(triples):
            yield tuple(_to_python(x, self.data) for x in (s, p, o))

    def _iter_facts_any(self):
        """Iterate *all* facts (optionally sorted for deterministic proofs)."""
        src = self.data if not self.proof else self._sort(self.data)
        for s, p, o in src:
            yield tuple(_to_python(x, self.data) for x in (s, p, o))

    # -- join helper for forward chaining -----------------------------------
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
                    str(gpat[1]).startswith(str(MATH)) or str(gpat[1]).startswith(str(TIME))
                ):
                    res = eval_builtin(gpat[1], (gpat[0], gpat[2]), σ)
                    if res:
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

    # ---------------------------------------------------------------------
    # Fact & proof bookkeeping --------------------------------------------
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
        """Attach a *single* node describing how *r* produced *prod*."""
        self._proofd.add(r)
        node = BNode()
        self.data.add((node, PROOF.viaRule, Collection(self.data, BNode(), [r.term]).uri))
        used = _triples_to_list(self.data, [self._subst(t, env) for t in r.body])
        self.data.add((node, PROOF.used, used))
        self.data.add((node, PROOF.produced, _triples_to_list(self.data, prod)))

    # ---------------------------------------------------------------------
    # Rule extraction from raw graphs --------------------------------------

    def _extract_rules(self, g: Graph):
        def _add(head: List[Triple], body: List[Triple], kind: str):
            self.rules.append(Rule(head, body, kind, _make_rule_term(self.data, body, head)))

        # forward rules -----------------------------------------------------
        for s, _, o in list(g.triples((None, LOG.implies, None))):
            _add(
                _graph_to_pats(g.value(o, LOG.graph), g),
                _graph_to_pats(g.value(s, LOG.graph), g),
                "forward",
            )
            _scrub(s, g); _scrub(o, g)

        # backward rules ----------------------------------------------------
        for s, _, o in list(g.triples((None, LOG.isImpliedBy, None))):
            _add(
                _graph_to_pats(g.value(s, LOG.graph), g),
                _graph_to_pats(g.value(o, LOG.graph), g),
                "backward",
            )
            _scrub(s, g); _scrub(o, g)

        # answer rules ------------------------------------------------------
        for s, _, o in list(g.triples((None, LOG.impliesAnswer, None))):
            _add(
                _graph_to_pats(g.value(o, LOG.graph), g),
                _graph_to_pats(g.value(s, LOG.graph), g),
                "answer",
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

###############################################################################
# Helper functions (graph munging) ###########################################
###############################################################################

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

###############################################################################
# CLI entry‑point #############################################################
###############################################################################

if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit("Usage: python ocellus.py [--proof] file.ttl [more.ttl …]")

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
        g.serialize(sys.stdout.buffer, format="turtle")

