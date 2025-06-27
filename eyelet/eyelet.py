#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
eyelet.py – pocket-sized EYE-style reasoner with provenance
===========================================================

 • log:implies       – forward rules
 • log:isImpliedBy   – backward rules
 • log:impliesAnswer – answer rules
   (CLI prints only instantiated answer-heads;
    add --trace to embed one provenance node per forward rule)

Built-ins implemented (numeric literals only)
   • math:difference
   • math:greaterThan
"""

import sys
from typing import Dict, List, Optional, Sequence, Tuple, Union, Iterator, Set

from rdflib import Graph, BNode, URIRef, Literal, Namespace
from rdflib.collection import Collection
from rdflib.namespace import RDF, XSD

# ─── namespaces ────────────────────────────────────────────────────
LOG   = Namespace("http://www.w3.org/2000/10/swap/log#")
MATH  = Namespace("http://www.w3.org/2000/10/swap/math#")
VAR   = Namespace("http://www.w3.org/2000/10/swap/var#")
TRACE = Namespace("http://example.org/trace#")

Triple = Tuple[Union[URIRef, BNode, Literal],
               Union[URIRef, BNode],
               Union[URIRef, BNode, Literal]]
Subst = Dict[URIRef, Union[URIRef, BNode, Literal]]

# ─── variable helpers / unification ────────────────────────────────
def is_var(t) -> bool:
    return isinstance(t, URIRef) and str(t).startswith(str(VAR))

def subst_term(t, σ: Subst):
    return σ.get(t, t) if is_var(t) else t

def _extend(v: URIRef, x, σ: Subst) -> Optional[Subst]:
    if v in σ and σ[v] != x:
        return None
    s2 = dict(σ); s2[v] = x
    return s2

def unify_term(a, b, σ: Subst) -> Optional[Subst]:
    a, b = subst_term(a, σ), subst_term(b, σ)
    if a == b:
        return σ
    if is_var(a):
        return _extend(a, b, σ)
    if is_var(b):
        return _extend(b, a, σ)
    return None

def unify_triple(pat: Triple, fact: Triple, σ: Subst) -> Optional[Subst]:
    for p, f in zip(pat, fact):
        σ = unify_term(p, f, σ)
        if σ is None:
            return None
    return σ

# ─── mini built-ins ────────────────────────────────────────────────
def _num(n) -> Optional[float]:
    if isinstance(n, Literal):
        try:
            return float(n)
        except Exception:
            return None
    return None

def eval_builtin(pred: URIRef, args: Tuple, σ: Subst) -> Optional[Subst]:
    if pred == MATH.difference:                 # ((A B) math:difference F)
        (pair,), F = args[:-1], args[-1]
        if not (isinstance(pair, tuple) and len(pair) == 2):
            return None
        a, b = map(lambda z: _num(subst_term(z, σ)), pair)
        if a is None or b is None:
            return None
        return unify_term(F, Literal(a - b, datatype=XSD.decimal), σ)

    if pred == MATH.greaterThan:                # (F math:greaterThan A)
        F, A = args
        f, a = _num(subst_term(F, σ)), _num(subst_term(A, σ))
        if f is None or a is None:
            return None
        return σ if f > a else None

    return None

# ─── helper to turn pattern lists into an RDF rule term ────────────
def _make_rule_term(g: Graph,
                    body_pats: List[Triple],
                    head_pats: List[Triple]) -> BNode:
    """Return a blank node encoding the rule in N3 style."""
    def _pat_list(pats):
        items = []
        for s, p, o in pats:
            tbn = BNode()
            g.add((tbn, LOG.triple,
                   Collection(g, BNode(), [s, p, o]).uri))
            items.append(tbn)
        return Collection(g, BNode(), items).uri

    body_list = _pat_list(body_pats)
    head_list = _pat_list(head_pats)

    body_bn = BNode(); g.add((body_bn, LOG.graph, body_list))
    head_bn = BNode(); g.add((head_bn, LOG.graph, head_list))

    rule_bn = BNode()
    g.add((rule_bn, LOG.triple,
           Collection(g, BNode(), [body_bn, LOG.implies, head_bn]).uri))
    return rule_bn

# ─── rule container ────────────────────────────────────────────────
class Rule:
    def __init__(self, head, body, kind, term):
        self.head, self.body, self.kind, self.term = head, body, kind, term

# ─── reasoner core ─────────────────────────────────────────────────
class Reasoner:
    def __init__(self, trace=False):
        self.data = Graph()
        self.rules: List[Rule] = []
        self.trace = trace
        if trace:
            self.data.bind("trace", TRACE)
        self._traced_rules: Set[Rule] = set()   # ensure one provenance node / rule

    # ─ I/O ───────────────────────────────────────────────────────
    def load(self, *files):
        for path in files:
            g = Graph(); g.parse(path)

            # copy prefixes (incl. default :)
            for pfx, uri in g.namespace_manager.namespaces():
                self.data.bind(pfx or "", uri, replace=False)

            self._extract_rules(g)
            self.data += g                       # keep only fact triples

    # ─ forward chaining ─────────────────────────────────────────
    def forward_chain(self, limit=50):
        changed = True
        while changed and limit:
            changed, limit = False, limit - 1
            for r in self.rules:
                if r.kind != "forward":
                    continue
                for env in self._match_body(r.body):
                    new = [t for t in self._apply(r.head, env)
                           if t not in self.data]
                    if not new:
                        continue
                    for t in new:
                        self.data.add(t)
                    changed = True
                    if self.trace and r not in self._traced_rules:
                        self._record_trace(r, env, new)

    def _record_trace(self, rule: Rule, env: Subst, produced: List[Triple]):
        """Add a provenance node for this rule (once)."""
        self._traced_rules.add(rule)
        node = BNode()
        self.data.add((node, TRACE.viaRule,
                       Collection(self.data, BNode(), [rule.term]).uri))
        used  = _triples_to_list(
            self.data, [self._subst(t, env) for t in rule.body])
        prod  = _triples_to_list(self.data, produced)
        self.data.add((node, TRACE.used,     used))
        self.data.add((node, TRACE.produced, prod))

    # ─ query / answers ──────────────────────────────────────────
    def answers(self) -> Graph:
        g = Graph()
        g.namespace_manager = self.data.namespace_manager
        if self.trace:
            g.bind("trace", TRACE)

        for r in self.rules:
            if r.kind != "answer":
                continue
            for env in self.ask(r.body):
                for t in self._apply(r.head, env):
                    g.add(t)

        if self.trace:
            _copy_trace_nodes(self.data, g)

        return g

    def ask(self, goals):
        yield from self._prove(goals, {})

    # ─ proof search ────────────────────────────────────────────
    def _prove(self, goals: Sequence[Triple], σ: Subst) -> Iterator[Subst]:
        if not goals:
            yield σ; return
        g0, *rest = goals
        g0 = self._subst(g0, σ)

        if isinstance(g0[1], URIRef) and str(g0[1]).startswith(str(MATH)):
            σ2 = eval_builtin(g0[1], (g0[0], g0[2]), σ)
            if σ2 is not None:
                yield from self._prove(rest, σ2)
            return

        for fact in self.data.triples(self._rdflib_pat(g0)):
            σ2 = unify_triple(g0, fact, σ)
            if σ2 is not None:
                yield from self._prove(rest, σ2)

        for r in self.rules:
            if r.kind != "backward":
                continue
            for h in r.head:
                σ2 = unify_triple(g0, h, σ)
                if σ2 is None:
                    continue
                new_goals = ([self._subst(p, σ2) for p in r.body] +
                             [self._subst(p, σ2) for p in r.head if p is not h] +
                             rest)
                yield from self._prove(new_goals, σ2)

    # ─ body matcher ────────────────────────────────────────────
    def _match_body(self, body):
        envs: List[Subst] = [{}]
        for pat in body:
            nxt: List[Subst] = []
            for σ in envs:
                gpat = self._subst(pat, σ)
                if isinstance(gpat[1], URIRef) and str(gpat[1]).startswith(str(MATH)):
                    res = eval_builtin(gpat[1], (gpat[0], gpat[2]), σ)
                    if res is not None:
                        nxt.append(res)
                    continue
                for fact in self.data.triples(self._rdflib_pat(gpat)):
                    σ2 = unify_triple(gpat, fact, σ)
                    if σ2 is not None:
                        nxt.append(σ2)
            envs = nxt
            if not envs:
                break
        yield from envs

    # ─ helpers ──────────────────────────────────────────────────
    def _subst(self, t, σ): return tuple(subst_term(x, σ) for x in t)  # type: ignore
    def _apply(self, ts, σ): yield from (self._subst(t, σ) for t in ts)
    @staticmethod
    def _rdflib_pat(t): return tuple(None if is_var(x) else x for x in t)

    # ─ rule extraction ──────────────────────────────────────────
    def _extract_rules(self, g: Graph):

        def _add_rule(head_pats, body_pats, kind):
            term = _make_rule_term(self.data, body_pats, head_pats)
            self.rules.append(Rule(head_pats, body_pats, kind, term))

        # forward
        for s, _, o in list(g.triples((None, LOG.implies, None))):
            body_pats = _graph_to_pats(g.value(s, LOG.graph), g)
            head_pats = _graph_to_pats(g.value(o, LOG.graph), g)
            _add_rule(head_pats, body_pats, "forward")
            _scrub(s, g); _scrub(o, g)

        # backward
        for s, _, o in list(g.triples((None, LOG.isImpliedBy, None))):
            body_pats = _graph_to_pats(g.value(o, LOG.graph), g)
            head_pats = _graph_to_pats(g.value(s, LOG.graph), g)
            _add_rule(head_pats, body_pats, "backward")
            _scrub(s, g); _scrub(o, g)

        # answer
        for s, _, o in list(g.triples((None, LOG.impliesAnswer, None))):
            body_pats = _graph_to_pats(g.value(s, LOG.graph), g)
            head_pats = _graph_to_pats(g.value(o, LOG.graph), g)
            _add_rule(head_pats, body_pats, "answer")
            _scrub(s, g); _scrub(o, g)

# ─── helper functions (no state) ───────────────────────────────────
def _graph_to_pats(list_node, g):
    if list_node is None:
        return []
    out = []
    for tbn in Collection(g, list_node):
        inner = g.value(tbn, LOG.triple)
        if inner is None:
            continue
        items = list(Collection(g, inner))
        if len(items) >= 3:
            out.append(tuple(items[:3]))
    return out

def _scrub(node, g):
    todo = [node]
    while todo:
        n = todo.pop()
        for s, p, o in list(g.triples((n, None, None))):
            g.remove((s, p, o))
            if p in (LOG.graph, LOG.triple, RDF.first, RDF.rest):
                todo.append(o)

# ─── trace utilities ──────────────────────────────────────────────
def _triple_bnode(g, triple):
    bn = BNode()
    g.add((bn, LOG.triple, Collection(g, BNode(), list(triple)).uri))
    return bn

def _triples_to_list(g, triples):
    return Collection(g, BNode(),
                      [_triple_bnode(g, t) for t in triples]).uri

def _copy_trace_nodes(src: Graph, dst: Graph):
    q = [s for s, _, _ in src.triples((None, TRACE.viaRule, None))]
    dst += src.triples((None, TRACE.viaRule, None))
    seen: Set[BNode] = set(q)
    while q:
        n = q.pop()
        for s, p, o in src.triples((n, None, None)):
            dst.add((s, p, o))
            if isinstance(o, BNode) and o not in seen:
                seen.add(o); q.append(o)
            if p == LOG.triple and isinstance(o, BNode):
                _copy_list_structure(src, dst, o, q, seen)

def _copy_list_structure(src, dst, head, q, seen):
    node = head
    while node and node != RDF.nil and (node, RDF.first, None) in src:
        if node not in seen:
            seen.add(node); q.append(node)
        for t in src.triples((node, None, None)):
            dst.add(t)
            if isinstance(t[2], BNode) and t[2] not in seen:
                seen.add(t[2]); q.append(t[2])
        node = src.value(node, RDF.rest)

# ─── CLI ───────────────────────────────────────────────────────────
if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit("Usage: python eyelet.py [--trace] file.ttl [more.ttl …]")

    want_trace = "--trace" in sys.argv
    files = [f for f in sys.argv[1:] if f != "--trace"]

    R = Reasoner(trace=want_trace)
    R.load(*files)
    R.forward_chain()

    R.answers().serialize(sys.stdout.buffer, format="turtle")

