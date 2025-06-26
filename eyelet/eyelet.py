#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
eyelet.py – a tiny EYE–style reasoner
=====================================

 • forward  rules  – log:implies
 • backward rules  – log:isImpliedBy
 • answer   rules  – log:impliesAnswer
   (CLI prints only the instantiated heads of answer rules)

Supported built-ins (numeric literals only):
 • math:difference
 • math:greaterThan
"""

import sys
from pathlib import Path
from typing import Dict, Iterator, List, Optional, Sequence, Tuple, Union

from rdflib import Graph, BNode, URIRef, Literal, Namespace
from rdflib.collection import Collection
from rdflib.namespace import RDF, XSD

# ───── namespaces ────────────────────────────────────────────────────
LOG  = Namespace("http://www.w3.org/2000/10/swap/log#")
MATH = Namespace("http://www.w3.org/2000/10/swap/math#")
VAR  = Namespace("http://www.w3.org/2000/10/swap/var#")

Triple = Tuple[Union[URIRef, BNode, Literal],
               Union[URIRef, BNode],
               Union[URIRef, BNode, Literal]]
Subst  = Dict[URIRef, Union[URIRef, BNode, Literal]]

# ───── variable helpers & unification ────────────────────────────────
def is_var(t) -> bool:
    return isinstance(t, URIRef) and str(t).startswith(str(VAR))

def subst_term(t, σ: Subst):
    return σ.get(t, t) if is_var(t) else t

def unify_term(a, b, σ: Subst) -> Optional[Subst]:
    a, b = subst_term(a, σ), subst_term(b, σ)
    if a == b:
        return σ
    if is_var(a):
        return _extend(a, b, σ)
    if is_var(b):
        return _extend(b, a, σ)
    return None

def _extend(v: URIRef, x, σ: Subst) -> Optional[Subst]:
    if v in σ and σ[v] != x:
        return None
    σ2 = dict(σ); σ2[v] = x
    return σ2

def unify_triple(pat: Triple, fact: Triple, σ: Subst) -> Optional[Subst]:
    for p, f in zip(pat, fact):
        σ = unify_term(p, f, σ)
        if σ is None:
            return None
    return σ

# ───── mini built-ins ────────────────────────────────────────────────
def _num(node) -> Optional[float]:
    if isinstance(node, Literal):
        try:
            return float(node)
        except Exception:
            return None
    return None

def eval_builtin(pred: URIRef, args: Tuple, σ: Subst) -> Optional[Subst]:
    if pred == MATH.difference:                      # ((A B) math:difference F)
        (pair,), F = args[:-1], args[-1]
        if not (isinstance(pair, tuple) and len(pair) == 2):
            return None
        a, b = map(lambda z: _num(subst_term(z, σ)), pair)
        if a is None or b is None:
            return None
        return unify_term(F, Literal(a - b, datatype=XSD.decimal), σ)

    if pred == MATH.greaterThan:                     # (F math:greaterThan A)
        F, A = args
        f, a = _num(subst_term(F, σ)), _num(subst_term(A, σ))
        if f is None or a is None:
            return None
        return σ if f > a else None

    return None                                      # unsupported

# ───── rule container ────────────────────────────────────────────────
class Rule:
    def __init__(self, head: List[Triple], body: List[Triple], kind: str):
        self.head, self.body, self.kind = head, body, kind   # forward | backward | answer

# ───── reasoner core ────────────────────────────────────────────────
class Reasoner:
    def __init__(self):
        self.data  = Graph()
        self.rules: List[Rule] = []

    # ── I/O ─────────────────────────────────────────────────────────
    def load(self, *files: Union[str, Path]):
        for p in files:
            g = Graph(); g.parse(p)
            self._extract_rules(g)
            self.data += g                                   # keep only facts

    # ── forward chaining ───────────────────────────────────────────
    def forward_chain(self, limit: int = 50):
        changed = True
        while changed and limit:
            changed, limit = False, limit - 1
            for r in self.rules:
                if r.kind != "forward":
                    continue
                for env in self._match_body(r.body):
                    for t in self._apply(r.head, env):
                        if t not in self.data:
                            self.data.add(t)
                            changed = True

    # ── backward chaining / ask ────────────────────────────────────
    def ask(self, goals: Sequence[Triple]) -> Iterator[Subst]:
        yield from self._prove(goals, {})

    def answers(self) -> Graph:
        out = Graph()
        for r in self.rules:
            if r.kind != "answer":
                continue
            for env in self.ask(r.body):
                for t in self._apply(r.head, env):
                    out.add(t)
        return out

    # ── proof search (depth-first) ────────────────────────────────
    def _prove(self, goals: Sequence[Triple], σ: Subst) -> Iterator[Subst]:
        if not goals:
            yield σ
            return

        g0, *rest = goals
        g0 = self._subst(g0, σ)

        # built-in as goal
        if isinstance(g0[1], URIRef) and str(g0[1]).startswith(str(MATH)):
            σ2 = eval_builtin(g0[1], (g0[0], g0[2]), σ)
            if σ2 is not None:
                yield from self._prove(rest, σ2)
            return                                   # no facts/rules needed

        # explicit facts
        for fact in self.data.triples(self._rdflib_pat(g0)):
            σ2 = unify_triple(g0, fact, σ)
            if σ2 is not None:
                yield from self._prove(rest, σ2)

        # backward rules
        for r in self.rules:
            if r.kind != "backward":
                continue
            for h in r.head:
                σ2 = unify_triple(g0, h, σ)
                if σ2 is None:
                    continue
                new_goals = [self._subst(p, σ2) for p in r.body] + \
                            [self._subst(p, σ2) for p in r.head if p is not h] + rest
                yield from self._prove(new_goals, σ2)

    # ── body matcher (forward rules) ───────────────────────────────
    def _match_body(self, body: List[Triple]) -> Iterator[Subst]:
        envs: List[Subst] = [{}]
        for pat in body:
            nxt: List[Subst] = []
            for σ in envs:
                gpat = self._subst(pat, σ)

                # built-in in body
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

    # ── tiny helpers ───────────────────────────────────────────────
    def _subst(self, t: Triple, σ: Subst) -> Triple:
        return tuple(subst_term(x, σ) for x in t)    # type: ignore

    def _apply(self, triples: List[Triple], σ: Subst):
        for t in triples:
            yield self._subst(t, σ)

    @staticmethod
    def _rdflib_pat(p: Triple):
        return tuple(None if is_var(x) else x for x in p)

    # ── rule extraction & cleanup ──────────────────────────────────
    def _extract_rules(self, g: Graph):
        # forward
        for a, _, b in list(g.triples((None, LOG.implies, None))):
            self._store_rule(a, b, "forward", g)
        # backward  (*** body = object, head = subject ***)
        for a, _, b in list(g.triples((None, LOG.isImpliedBy, None))):
            self._store_rule(b, a, "backward", g)   # <-- order fixed here
        # answer
        for a, _, b in list(g.triples((None, LOG.impliesAnswer, None))):
            self._store_rule(a, b, "answer", g)

    def _store_rule(self, subj, obj, kind, g: Graph):
        body = _graph_to_patterns(g.value(subj, LOG.graph), g)
        head = _graph_to_patterns(g.value(obj,  LOG.graph), g)
        self.rules.append(Rule(head, body, kind))
        _scrub(subj, g); _scrub(obj, g)

# ───── helpers (no reasoner state) ────────────────────────────────────
def _graph_to_patterns(list_node, g: Graph) -> List[Triple]:
    if list_node is None:
        return []
    patterns: List[Triple] = []
    for triple_node in Collection(g, list_node):
        triple_list = g.value(triple_node, LOG.triple)
        if triple_list is None:
            continue
        parts = list(Collection(g, triple_list))
        if len(parts) < 3:
            continue
        s, p, o = parts[:3]
        patterns.append((s, p, o))
    return patterns

def _scrub(node, g: Graph):
    """Remove rule-description triples from the graph."""
    todo = [node]
    while todo:
        n = todo.pop()
        for s, p, o in list(g.triples((n, None, None))):
            g.remove((s, p, o))
            if p in (RDF.first, RDF.rest, LOG.graph, LOG.triple):
                todo.append(o)

# ───── CLI ──────────────────────────────────────────────────────────
if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit("Usage: python eyelet.py file.ttl [more.ttl …]")

    r = Reasoner()
    r.load(*sys.argv[1:])
    r.forward_chain()

    answers = r.answers()
    (answers if len(answers) else r.data).serialize(
        sys.stdout.buffer, format="turtle"
    )

