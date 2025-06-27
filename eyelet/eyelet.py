#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
eyelet.py – pocket-sized EYE-style reasoner (RDF-list aware)
============================================================

Forward   rules :  [ … ] log:implies      [ … ] .
Backward  rules :  [ … ] log:isImpliedBy  [ … ] .
Answer    rules :  [ … ] log:impliesAnswer [ … ] .

Add --trace to embed one provenance node per forward rule.

• Handles RDF lists in triples (e.g. :joe :is (:good :Cobbler)).
• Built-ins implemented (numeric literals): math:difference | math:greaterThan
"""

import sys
from typing import Dict, List, Optional, Sequence, Tuple, Union, Iterator, Set

from rdflib import Graph, BNode, URIRef, Literal, Namespace
from rdflib.collection import Collection
from rdflib.namespace import RDF, XSD

# ───── namespaces ────────────────────────────────────────────────
LOG   = Namespace("http://www.w3.org/2000/10/swap/log#")
MATH  = Namespace("http://www.w3.org/2000/10/swap/math#")
VAR   = Namespace("http://www.w3.org/2000/10/swap/var#")
TRACE = Namespace("http://example.org/trace#")

# a term can be an rdflib node *or* a tuple representing an RDF list
Term   = Union[URIRef, BNode, Literal, Tuple]
Triple = Tuple[Term, Term, Term]
Subst  = Dict[URIRef, Term]

# ───── list helpers ──────────────────────────────────────────────
def _to_python(node: Term, g: Graph, _memo=None) -> Term:
    """Convert RDF list heads to Python tuples (recursively)."""
    if _memo is None:
        _memo = {}
    if isinstance(node, (URIRef, Literal)):
        return node
    if isinstance(node, BNode):
        if node in _memo:
            return _memo[node]
        if (node, RDF.first, None) in g:
            els, cur = [], node
            while cur and cur != RDF.nil:
                els.append(_to_python(g.value(cur, RDF.first), g, _memo))
                cur = g.value(cur, RDF.rest)
            tpl = tuple(els)
            _memo[node] = tpl
            return tpl
    return node

def _from_python(term: Term, g: Graph) -> Term:
    """Convert a Python tuple back into an RDF list head."""
    if not isinstance(term, tuple):
        return term
    items = [_from_python(x, g) for x in term]     # recurse for nested tuples
    head  = BNode()
    Collection(g, head, items)                     # ← builds the whole list incl. rdf:nil
    return head

# ───── variable helpers & unification ────────────────────────────
def is_var(t) -> bool:
    return isinstance(t, URIRef) and str(t).startswith(str(VAR))

def subst_term(t: Term, σ: Subst):
    if isinstance(t, tuple):
        return tuple(subst_term(x, σ) for x in t)
    return σ.get(t, t) if is_var(t) else t

def _extend(v: URIRef, x: Term, σ: Subst) -> Optional[Subst]:
    if v in σ and σ[v] != x:
        return None
    σ2 = dict(σ); σ2[v] = x
    return σ2

def unify_term(a: Term, b: Term, σ: Subst) -> Optional[Subst]:
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
    for p, f in zip(pat, fact):
        σ = unify_term(p, f, σ)
        if σ is None:
            return None
    return σ

# ───── built-ins (numeric) ───────────────────────────────────────
def _num(lit):
    if isinstance(lit, Literal):
        try: return float(lit)
        except Exception: return None
    return None

def eval_builtin(pred, args, σ):
    if pred == MATH.difference:
        (pair,), F = args[:-1], args[-1]
        if not (isinstance(pair, tuple) and len(pair) == 2): return None
        a, b = map(lambda z: _num(subst_term(z, σ)), pair)
        return unify_term(F, Literal(a - b, datatype=XSD.decimal), σ) if a is not None and b is not None else None
    if pred == MATH.greaterThan:
        F, A = args
        f, a = _num(subst_term(F, σ)), _num(subst_term(A, σ))
        return σ if (f is not None and a is not None and f > a) else None
    return None

# ───── rule term constructor ─────────────────────────────────────
def _make_rule_term(g: Graph, body: List[Triple], head: List[Triple]):
    def _pat_list(pats):
        items=[]
        for s,p,o in pats:
            tbn=BNode()
            g.add((tbn, LOG.triple, Collection(g,BNode(),[_from_python(s,g),_from_python(p,g),_from_python(o,g)]).uri))
            items.append(tbn)
        return Collection(g,BNode(),items).uri
    body_l=_pat_list(body); head_l=_pat_list(head)
    b_bn=BNode(); g.add((b_bn, LOG.graph, body_l))
    h_bn=BNode(); g.add((h_bn, LOG.graph, head_l))
    rule_bn=BNode()
    g.add((rule_bn, LOG.triple, Collection(g,BNode(),[b_bn, LOG.implies, h_bn]).uri))
    return rule_bn

# ───── rule container ────────────────────────────────────────────
class Rule:
    def __init__(self, head, body, kind, term):
        self.head, self.body, self.kind, self.term = head, body, kind, term

# ───── reasoner ───────────────────────────────────────────────────
class Reasoner:
    def __init__(self, trace=False):
        self.data = Graph()
        self.rules: List[Rule] = []
        self.trace = trace
        if trace: self.data.bind("trace", TRACE)
        self._traced: Set[Rule] = set()

    # ─ load files ───────────────────────────────────────────────
    def load(self, *files):
        for p in files:
            g = Graph(); g.parse(p)
            for pfx, uri in g.namespace_manager.namespaces():
                self.data.bind(pfx or "", uri, replace=False)
            self._extract_rules(g)
            self.data += g                   # keep only fact triples

    # ─ forward chaining ─────────────────────────────────────────
    def forward_chain(self, limit=50):
        changed=True
        while changed and limit:
            changed,limit=False,limit-1
            for r in self.rules:
                if r.kind!="forward": continue
                for env in self._match_body(r.body):
                    new=[t for t in self._apply(r.head,env) if t not in self._iter_facts_matching_any()]
                    if not new: continue
                    for t in new: self._add_fact(t)
                    changed=True
                    if self.trace and r not in self._traced:
                        self._record_trace(r,env,new)

    def _iter_facts_matching_any(self):
        for s,p,o in self.data:
            yield tuple(_to_python(x,self.data) for x in (s,p,o))

    def _add_fact(self,t):
        s=_from_python(t[0],self.data) if isinstance(t[0],tuple) else t[0]
        p=_from_python(t[1],self.data) if isinstance(t[1],tuple) else t[1]
        o=_from_python(t[2],self.data) if isinstance(t[2],tuple) else t[2]
        self.data.add((s,p,o))

    def _record_trace(self,r,env,prod):
        self._traced.add(r)
        node=BNode()
        self.data.add((node, TRACE.viaRule, Collection(self.data,BNode(),[r.term]).uri))
        used=_triples_to_list(self.data,[self._subst(t,env) for t in r.body])
        self.data.add((node, TRACE.used, used))
        self.data.add((node, TRACE.produced, _triples_to_list(self.data,prod)))

    # ─ asking / answers ─────────────────────────────────────────
    def answers(self):
        g=Graph(); g.namespace_manager=self.data.namespace_manager
        if self.trace: g.bind("trace", TRACE)
        for r in self.rules:
            if r.kind!="answer": continue
            for env in self.ask(r.body):
                for t in self._apply(r.head, env):
                    g.add(_triple_to_rdflib(t,g))
        if self.trace: _copy_trace_nodes(self.data,g)
        return g

    def ask(self,goals): yield from self._prove(goals,{})

    # ─ proof search ────────────────────────────────────────────
    def _prove(self,goals,σ):
        if not goals: yield σ; return
        g0,*rest=goals
        g0=self._subst(g0,σ)
        if isinstance(g0[1],URIRef) and str(g0[1]).startswith(str(MATH)):
            σ2=eval_builtin(g0[1],(g0[0],g0[2]),σ)
            if σ2: yield from self._prove(rest,σ2)
            return
        for fact in self._iter_facts_matching(g0):
            σ2=unify_triple(g0,fact,σ)
            if σ2: yield from self._prove(rest,σ2)
        for r in self.rules:
            if r.kind!="backward": continue
            for h in r.head:
                σ2=unify_triple(g0,h,σ)
                if σ2 is None: continue
                new=([self._subst(t,σ2) for t in r.body]+
                     [self._subst(t,σ2) for t in r.head if t is not h]+rest)
                yield from self._prove(new,σ2)

    def _iter_facts_matching(self,pat):
        def conv(x): return None if (is_var(x) or isinstance(x,tuple)) else _from_python(x,self.data)
        for s,p,o in self.data.triples((conv(pat[0]),conv(pat[1]),conv(pat[2]))):
            yield tuple(_to_python(x,self.data) for x in (s,p,o))

    # ─ body matcher ────────────────────────────────────────────
    def _match_body(self,body):
        envs=[{}]
        for pat in body:
            nxt=[]
            for σ in envs:
                gpat=self._subst(pat,σ)
                if isinstance(gpat[1],URIRef) and str(gpat[1]).startswith(str(MATH)):
                    res=eval_builtin(gpat[1],(gpat[0],gpat[2]),σ)
                    if res: nxt.append(res)
                    continue
                for fact in self._iter_facts_matching(gpat):
                    σ2=unify_triple(gpat,fact,σ)
                    if σ2: nxt.append(σ2)
            envs=nxt
            if not envs: break
        yield from envs

    # ─ helpers ────────────────────────────────────────────────
    def _subst(self,t,σ): return tuple(subst_term(x,σ) for x in t)
    def _apply(self,ts,σ): yield from (self._subst(t,σ) for t in ts)

    # ─ rule extraction ─────────────────────────────────────────
    def _extract_rules(self,g):
        def _add(head,body,kind):
            self.rules.append(Rule(head,body,kind,_make_rule_term(self.data,body,head)))
        for s,_,o in list(g.triples((None,LOG.implies,None))):
            body=_graph_to_pats(g.value(s,LOG.graph),g)
            head=_graph_to_pats(g.value(o,LOG.graph),g)
            _add(head,body,"forward"); _scrub(s,g); _scrub(o,g)
        for s,_,o in list(g.triples((None,LOG.isImpliedBy,None))):
            body=_graph_to_pats(g.value(o,LOG.graph),g)
            head=_graph_to_pats(g.value(s,LOG.graph),g)
            _add(head,body,"backward"); _scrub(s,g); _scrub(o,g)
        for s,_,o in list(g.triples((None,LOG.impliesAnswer,None))):
            body=_graph_to_pats(g.value(s,LOG.graph),g)
            head=_graph_to_pats(g.value(o,LOG.graph),g)
            _add(head,body,"answer"); _scrub(s,g); _scrub(o,g)

# ───── misc helpers ───────────────────────────────────────────────
def _graph_to_pats(list_node,g):
    if list_node is None: return []
    pats=[]
    for tbn in Collection(g,list_node):
        inner=g.value(tbn,LOG.triple)
        if inner is None: continue
        parts=list(Collection(g,inner))
        if len(parts)>=3:
            pats.append(tuple(_to_python(x,g) for x in parts[:3]))
    return pats

def _scrub(n,g):
    todo=[n]
    while todo:
        u=todo.pop()
        for s,p,o in list(g.triples((u,None,None))):
            g.remove((s,p,o))
            if p in (LOG.graph,LOG.triple,RDF.first,RDF.rest):
                todo.append(o)

def _triple_bnode(g,tr):
    bn=BNode(); g.add((bn,LOG.triple, Collection(g,BNode(),[_from_python(x,g) for x in tr]).uri)); return bn

def _triples_to_list(g,tris):
    return Collection(g,BNode(),[_triple_bnode(g,t) for t in tris]).uri

def _triple_to_rdflib(tr,g):
    return tuple(_from_python(x,g) if isinstance(x,tuple) else x for x in tr)

def _copy_trace_nodes(src,dst):
    q=[s for s,_,_ in src.triples((None,TRACE.viaRule,None))]
    dst += src.triples((None,TRACE.viaRule,None))
    seen=set(q)
    while q:
        n=q.pop()
        for s,p,o in src.triples((n,None,None)):
            dst.add((s,p,o))
            if isinstance(o,BNode) and o not in seen:
                seen.add(o); q.append(o)
            if p==LOG.triple and isinstance(o,BNode):
                _copy_list_structure(src,dst,o,q,seen)

def _copy_list_structure(src,dst,head,q,seen):
    node=head
    while node and node!=RDF.nil and (node,RDF.first,None) in src:
        if node not in seen: seen.add(node); q.append(node)
        for t in src.triples((node,None,None)):
            dst.add(t)
            if isinstance(t[2],BNode) and t[2] not in seen:
                seen.add(t[2]); q.append(t[2])
        node=src.value(node,RDF.rest)

# ───── CLI ───────────────────────────────────────────────────────
if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit("Usage: python eyelet.py [--trace] file.ttl [more.ttl …]")
    want_trace="--trace" in sys.argv
    files=[f for f in sys.argv[1:] if f!="--trace"]
    R=Reasoner(trace=want_trace); R.load(*files); R.forward_chain()
    R.answers().serialize(sys.stdout.buffer, format="turtle")

