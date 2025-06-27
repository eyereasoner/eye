#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
eyelet.py – pocket-sized EYE-style reasoner
===========================================

Forward   rules :  [ … ] log:implies       [ … ] .
Backward  rules :  [ … ] log:isImpliedBy   [ … ] .
Answer    rules :  [ … ] log:impliesAnswer [ … ] .

Add --trace to embed one provenance node per forward rule.

• Handles RDF lists in triples (e.g. :joe :is (:good :Cobbler)).
• Built-ins implemented:
      – math:difference   | math:greaterThan
      – time:localTime
"""

import sys, re
from datetime import datetime, date, time as _time, timedelta
from typing   import Dict, List, Optional, Tuple, Union, Set

from rdflib            import Graph, BNode, URIRef, Literal, Namespace
from rdflib.collection import Collection
from rdflib.namespace  import RDF, XSD

# ───── namespaces ────────────────────────────────────────────────
LOG   = Namespace("http://www.w3.org/2000/10/swap/log#")
MATH  = Namespace("http://www.w3.org/2000/10/swap/math#")
TIME  = Namespace("http://www.w3.org/2000/10/swap/time#")
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
    head = BNode()
    Collection(g, head, [_from_python(x, g) for x in term])
    return head

# ───── variable helpers & unification ────────────────────────────
def is_var(t) -> bool:
    return isinstance(t, URIRef) and str(t).startswith(str(VAR))

def subst_term(t: Term, σ: Subst):
    if isinstance(t, tuple):
        return tuple(subst_term(x, σ) for x in t)
    return σ.get(t, t) if is_var(t) else t

def _extend(v: URIRef, x: Term, σ: Subst):
    if v in σ and σ[v] != x:
        return None
    σ2 = dict(σ); σ2[v] = x
    return σ2

def unify_term(a: Term, b: Term, σ: Subst):
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

def unify_triple(pat: Triple, fact: Triple, σ: Subst):
    for p, f in zip(pat, fact):
        σ = unify_term(p, f, σ)
        if σ is None:
            return None
    return σ

# ───── literal helpers ───────────────────────────────────────────
_DUR_RE = re.compile(
    r"^P(?:(?P<years>\d+)Y)?(?:(?P<months>\d+)M)?(?:(?P<days>\d+)D)?"
    r"(?:T(?:(?P<hours>\d+)H)?(?:(?P<minutes>\d+)M)?(?:(?P<seconds>\d+)S)?)?$"
)

def _duration_to_days(lit: Literal) -> Optional[float]:
    """Roughly convert xsd:duration → decimal days (365 d/y, 30 d/M)."""
    if not (isinstance(lit, Literal) and lit.datatype == XSD.duration):
        return None
    m = _DUR_RE.match(str(lit))
    if not m:
        return None
    p = {k: int(v) if v else 0 for k, v in m.groupdict().items()}
    days = p["years"] * 365 + p["months"] * 30 + p["days"]
    secs = p["hours"] * 3600 + p["minutes"] * 60 + p["seconds"]
    return days + secs / 86400.0

def _num(lit: Literal):
    """Literal → float (xsd:decimal/int or xsd:duration)."""
    if not isinstance(lit, Literal):
        return None
    if lit.datatype == XSD.duration:
        return _duration_to_days(lit)
    try:
        return float(lit)
    except Exception:
        return None

def _iso_to_py(lit: Literal):
    """Parse xsd:dateTime/date/time → Python dt/date/time."""
    if not isinstance(lit, Literal):
        return None
    if lit.datatype == XSD.dateTime:
        try: return datetime.fromisoformat(str(lit))
        except ValueError: return None
    if lit.datatype == XSD.date:
        try: return date.fromisoformat(str(lit))
        except ValueError: return None
    if lit.datatype == XSD.time:
        try: return _time.fromisoformat(str(lit))
        except ValueError: return None
    return None

def _td_to_dec(td: timedelta):
    return Literal(td.total_seconds() / 86400.0, datatype=XSD.decimal)

def _align_dt_pair(a_dt, b_dt):
    """Glue mismatched date/time objects so subtraction works."""
    if isinstance(a_dt, _time) and isinstance(b_dt, _time):
        today = date.today()
        return (datetime.combine(today, a_dt),
                datetime.combine(today, b_dt))
    if isinstance(a_dt, _time) and isinstance(b_dt, (date, datetime)):
        a_dt = datetime.combine(b_dt.date() if isinstance(b_dt, datetime) else b_dt, a_dt)
    if isinstance(b_dt, _time) and isinstance(a_dt, (date, datetime)):
        b_dt = datetime.combine(a_dt.date() if isinstance(a_dt, datetime) else a_dt, b_dt)
    if isinstance(a_dt, date) and not isinstance(a_dt, datetime):
        a_dt = datetime.combine(a_dt, _time(0,0))
    if isinstance(b_dt, date) and not isinstance(b_dt, datetime):
        b_dt = datetime.combine(b_dt, _time(0,0))
    return a_dt, b_dt

# ───── built-ins ────────────────────────────────────────────────
def eval_builtin(pred: URIRef, args: Tuple, σ: Subst):
    # math:difference
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
            a_dt, b_dt = _align_dt_pair(a_dt, b_dt)
            return unify_term(F, _td_to_dec(a_dt - b_dt), σ)
        return None

    # math:greaterThan
    if pred == MATH.greaterThan:
        F, A = args
        f, a = _num(subst_term(F, σ)), _num(subst_term(A, σ))
        return σ if (f is not None and a is not None and f > a) else None

    # time:localTime
    if pred == TIME.localTime:
        _subj, obj = args
        now = datetime.now().isoformat(timespec="seconds")
        return unify_term(obj, Literal(now, datatype=XSD.dateTime), σ)

    return None

# ───── rule term constructor (unchanged) ─────────────────────────
def _make_rule_term(g: Graph, body: List[Triple], head: List[Triple]):
    def _pat_list(pats):
        items=[]
        for s,p,o in pats:
            tbn=BNode()
            g.add((tbn, LOG.triple, Collection(
                   g, BNode(),
                   [_from_python(s,g), _from_python(p,g), _from_python(o,g)]
                 ).uri))
            items.append(tbn)
        return Collection(g, BNode(), items).uri
    body_l=_pat_list(body); head_l=_pat_list(head)
    b_bn=BNode(); g.add((b_bn, LOG.graph, body_l))
    h_bn=BNode(); g.add((h_bn, LOG.graph, head_l))
    rule_bn=BNode()
    g.add((rule_bn, LOG.triple,
           Collection(g, BNode(), [b_bn, LOG.implies, h_bn]).uri))
    return rule_bn

# ───── rule container ────────────────────────────────────────────
class Rule:
    def __init__(self, head, body, kind, term):
        self.head, self.body, self.kind, self.term = head, body, kind, term

# ───── reasoner ──────────────────────────────────────────────────
class Reasoner:
    def __init__(self, trace=False):
        self.data  = Graph()
        self.rules: List[Rule] = []
        self.trace = trace
        if trace:
            self.data.bind("trace", TRACE)
        self._traced: Set[Rule] = set()

    # ─── deterministic helpers (only if trace) ──────────────────
    def _sort(self, triples):
        if not self.trace:
            return triples
        return sorted(triples, key=lambda t: (str(t[0]), str(t[1]), str(t[2])))

    # ─ load files ───────────────────────────────────────────────
    def load(self, *files):
        for p in files:
            g = Graph(); g.parse(p)
            for pfx, uri in g.namespace_manager.namespaces():
                self.data.bind(pfx or "", uri, replace=False)
            self._extract_rules(g)
            self.data += g     # keep only fact triples

    # ─ forward chaining ─────────────────────────────────────────
    def forward_chain(self, limit=50):
        changed = True
        while changed and limit:
            changed, limit = False, limit - 1
            for r in self.rules:          # rule list is already stable
                if r.kind != "forward": continue
                for env in self._match_body(r.body):
                    new = [t for t in self._apply(r.head, env)
                           if t not in self._iter_facts_any()]
                    if not new: continue
                    for t in self._sort(new):
                        self._add_fact(t)
                    changed = True
                    if self.trace and r not in self._traced:
                        self._record_trace(r, env, new)

    # fast iterable of all facts
    def _iter_facts_any(self):
        src = self.data if not self.trace else self._sort(self.data)
        for s,p,o in src:
            yield tuple(_to_python(x, self.data) for x in (s,p,o))

    def _add_fact(self, t):
        s = _from_python(t[0], self.data) if isinstance(t[0], tuple) else t[0]
        p = _from_python(t[1], self.data) if isinstance(t[1], tuple) else t[1]
        o = _from_python(t[2], self.data) if isinstance(t[2], tuple) else t[2]
        self.data.add((s, p, o))

    def _record_trace(self, r, env, prod):
        self._traced.add(r)
        node = BNode()
        self.data.add((node, TRACE.viaRule,
                       Collection(self.data, BNode(), [r.term]).uri))
        used = _triples_to_list(
            self.data, [self._subst(t, env) for t in r.body])
        self.data.add((node, TRACE.used, used))
        self.data.add((node, TRACE.produced,
                       _triples_to_list(self.data, prod)))

    # ─ ask / answers ───────────────────────────────────────────
    def answers(self):
        g = Graph(); g.namespace_manager = self.data.namespace_manager
        if self.trace: g.bind("trace", TRACE)
        for r in self.rules:
            if r.kind != "answer": continue
            for env in self.ask(r.body):
                for t in self._apply(r.head, env):
                    g.add(_triple_to_rdflib(t, g))
        if self.trace: _copy_trace_nodes(self.data, g)
        return g

    def ask(self, goals):
        yield from self._prove(goals, {})

    # ─ depth-first proof search ────────────────────────────────
    def _prove(self, goals, σ):
        if not goals: yield σ; return
        g0, *rest = goals
        g0 = self._subst(g0, σ)
        if (isinstance(g0[1], URIRef) and
            (str(g0[1]).startswith(str(MATH)) or
             str(g0[1]).startswith(str(TIME)))):
            σ2 = eval_builtin(g0[1], (g0[0], g0[2]), σ)
            if σ2: yield from self._prove(rest, σ2)
            return
        for fact in self._iter_facts(g0):
            σ2 = unify_triple(g0, fact, σ)
            if σ2: yield from self._prove(rest, σ2)
        for r in self.rules:
            if r.kind != "backward": continue
            for h in r.head:
                σ2 = unify_triple(g0, h, σ)
                if σ2 is None: continue
                new = ([self._subst(t, σ2) for t in r.body] +
                       [self._subst(t, σ2) for t in r.head if t is not h] +
                       rest)
                yield from self._prove(new, σ2)

    def _iter_facts(self, pat):
        def conv(x): return None if (is_var(x) or isinstance(x, tuple)) \
                               else _from_python(x, self.data)
        triples = self.data.triples((conv(pat[0]), conv(pat[1]), conv(pat[2])))
        triples = self._sort(triples)
        for s,p,o in triples:
            yield tuple(_to_python(x, self.data) for x in (s,p,o))

    # ─ body matcher ────────────────────────────────────────────
    def _match_body(self, body):
        envs=[{}]
        for pat in body:
            nxt=[]
            for σ in envs:
                gpat = self._subst(pat, σ)
                if (isinstance(gpat[1], URIRef) and
                    (str(gpat[1]).startswith(str(MATH)) or
                     str(gpat[1]).startswith(str(TIME)))):
                    res = eval_builtin(gpat[1], (gpat[0], gpat[2]), σ)
                    if res: nxt.append(res)
                    continue
                for fact in self._iter_facts(gpat):
                    σ2 = unify_triple(gpat, fact, σ)
                    if σ2: nxt.append(σ2)
            envs = nxt
            if not envs: break
        yield from envs

    # ─ helpers ────────────────────────────────────────────────
    def _subst(self, t, σ): return tuple(subst_term(x, σ) for x in t)
    def _apply(self, ts, σ): yield from (self._subst(t, σ) for t in ts)

    # ─ rule extraction ─────────────────────────────────────────
    def _extract_rules(self, g):
        def _add(head, body, kind):
            self.rules.append(
                Rule(head, body, kind,
                     _make_rule_term(self.data, body, head)))
        for s,_,o in list(g.triples((None, LOG.implies, None))):
            body=_graph_to_pats(g.value(s, LOG.graph), g)
            head=_graph_to_pats(g.value(o, LOG.graph), g)
            _add(head, body, "forward");  _scrub(s,g); _scrub(o,g)
        for s,_,o in list(g.triples((None, LOG.isImpliedBy, None))):
            body=_graph_to_pats(g.value(o, LOG.graph), g)
            head=_graph_to_pats(g.value(s, LOG.graph), g)
            _add(head, body, "backward"); _scrub(s,g); _scrub(o,g)
        for s,_,o in list(g.triples((None, LOG.impliesAnswer, None))):
            body=_graph_to_pats(g.value(s, LOG.graph), g)
            head=_graph_to_pats(g.value(o, LOG.graph), g)
            _add(head, body, "answer");   _scrub(s,g); _scrub(o,g)

# ───── misc helpers (deterministic in trace) ─────────────────────
def _graph_to_pats(list_node, g):
    if list_node is None: return []
    pats=[]
    for tbn in Collection(g, list_node):
        inner=g.value(tbn, LOG.triple)
        if inner is None: continue
        parts=list(Collection(g, inner))
        if len(parts)>=3:
            pats.append(tuple(_to_python(x,g) for x in parts[:3]))
    return pats

def _scrub(n,g):
    todo=[n]
    while todo:
        u=todo.pop()
        for s,p,o in list(g.triples((u,None,None))):
            g.remove((s,p,o))
            if p in (LOG.graph, LOG.triple, RDF.first, RDF.rest):
                todo.append(o)

def _triple_bnode(g,tr):
    bn=BNode()
    g.add((bn, LOG.triple,
           Collection(g,BNode(),
                      [_from_python(x,g) for x in tr]).uri))
    return bn

def _triples_to_list(g,tris):
    return Collection(g, BNode(),
                      [_triple_bnode(g,t) for t in tris]).uri

def _triple_to_rdflib(tr,g):
    return tuple(_from_python(x,g) if isinstance(x,tuple) else x for x in tr)

def _copy_trace_nodes(src,dst):
    q=sorted([s for s,_,_ in src.triples((None, TRACE.viaRule, None))],
             key=str)
    dst += src.triples((None, TRACE.viaRule, None))
    seen=set(q)
    while q:
        n=q.pop(0)
        for s,p,o in sorted(src.triples((n,None,None)),
                            key=lambda t:(str(t[0]),str(t[1]),str(t[2]))):
            dst.add((s,p,o))
            if isinstance(o,BNode) and o not in seen:
                seen.add(o); q.append(o)
            if p==LOG.triple and isinstance(o,BNode):
                _copy_list_structure(src,dst,o,q,seen)

def _copy_list_structure(src,dst,head,q,seen):
    node=head
    while node and node!=RDF.nil and (node,RDF.first,None) in src:
        if node not in seen:
            seen.add(node); q.append(node)
        for t in sorted(src.triples((node,None,None)),
                        key=lambda t:(str(t[0]),str(t[1]),str(t[2]))):
            dst.add(t)
            if isinstance(t[2],BNode) and t[2] not in seen:
                seen.add(t[2]); q.append(t[2])
        node=src.value(node,RDF.rest)

# ───── CLI ───────────────────────────────────────────────────────
if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit("Usage: python eyelet.py [--trace] file.ttl [more.ttl …]")
    want_trace = "--trace" in sys.argv
    files      = [f for f in sys.argv[1:] if f != "--trace"]
    R = Reasoner(trace=want_trace)
    R.load(*files)
    R.forward_chain()
    R.answers().serialize(sys.stdout.buffer, format="turtle")

