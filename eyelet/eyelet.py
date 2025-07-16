#!/usr/bin/env python3
"""
eyelet.py – minimal resolution prover
• Variant filtering
• Breadth‑first SOS loop
"""

import re, sys, pathlib
from typing import List, Dict, Tuple

# ─── simple Peekable ─────────────────────────────────────────
class Peekable:
    def __init__(self, iterable):
        self.it = iter(iterable)
        self.buf = []
    def __iter__(self): return self
    def __next__(self):
        if self.buf: return self.buf.pop()
        return next(self.it)
    def peek(self):
        try:
            tok = self.__next__()
            self.buf.append(tok)
            return tok
        except StopIteration:
            return None

# ─── lexer ──────────────────────────────────────────────────
VAR   = re.compile(r"^[a-z][A-Za-z0-9_]*$")
TOKRE = re.compile(r"[A-Za-z0-9_]+|[(),]")
def tokenize(s):  # generator
    for m in TOKRE.finditer(s):
        yield m.group(0)

# ─── data types ─────────────────────────────────────────────
class Term:
    def __init__(self, f: str, args=None):
        self.f = f
        self.args = tuple(args or ())
    def is_var(self): return VAR.match(self.f) and not self.args
    def __repr__(self): return self.f if not self.args else f"{self.f}({', '.join(map(repr,self.args))})"
    def __hash__(self): return hash((self.f, self.args))
    def __eq__(self, o): return isinstance(o, Term) and (self.f, self.args) == (o.f, o.args)

class Literal:
    def __init__(self, p: str, args, neg=False):
        self.pred, self.args, self.neg = p, tuple(args), neg
    def negate(self): return Literal(self.pred, self.args, not self.neg)
    def substitute(self, theta):
        return Literal(self.pred, [substitute(a, theta) for a in self.args], self.neg)
    def __repr__(self): return ("¬" if self.neg else "") + f"{self.pred}({', '.join(map(repr, self.args))})"
    def __hash__(self): return hash((self.pred, self.args, self.neg))
    def __eq__(self, o): return isinstance(o, Literal) and (self.pred, self.args, self.neg) == (o.pred, o.args, o.neg)

Clause = frozenset

# ─── parser ─────────────────────────────────────────────────
def parse_term(tokens: Peekable) -> Term:
    f = next(tokens)
    if tokens.peek() != '(':
        return Term(f)
    next(tokens)                      # '('
    args = []
    if tokens.peek() != ')':
        while True:
            args.append(parse_term(tokens))
            if tokens.peek() == ',':
                next(tokens)
                continue
            break
    next(tokens)                      # ')'
    return Term(f, args)

def parse_literal(text: str) -> Literal:
    text = text.strip()
    neg = text.startswith(('¬', '~'))
    if neg: text = text[1:].strip()
    head, tail = text.split('(', 1)
    if not tail.endswith(')'): raise ValueError("literal lacks ')'")
    tok_iter = Peekable(tokenize(tail[:-1]))
    args = []
    if tok_iter.peek() is not None:
        while True:
            args.append(parse_term(tok_iter))
            if tok_iter.peek() == ',':
                next(tok_iter)
                continue
            break
    return Literal(head.strip(), args, neg)

def parse_clause(line: str, *, lineno: int):
    try:
        return Clause(parse_literal(p) for p in line.split('|'))
    except ValueError as e:
        raise ValueError(f"{e} (line {lineno})") from None

# ─── unification ────────────────────────────────────────────
def substitute(t: Term, θ):
    if t.is_var():
        while t.is_var() and t in θ:
            t = θ[t]
        return t
    if t.args:
        return Term(t.f, [substitute(a, θ) for a in t.args])
    return t

def unify(x: Term, y: Term, θ=None):
    θ = θ or {}
    x, y = substitute(x, θ), substitute(y, θ)
    if x == y: return θ
    if x.is_var():
        θ[x] = y
        return θ
    if y.is_var():
        θ[y] = x
        return θ
    if x.f != y.f or len(x.args) != len(y.args): return None
    for xi, yi in zip(x.args, y.args):
        θ = unify(xi, yi, θ)
        if θ is None: return None
    return θ

def unify_tuple(a1, a2):
    θ = {}
    for s, t in zip(a1, a2):
        θ = unify(s, t, θ)
        if θ is None: return None
    return θ

# ─── variant filter ────────────────────────────────────────
def canonical(cl: Clause):
    vmap = {}
    def norm(t):
        if t.is_var():
            vmap.setdefault(t.f, f"_v{len(vmap)}")
            return vmap[t.f]
        return (t.f, tuple(norm(a) for a in t.args))
    lits = []
    for L in sorted(cl, key=repr):
        tag = ("¬" if L.neg else "") + L.pred
        lits.append((tag, tuple(norm(a) for a in L.args)))
    return tuple(lits)

# ─── resolution ────────────────────────────────────────────
def resolve(Ci, Cj):
    for Li in Ci:
        for Lj in Cj:
            if Li.pred == Lj.pred and Li.neg != Lj.neg:
                θ = unify_tuple(Li.args, Lj.args)
                if θ is None: continue
                R = frozenset((Ci | Cj) - {Li, Lj})
                R = frozenset(L.substitute(θ) for L in R)
                if any(L.negate() in R for L in R): continue
                yield R

# ─── prover (breadth‑first SOS) ────────────────────────────
def clause_str(c): return "⊥" if not c else " | ".join(sorted(map(repr, c)))

def prove(kb: List[Clause], neg_goal: Clause):
    sos = [neg_goal]
    all_clauses = set(kb) | {neg_goal}
    seen_keys = {canonical(c) for c in all_clauses}
    step = 0
    while sos:
        Ci = sos.pop(0)
        for Cj in sorted(all_clauses, key=repr):
            for R in resolve(Ci, Cj):
                key = canonical(R)
                if key in seen_keys: continue
                seen_keys.add(key)
                step += 1
                print(f"{step:02d}. {clause_str(R)}   (from {clause_str(Ci)} , {clause_str(Cj)})")
                if not R:
                    print("\nEmpty clause derived — goal is entailed. 🎉")
                    return
                sos.append(R)
                all_clauses.add(R)
    print("Proof failed — goal not entailed.")

# ─── loader & main ────────────────────────────────────────
def load_kb(path):
    kb, goal = [], None
    with open(path, encoding="utf-8") as fh:
        for n, raw in enumerate(fh, 1):
            line = raw.partition("#")[0].strip()
            if not line: continue
            if line.upper().startswith("GOAL:"):
                goal = line.split(":", 1)[1].strip()
            else:
                kb.append(parse_clause(line, lineno=n))
    if goal is None: raise ValueError("KB missing GOAL line")
    return kb, goal

if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit("usage: python eyelet.py <kb-file>")
    kbfile = sys.argv[1]
    if not pathlib.Path(kbfile).exists():
        sys.exit("KB file not found")
    KB, goal = load_kb(kbfile)
    print(f"Knowledge base loaded from '{kbfile}'.  Goal: {goal}\n")
    prove(KB, parse_clause(f"¬{goal}", lineno=0))

