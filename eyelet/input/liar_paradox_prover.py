#!/usr/bin/env python3
"""
liar_paradox_prover.py
──────────────────────
Deterministic first‑order resolution proof of a “Liar” paradox.

Predicate  T(x)    ≝  “the statement x is true”
Constant   Liar    ≝  the self‑referential statement “This statement is false”

We encode the (inconsistent) semantics of *Liar* with two universal rules:

1.  (∀x)  [ ¬T(x) → T(Liar) ]          ⟹   T(x)  ∨  T(Liar)
2.  (∀x)  [  T(Liar) → ¬T(x) ]         ⟹   ¬T(Liar)  ∨  ¬T(x)

Intuitively, rule 1 says: “if a statement isn’t true, then *Liar* is true”;
rule 2 says: “if *Liar* is true, no statement is true.”

Goal to prove: **T(Liar)**  
We refute its negation ¬T(Liar).

Deterministic proof (always identical):

01. T(Liar)                 (from ¬T(Liar) , T(x) | T(Liar))
02. ⊥                       (from T(Liar) , ¬T(Liar))

Empty clause derived – contradiction established.
"""

import re, itertools

# ─────────────────────  Tiny term & literal classes  ────────────────────────
VAR_RE = re.compile(r"^[a-z][A-Za-z0-9_]*$")          # variables start lower‑case


class Term:
    __slots__ = ("functor", "args")
    def __init__(self, functor, args=None):
        self.functor = functor
        self.args = tuple(args) if args else tuple()
    def is_var(self):  return VAR_RE.match(self.functor) and not self.args
    def __repr__(self): return (self.functor if not self.args
                                else f"{self.functor}({', '.join(map(repr, self.args))})")
    def __hash__(self): return hash((self.functor, self.args))
    def __eq__(self, o): return (self.functor, self.args) == (o.functor, o.args)


class Literal:
    __slots__ = ("pred", "args", "neg")
    def __init__(self, pred, args, neg=False):
        self.pred, self.args, self.neg = pred, tuple(args), neg
    def negate(self):        return Literal(self.pred, self.args, not self.neg)
    def substitute(self, θ): return Literal(self.pred, [substitute(a, θ) for a in self.args], self.neg)
    def __repr__(self):
        return ("¬" if self.neg else "") + f"{self.pred}({', '.join(map(repr, self.args))})"
    def __hash__(self):      return hash((self.pred, self.args, self.neg))
    def __eq__(self, o):     return (self.pred, self.args, self.neg) == (o.pred, o.args, o.neg)


Clause = frozenset                                     # immutable set of Literals

# ───────────────────────────────  Mini‑parser  ────────────────────────────
def parse_literal(txt: str) -> Literal:
    txt = txt.strip()
    neg = txt.startswith(('¬', '~'))
    if neg:
        txt = txt[1:].strip()
    m = re.match(r"^([A-Za-z0-9_]+)\(([^)]*)\)$", txt)
    if not m:
        raise ValueError(f"Bad literal: {txt}")
    pred, args_str = m.group(1), m.group(2)
    args = [Term(a.strip()) for a in args_str.split(',') if a.strip()]
    return Literal(pred, args, neg)

def parse_clause(line: str) -> Clause:
    return frozenset(parse_literal(part) for part in line.split('|'))

# ───────────────────────────────  Unification  ─────────────────────────────
def substitute(t: Term, θ):
    if t.is_var():
        while t in θ:
            t = θ[t]
        return t
    if t.args:
        return Term(t.functor, [substitute(a, θ) for a in t.args])
    return t

def occurs(v: Term, t: Term, θ) -> bool:
    if v == t: return True
    if t.is_var() and t in θ: return occurs(v, θ[t], θ)
    return any(occurs(v, a, θ) for a in t.args)

def unify(x: Term, y: Term, θ=None):
    if θ is None: θ = {}
    x, y = substitute(x, θ), substitute(y, θ)
    if x == y: return θ
    if x.is_var():
        if occurs(x, y, θ): return None
        θ[x] = y; return θ
    if y.is_var():
        if occurs(y, x, θ): return None
        θ[y] = x; return θ
    if x.functor != y.functor or len(x.args) != len(y.args): return None
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

# ─────────────────────────  Deterministic resolution  ──────────────────────
def clause_str(c): return "⊥" if not c else " | ".join(sorted(map(repr, c)))

def resolve(ci, cj):
    for Li in sorted(ci, key=repr):
        for Lj in sorted(cj, key=repr):
            if Li.pred == Lj.pred and Li.neg != Lj.neg:
                θ = unify_tuple(Li.args, Lj.args)
                if θ is None: continue
                resolvent = frozenset((ci | cj) - {Li, Lj})
                resolvent = frozenset(L.substitute(θ) for L in resolvent)
                if any(L.negate() in resolvent for L in resolvent): continue  # tautology
                yield resolvent

def prove(kb, neg_goal):
    sos = [neg_goal]                          # FIFO set‑of‑support
    all_clauses = set(kb) | {neg_goal}
    step = 0
    while sos:
        Ci = sos.pop(0)
        for Cj in sorted(all_clauses, key=clause_str):
            for R in resolve(Ci, Cj):
                if R in all_clauses: continue
                step += 1
                print(f"{step:02d}. {clause_str(R)}   "
                      f"(from {clause_str(Ci)} , {clause_str(Cj)})")
                if not R:
                    print("\nEmpty clause derived – contradiction established.")
                    return True
                sos.append(R)
                all_clauses.add(R)
    print("No contradiction found (this should not happen).")
    return False

# ─────────────────────  Knowledge‑base & negated goal  ─────────────────────
KB_TEXT = [
    "T(x) | T(Liar)",
    "¬T(Liar) | ¬T(x)",
]
KB = [parse_clause(line) for line in KB_TEXT]

NEGATED_GOAL = parse_clause("¬T(Liar)")

# ─────────────────────────────────────  Main  ───────────────────────────────
if __name__ == "__main__":
    prove(KB, NEGATED_GOAL)

