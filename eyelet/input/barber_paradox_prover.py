#!/usr/bin/env python3
"""
barber_paradox_prover.py
────────────────────────
Deterministic first‑order resolution proof of the Barber paradox.

Predicate   Shaves(x,y)   ≝  “x shaves y”
Constant    Barber        ≝  the (hypothetical) barber

Axiom:  ∀x [ Shaves(Barber,x)  ↔  ¬Shaves(x,x) ]

Clausal form (variables implicitly ∀‑quantified):

1. ¬Shaves(Barber,x) ∨ ¬Shaves(x,x)         ; (Shaves(Barber,x) → ¬Shaves(x,x))
2.  Shaves(x,x) ∨  Shaves(Barber,x)         ; (¬Shaves(x,x)    →  Shaves(Barber,x))

Goal to prove: **Shaves(Barber,Barber)**  
We refute its negation ¬Shaves(Barber,Barber).

Deterministic proof:

01. Shaves(Barber, Barber)   (from ¬Shaves(Barber, Barber) , Shaves(Barber, x) | Shaves(x, x))
02. ⊥                        (from Shaves(Barber, Barber) , ¬Shaves(Barber, Barber))

Empty clause derived – contradiction established.
"""

import re, itertools

# ──────────────────────────────  Tiny term & literal classes  ────────────────
VAR_RE = re.compile(r"^[a-z][A-Za-z0-9_]*$")           # variables are lower‑case


class Term:
    __slots__ = ("functor", "args")

    def __init__(self, functor, args=None):
        self.functor = functor
        self.args = tuple(args) if args else tuple()

    def is_var(self):  return VAR_RE.match(self.functor) and not self.args

    def __repr__(self):
        return (self.functor if not self.args
                else f"{self.functor}({', '.join(map(repr, self.args))})")

    def __hash__(self): return hash((self.functor, self.args))
    def __eq__(self, other): return (self.functor, self.args) == (other.functor, other.args)


class Literal:
    __slots__ = ("pred", "args", "neg")

    def __init__(self, pred, args, neg=False):
        self.pred, self.args, self.neg = pred, tuple(args), neg

    def negate(self):        return Literal(self.pred, self.args, not self.neg)
    def substitute(self, θ): return Literal(self.pred, [substitute(a, θ) for a in self.args], self.neg)

    def __repr__(self):
        return ("¬" if self.neg else "") + f"{self.pred}({', '.join(map(repr, self.args))})"

    def __hash__(self):      return hash((self.pred, self.args, self.neg))
    def __eq__(self, other): return (self.pred, self.args, self.neg) == (other.pred, other.args, other.neg)


Clause = frozenset                                           # immutable set of Literals

# ─────────────────────────────────  Minimal parser  ──────────────────────────
def parse_literal(text: str) -> Literal:
    """Parse strings like  '¬Shaves(Barber,x)' or 'Shaves(x,x)'."""
    text = text.strip()
    neg = text.startswith(('¬', '~'))
    if neg:
        text = text[1:].strip()

    m = re.match(r"^([A-Za-z0-9_]+)\(([^)]*)\)$", text)
    if not m:
        raise ValueError(f"Bad literal: {text}")

    pred, arg_str = m.group(1), m.group(2)
    args = [Term(a.strip()) for a in arg_str.split(',') if a.strip()]
    return Literal(pred, args, neg)


def parse_clause(line: str) -> Clause:
    return frozenset(parse_literal(part) for part in line.split('|'))

# ─────────────────────────────────  Unification  ────────────────────────────
def substitute(term: Term, θ):
    if term.is_var():
        while term in θ:
            term = θ[term]
        return term
    if term.args:
        return Term(term.functor, [substitute(a, θ) for a in term.args])
    return term


def occurs(v: Term, t: Term, θ) -> bool:
    if v == t:
        return True
    if t.is_var() and t in θ:
        return occurs(v, θ[t], θ)
    return any(occurs(v, a, θ) for a in t.args)


def unify(x: Term, y: Term, θ=None):
    if θ is None:
        θ = {}
    x, y = substitute(x, θ), substitute(y, θ)
    if x == y:
        return θ
    if x.is_var():
        if occurs(x, y, θ):
            return None
        θ[x] = y
        return θ
    if y.is_var():
        if occurs(y, x, θ):
            return None
        θ[y] = x
        return θ
    if x.functor != y.functor or len(x.args) != len(y.args):
        return None
    for xi, yi in zip(x.args, y.args):
        θ = unify(xi, yi, θ)
        if θ is None:
            return None
    return θ


def unify_tuple(a1, a2):
    θ = {}
    for s, t in zip(a1, a2):
        θ = unify(s, t, θ)
        if θ is None:
            return None
    return θ

# ──────────────────────────  Deterministic resolution  ──────────────────────
def clause_str(c): return "⊥" if not c else " | ".join(sorted(map(repr, c)))


def resolve(ci, cj):
    for Li in sorted(ci, key=repr):
        for Lj in sorted(cj, key=repr):
            if Li.pred == Lj.pred and Li.neg != Lj.neg:
                θ = unify_tuple(Li.args, Lj.args)
                if θ is None:
                    continue
                resolvent = frozenset((ci | cj) - {Li, Lj})
                resolvent = frozenset(L.substitute(θ) for L in resolvent)
                if any(L.negate() in resolvent for L in resolvent):
                    continue     # tautology
                yield resolvent


def prove(kb, neg_goal):
    sos = [neg_goal]                          # FIFO queue – deterministic
    all_clauses = set(kb) | {neg_goal}
    step = 0
    while sos:
        Ci = sos.pop(0)
        for Cj in sorted(all_clauses, key=clause_str):
            for resolvent in resolve(Ci, Cj):
                if resolvent in all_clauses:
                    continue
                step += 1
                print(f"{step:02d}. {clause_str(resolvent)}   "
                      f"(from {clause_str(Ci)} , {clause_str(Cj)})")
                if not resolvent:
                    print("\nEmpty clause derived – contradiction established.")
                    return True
                sos.append(resolvent)
                all_clauses.add(resolvent)
    print("No contradiction found (this should not happen).")
    return False

# ──────────────────────────  Knowledge‑base & negated goal  ────────────────
KB_TEXT = [
    "¬Shaves(Barber,x) | ¬Shaves(x,x)",
    "Shaves(x,x) | Shaves(Barber,x)",
]
KB = [parse_clause(line) for line in KB_TEXT]

NEGATED_GOAL = parse_clause("¬Shaves(Barber,Barber)")

# ─────────────────────────────────────  Main  ───────────────────────────────
if __name__ == "__main__":
    prove(KB, NEGATED_GOAL)

