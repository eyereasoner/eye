#!/usr/bin/env python3
"""barber_paradox_prover.py
──────────────────────────
A **deterministic first‑order resolution prover** that demonstrates the
inconsistency of the *Barber* paradox.

> *“In a certain village the barber shaves all and only those men who do **not**
> shave themselves.  Does the barber shave himself?”*

We formalise the story in classical logic and show—via resolution—that the two
axiom clauses encoding the barber’s rule contradict the assumption that the
barber does **not** shave himself.

# Formalisation
────────────────
* Predicate  ``Shaves(x, y)``   –  “*x* shaves *y*”.
* Constant   ``Barber``         –  the (hypothetical) barber.

Givens (biconditional broken into two implications – already in **CNF**):

    1.  ¬Shaves(Barber,x) ∨ ¬Shaves(x,x)   ;  Shaves(Barber,x) → ¬Shaves(x,x)
    2.   Shaves(x,x) ∨  Shaves(Barber,x)   ; ¬Shaves(x,x)    →  Shaves(Barber,x)

Goal: prove ``Shaves(Barber, Barber)``.
We instead **assume its negation** ¬Shaves(Barber, Barber) and derive ⊥.

Deterministic set‑of‑support resolution delivers the two‑step refutation:

    01. Shaves(Barber, Barber)
    02. ⊥

# How to run
────────────
```
python barber_paradox_prover.py
```
You will see every derived clause printed, culminating in the empty clause.
"""

from __future__ import annotations

import re
import itertools  # kept for potential extensions; not used directly here

# ─────────────────────────────  Variable regex  ────────────────────────────
VAR_RE = re.compile(r"^[a-z][A-Za-z0-9_]*$")  # variables start lower‑case


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Term, Literal, Clause                                                │
# ╰────────────────────────────────────────────────────────────────────────╯
class Term:
    """First‑order **term** (variable, constant or compound)."""

    __slots__ = ("functor", "args")

    def __init__(self, functor: str, args: list["Term"] | tuple["Term", ...] | None = None):
        self.functor = functor
        self.args = tuple(args) if args else tuple()

    # --------------------------- helpers -------------------------------- #
    def is_var(self) -> bool:
        return VAR_RE.match(self.functor) and not self.args

    # --------------------------- dunder --------------------------------- #
    def __repr__(self) -> str:
        return self.functor if not self.args else f"{self.functor}({', '.join(map(repr, self.args))})"

    def __hash__(self) -> int:
        return hash((self.functor, self.args))

    def __eq__(self, other: object) -> bool:
        return isinstance(other, Term) and (self.functor, self.args) == (other.functor, other.args)


class Literal:
    """(Possibly negated) predicate applied to a list of terms."""

    __slots__ = ("pred", "args", "neg")

    def __init__(self, pred: str, args: list[Term], neg: bool = False):
        self.pred = pred
        self.args = tuple(args)
        self.neg = neg

    # ------------------------------------------------------------------ #
    def negate(self) -> "Literal":
        return Literal(self.pred, self.args, not self.neg)

    def substitute(self, θ: dict[Term, Term]) -> "Literal":
        return Literal(self.pred, [substitute(a, θ) for a in self.args], self.neg)

    # ------------------------------------------------------------------ #
    def __repr__(self) -> str:
        return ("¬" if self.neg else "") + f"{self.pred}({', '.join(map(repr, self.args))})"

    def __hash__(self) -> int:
        return hash((self.pred, self.args, self.neg))

    def __eq__(self, other: object) -> bool:
        return (isinstance(other, Literal) and
                (self.pred, self.args, self.neg) == (other.pred, other.args, other.neg))


Clause = frozenset[Literal]  # alias for readability


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Parsing utilities                                                     │
# ╰────────────────────────────────────────────────────────────────────────╯

def parse_literal(text: str) -> Literal:
    """Convert textual literal syntax into a :class:`Literal`."""
    text = text.strip()
    neg = text.startswith(('¬', '~'))
    if neg:
        text = text[1:].strip()

    m = re.match(r"^([A-Za-z0-9_]+)\(([^)]*)\)$", text)
    if not m:
        raise ValueError(f"Bad literal: {text}")

    pred, arg_str = m.groups()
    args = [Term(a.strip()) for a in arg_str.split(',') if a.strip()]
    return Literal(pred, args, neg)


def parse_clause(line: str) -> Clause:
    return frozenset(parse_literal(part) for part in line.split('|'))


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Unification with occurs‑check                                         │
# ╰────────────────────────────────────────────────────────────────────────╯

def substitute(t: Term, θ: dict[Term, Term]) -> Term:
    if t.is_var():
        while t in θ:
            t = θ[t]
        return t
    if t.args:
        return Term(t.functor, [substitute(a, θ) for a in t.args])
    return t


def occurs(v: Term, t: Term, θ: dict[Term, Term]) -> bool:
    if v == t:
        return True
    if t.is_var() and t in θ:
        return occurs(v, θ[t], θ)
    return any(occurs(v, a, θ) for a in t.args)


def unify(x: Term, y: Term, θ: dict[Term, Term] | None = None) -> dict[Term, Term] | None:
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


def unify_tuple(a1: tuple[Term, ...], a2: tuple[Term, ...]) -> dict[Term, Term] | None:
    θ: dict[Term, Term] = {}
    for s, t in zip(a1, a2):
        θ = unify(s, t, θ)
        if θ is None:
            return None
    return θ


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Resolution engine                                                     │
# ╰────────────────────────────────────────────────────────────────────────╯

def clause_str(c: Clause) -> str:
    return "⊥" if not c else " | ".join(sorted(map(repr, c)))


def resolve(ci: Clause, cj: Clause):
    """Yield every non‑tautological resolvent of *ci* and *cj*."""
    for Li in sorted(ci, key=repr):
        for Lj in sorted(cj, key=repr):
            if Li.pred == Lj.pred and Li.neg != Lj.neg:
                θ = unify_tuple(Li.args, Lj.args)
                if θ is None:
                    continue

                resolvent = frozenset((ci | cj) - {Li, Lj})
                resolvent = frozenset(L.substitute(θ) for L in resolvent)

                # skip tautologies like P | ¬P
                if any(L.negate() in resolvent for L in resolvent):
                    continue

                yield resolvent


def prove(kb: list[Clause], neg_goal: Clause) -> bool:
    """Breadth‑first set‑of‑support resolution."""
    sos = [neg_goal]
    all_clauses = set(kb) | {neg_goal}
    step = 0

    while sos:
        Ci = sos.pop(0)
        for Cj in sorted(all_clauses, key=clause_str):
            for R in resolve(Ci, Cj):
                if R in all_clauses:
                    continue

                step += 1
                print(f"{step:02d}. {clause_str(R)}   (from {clause_str(Ci)} , {clause_str(Cj)})")

                if not R:
                    print("\nEmpty clause derived – contradiction established.")
                    return True

                sos.append(R)
                all_clauses.add(R)

    print("No contradiction found (this should not happen).")
    return False


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Knowledge base and negated goal                                       │
# ╰────────────────────────────────────────────────────────────────────────╯
KB_TEXT = [
    "¬Shaves(Barber,x) | ¬Shaves(x,x)",  # forward implication
    " Shaves(x,x) |  Shaves(Barber,x)",  # contrapositive
]

KB: list[Clause] = [parse_clause(line) for line in KB_TEXT]
NEGATED_GOAL: Clause = parse_clause("¬Shaves(Barber,Barber)")


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Script entry                                                          │
# ╰────────────────────────────────────────────────────────────────────────╯
if __name__ == "__main__":
    prove(KB, NEGATED_GOAL)

