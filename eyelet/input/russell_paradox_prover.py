#!/usr/bin/env python3
"""russell_paradox_prover.py
────────────────────────────
A **deterministic first‑order resolution prover** that formalises and
refutes the naïve‑comprehension version of *Russell’s paradox*.

> **Russell set** R := { x | x ∉ x }
>
> Does R contain itself?

In first‑order logic we encode membership with a binary predicate
``M(x, y)`` meaning “*x* is a **member** of *y*”.  Russell’s definition
becomes a biconditional

    ∀x [ M(x,R) ↔ ¬M(x,x) ].

Breaking this into clauses (CNF) gives

    1.  M(x,x) ∨  M(x,R)         ; (¬M(x,x) → M(x,R))
    2. ¬M(x,R) ∨ ¬M(x,x)         ; ( M(x,R) → ¬M(x,x))

Our **goal** is to derive a contradiction from the assumption
¬M(R,R).  In two deterministic resolution steps the prover yields the
empty clause ⊥, confirming the inconsistency.

Run the script to see the proof trace:

```bash
python russell_paradox_prover.py
```
"""

from __future__ import annotations

import re
import itertools  # imported for symmetry with sibling scripts; not used here

# ───────────────────────────────  Regex helper  ────────────────────────────
# Variable tokens start with a lowercase ASCII letter; everything else is a
# constant/functor symbol.
VAR_RE = re.compile(r"^[a-z][A-Za-z0-9_]*$")


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Term, Literal, Clause                                                │
# ╰────────────────────────────────────────────────────────────────────────╯

class Term:
    """First‑order term (variable, constant, or compound function term)."""

    __slots__ = ("functor", "args")

    def __init__(self, functor: str, args: list["Term"] | tuple["Term", ...] | None = None):
        self.functor = functor
        self.args = tuple(args) if args else tuple()

    # --------------------------- helpers -------------------------------- #
    def is_var(self) -> bool:
        """Return *True* if the term is syntactically a **variable**."""
        return VAR_RE.match(self.functor) and not self.args

    # --------------------------- dunder --------------------------------- #
    def __repr__(self) -> str:
        return self.functor if not self.args else f"{self.functor}({', '.join(map(repr, self.args))})"

    def __hash__(self) -> int:
        return hash((self.functor, self.args))

    def __eq__(self, other: object) -> bool:
        return isinstance(other, Term) and (self.functor, self.args) == (other.functor, other.args)


class Literal:
    """An atomic predicate application that may be **negated**."""

    __slots__ = ("pred", "args", "neg")

    def __init__(self, pred: str, args: list[Term], neg: bool = False):
        self.pred = pred
        self.args = tuple(args)
        self.neg = neg

    # ------------------------- factory ops ----------------------------- #
    def negate(self) -> "Literal":
        return Literal(self.pred, self.args, not self.neg)

    def substitute(self, θ: dict[Term, Term]) -> "Literal":
        return Literal(self.pred, [substitute(a, θ) for a in self.args], self.neg)

    # --------------------------- dunder -------------------------------- #
    def __repr__(self) -> str:
        return ("¬" if self.neg else "") + f"{self.pred}({', '.join(map(repr, self.args))})"

    def __hash__(self) -> int:
        return hash((self.pred, self.args, self.neg))

    def __eq__(self, other: object) -> bool:
        return (isinstance(other, Literal) and
                (self.pred, self.args, self.neg) == (other.pred, other.args, other.neg))


Clause = frozenset[Literal]  # alias for clarity – the empty set represents ⊥


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Parsing utilities                                                     │
# ╰────────────────────────────────────────────────────────────────────────╯

def parse_literal(text: str) -> Literal:
    """Convert textual literal syntax (e.g. '¬M(x,R)') into a Literal."""
    text = text.strip()
    neg = text.startswith(('¬', '~'))
    if neg:
        text = text[1:].strip()

    m = re.match(r"^([A-Za-z0-9_]+)\(([^)]*)\)$", text)
    if not m:
        raise ValueError(f"Bad literal syntax: {text}")

    pred, arg_str = m.groups()
    args = [Term(a.strip()) for a in arg_str.split(',') if a.strip()]
    return Literal(pred, args, neg)


def parse_clause(line: str) -> Clause:
    """Parse a disjunction of literals separated by '|'."""
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
    """Generate every non‑tautological resolvent of *ci* and *cj*."""
    for Li in sorted(ci, key=repr):
        for Lj in sorted(cj, key=repr):
            if Li.pred == Lj.pred and Li.neg != Lj.neg:
                θ = unify_tuple(Li.args, Lj.args)
                if θ is None:
                    continue

                resolvent = frozenset((ci | cj) - {Li, Lj})
                resolvent = frozenset(L.substitute(θ) for L in resolvent)

                # Skip tautologies such as P | ¬P.
                if any(L.negate() in resolvent for L in resolvent):
                    continue

                yield resolvent


def prove(kb: list[Clause], neg_goal: Clause) -> bool:
    """Breadth‑first set‑of‑support resolution refutation."""
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

    print("No contradiction found (this should not happen here).")
    return False


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Knowledge base and negated goal                                       │
# ╰────────────────────────────────────────────────────────────────────────╯
KB_TEXT = [
    "M(x,x) | M(x,Russell)",          # forward implication
    "¬M(x,Russell) | ¬M(x,x)",        # converse
]

KB: list[Clause] = [parse_clause(line) for line in KB_TEXT]
NEGATED_GOAL: Clause = parse_clause("¬M(Russell,Russell)")


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Script entry point                                                    │
# ╰────────────────────────────────────────────────────────────────────────╯
if __name__ == "__main__":
    prove(KB, NEGATED_GOAL)

