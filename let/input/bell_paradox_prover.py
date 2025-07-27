#!/usr/bin/env python3
"""bell_contradiction_prover.py
────────────────────────────────
A **deterministic first‑order resolution prover** that exposes the logical
incompatibility of *local‑realist* hidden‑variable assignments with the set of
pairwise correlations predicted by quantum mechanics for a maximally entangled
***two‑qubit*** state (e.g. CHSH/Bell/Clauser–Horne settings).

The script is intentionally *self‑contained* and heavily documented so that
students can trace every step—from parsing and unification to the derivation
of the empty clause ⊥ using a *set‑of‑support* search strategy.

# Physical background
─────────────────────
We imagine two spatially separated experimenters—*Alice* and *Bob*—who each
choose between **two** binary measurements (settings 0 or 1) and record a
±1 or (equivalently) 0/1 outcome.

* ``A0``  – Alice’s outcome if she measures setting 0
* ``A1``  – Alice’s outcome if she measures setting 1
* ``B0``  – Bob’s   outcome if he measures setting 0
* ``B1``  – Bob’s   outcome if he measures setting 1

Under *local determinism*, a hidden‑variable λ must fix all four outcomes
**prior** to any measurement choice.  Quantum mechanics nevertheless allows
a single entangled pair to satisfy the following correlations *simultaneously*:

    (1)  A0 = B0
    (2)  A0 = B1
    (3)  A1 = B0
    (4)  A1 ≠ B1   ← the crucial CHSH twist

Chaining (1)–(3) logically forces A1 = B1, contradicting (4).  Our prover
formalises this contradiction in *classical* logic with only eight clauses.

# Logical encoding
──────────────────
Define a unary predicate ``T(x)`` meaning “the bit *x* equals 1”.

* Equality of two bits *p = q* ⇢

      (¬T(p) ∨ T(q)) ∧ (T(p) ∨ ¬T(q))

* Inequality *p ≠ q* ⇢

      (¬T(p) ∨ ¬T(q)) ∧ (T(p) ∨ T(q))

All four correlations give **four pairs** of clauses.  We then *assume*
¬T(A1) (i.e. A₁ = 0) and show that resolution derives ⊥ in five steps:

    01. ¬T(B0)
    02. ¬T(A0)
    03. ¬T(B1)
    04.  T(A1)
    05.  ⊥

Line 05 completes the refutation: the assumption of a single local hidden‑
variable assignment conflicts with the quantum predictions.
"""

from __future__ import annotations

import re
import itertools  # imported for future extensions; not used directly here

# ─────────────────────────────── Regex helper ──────────────────────────────
# A *variable* token starts with a lowercase ASCII letter.  Anything else is
# treated as a constant functor/predicate symbol.
VAR_RE = re.compile(r"^[a-z][A-Za-z0-9_]*$")

# ╭────────────────────────────────────────────────────────────────────────╮
# │  Term, Literal and Clause data structures                              │
# ╰────────────────────────────────────────────────────────────────────────╯

class Term:
    """A first‑order term: **variable**, **constant** or **function term**.

    Attributes
    ----------
    functor : str
        Symbol of the term.  For variables this is just their *name*; for
        constants it is the constant symbol; for compound terms it is the
        function symbol.
    args : tuple[Term, ...]
        Argument list.  Empty for variables and constants.
    """

    __slots__ = ("functor", "args")

    def __init__(self, functor: str, args: list["Term"] | tuple["Term", ...] | None = None):
        self.functor = functor
        self.args = tuple(args) if args else tuple()

    # ------------------------------------------------------------------ #
    def is_var(self) -> bool:
        """Return *True* iff the term qualifies syntactically as a variable."""
        return VAR_RE.match(self.functor) and not self.args

    # ------------------------------------------------------------------ #
    def __repr__(self) -> str:
        if not self.args:  # variable or constant
            return self.functor
        return f"{self.functor}({', '.join(map(repr, self.args))})"

    def __hash__(self) -> int:
        return hash((self.functor, self.args))

    def __eq__(self, other: object) -> bool:
        return isinstance(other, Term) and (self.functor, self.args) == (other.functor, other.args)


class Literal:
    """A (possibly negated) atomic predicate application."""

    __slots__ = ("pred", "args", "neg")

    def __init__(self, pred: str, args: list[Term], neg: bool = False):
        self.pred = pred
        self.args = tuple(args)
        self.neg = neg  # polarity

    # ------------------------------------------------------------------ #
    def negate(self) -> "Literal":
        return Literal(self.pred, self.args, not self.neg)

    def substitute(self, θ: dict[Term, Term]) -> "Literal":
        return Literal(self.pred, [substitute(a, θ) for a in self.args], self.neg)

    # ------------------------------------------------------------------ #
    def __repr__(self) -> str:
        args_repr = ", ".join(map(repr, self.args))
        return ("¬" if self.neg else "") + f"{self.pred}({args_repr})"

    def __hash__(self) -> int:
        return hash((self.pred, self.args, self.neg))

    def __eq__(self, other: object) -> bool:
        return (isinstance(other, Literal) and
                (self.pred, self.args, self.neg) == (other.pred, other.args, other.neg))


# A **clause** is a frozenset of literals; the empty set denotes ⊥ (false).
Clause = frozenset[Literal]


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Parsing utilities                                                     │
# ╰────────────────────────────────────────────────────────────────────────╯

def parse_literal(text: str) -> Literal:
    """Convert textual literal syntax into a :class:`Literal` object.

    Examples
    --------
    >>> parse_literal('T(A0)')
    >>> parse_literal('¬T(B1)')
    """
    text = text.strip()
    neg = text.startswith(('¬', '~'))
    if neg:
        text = text[1:].strip()

    m = re.match(r"^([A-Za-z0-9_]+)\(([^)]*)\)$", text)
    if not m:
        raise ValueError(f"Bad literal syntax: {text}")

    pred, args_str = m.groups()
    args = [Term(a.strip()) for a in args_str.split(',') if a.strip()]
    return Literal(pred, args, neg)


def parse_clause(line: str) -> Clause:
    """Parse a disjunction *L₁ | L₂ | …* into a :class:`Clause`."""
    return frozenset(parse_literal(part) for part in line.split('|'))


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Unification with occurs‑check                                         │
# ╰────────────────────────────────────────────────────────────────────────╯

def substitute(t: Term, θ: dict[Term, Term]) -> Term:
    """Apply substitution θ to term *t* (with path compression for vars)."""
    if t.is_var():
        while t in θ:
            t = θ[t]
        return t
    if t.args:
        return Term(t.functor, [substitute(a, θ) for a in t.args])
    return t  # constant


def occurs(v: Term, t: Term, θ: dict[Term, Term]) -> bool:
    """Return *True* if variable *v* occurs *directly or indirectly* in *t*."""
    if v == t:
        return True
    if t.is_var() and t in θ:
        return occurs(v, θ[t], θ)
    return any(occurs(v, a, θ) for a in t.args)


def unify(x: Term, y: Term, θ: dict[Term, Term] | None = None) -> dict[Term, Term] | None:
    """Robinson unification algorithm with explicit occurs‑check."""
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
    """Unify two tuples of terms (argument lists)."""
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
    """Return a human‑friendly string representation of a clause."""
    return "⊥" if not c else " | ".join(sorted(map(repr, c)))


def resolve(ci: Clause, cj: Clause):
    """Yield every non‑tautological *resolvent* of clauses ci and cj.

    The deterministic order imposed by ``sorted`` ensures that every run of
    the program prints the **same** proof sequence.
    """
    for Li in sorted(ci, key=repr):
        for Lj in sorted(cj, key=repr):
            if Li.pred == Lj.pred and Li.neg != Lj.neg:
                θ = unify_tuple(Li.args, Lj.args)
                if θ is None:
                    continue

                # Merge literals except the complementary pair and apply θ.
                resolvent = frozenset((ci | cj) - {Li, Lj})
                resolvent = frozenset(L.substitute(θ) for L in resolvent)

                # Skip tautological resolvents containing P and ¬P.
                if any(L.negate() in resolvent for L in resolvent):
                    continue

                yield resolvent


def prove(kb: list[Clause], neg_goal: Clause) -> bool:
    """Deterministic *set‑of‑support* resolution refutation procedure."""
    sos = [neg_goal]                    # FIFO queue (breadth‑first search)
    all_clauses = set(kb) | {neg_goal}
    step = 0

    while sos:
        Ci = sos.pop(0)
        for Cj in sorted(all_clauses, key=clause_str):
            for R in resolve(Ci, Cj):
                if R in all_clauses:
                    continue  # already known

                step += 1
                print(f"{step:02d}. {clause_str(R)}   (from {clause_str(Ci)} , {clause_str(Cj)})")

                if not R:  # derived ⊥
                    print("\nEmpty clause derived – contradiction established.")
                    return True

                sos.append(R)
                all_clauses.add(R)

    # Should never reach here for the Bell example.
    print("No contradiction found (should not happen in this Bell example).")
    return False


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Knowledge base: eight clauses for the four correlations               │
# ╰────────────────────────────────────────────────────────────────────────╯
KB_TEXT = [
    # (1)  A0 = B0
    "¬T(A0) | T(B0)",
    " T(A0) | ¬T(B0)",

    # (2)  A0 = B1
    "¬T(A0) | T(B1)",
    " T(A0) | ¬T(B1)",

    # (3)  A1 = B0
    "¬T(A1) | T(B0)",
    " T(A1) | ¬T(B0)",

    # (4)  A1 ≠ B1   (CHSH inequality)
    "¬T(A1) | ¬T(B1)",
    " T(A1) |  T(B1)",
]

KB: list[Clause] = [parse_clause(line) for line in KB_TEXT]

# Negated goal: assume A1 = 0 (¬T(A1)) and derive a contradiction.
NEGATED_GOAL: Clause = parse_clause("¬T(A1)")


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Script entry point                                                    │
# ╰────────────────────────────────────────────────────────────────────────╯
if __name__ == "__main__":
    prove(KB, NEGATED_GOAL)

