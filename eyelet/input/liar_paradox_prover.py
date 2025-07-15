#!/usr/bin/env python3
"""liar_paradox_prover.py
─────────────────────────
A **deterministic first‑order resolution prover** that derives a contradiction
from the classic self‑referential *“Liar”* sentence.

This version is *heavily commented* to serve as an educational reference.
Scroll through the source to learn how a minimal resolution-based theorem
prover can be built in under ~150 lines of Python.

# Logical background
────────────────────
* Predicate  ``T(x)`` means “the statement *x* is **true**”.
* Constant   ``Liar`` denotes the self‑referential statement *“This statement
  is false.”*

We intentionally encode the **inconsistent** semantics of *Liar* using two
universally‑quantified clauses::

    (∀x)  [¬T(x) → T(Liar)]      ⟹      T(x)  ∨  T(Liar)
    (∀x)  [ T(Liar) → ¬T(x)]     ⟹   ¬T(Liar) ∨ ¬T(x)

* **Rule 1**:  If a statement is *not* true, then *Liar* is true.
* **Rule 2**:  If *Liar* is true, then *no* statement is true.

Our **goal** is to prove ``T(Liar)``.  We do so *indirectly* by **refuting**
its negation ¬T(Liar).  Resolution search is *deterministic* (breadth‑first) so
the proof found is always the same:

    01. T(Liar)                 (resolving ¬T(Liar) with  T(x) ∨ T(Liar))
    02. ⊥                       (resolving  T(Liar) with ¬T(Liar) ∨ ¬T(x))

The empty clause ⊥ signals a contradiction, completing the proof.

Run the module as a script to see the steps printed on the console.
"""

from __future__ import annotations  # allow forward references in type hints

import re
import itertools  # (imported for possible extensions; not strictly needed)

# ─────────────────────────────── Regex helpers ─────────────────────────────
# Variables are represented by *strings* whose first character is lowercase.
# Example: "x", "foo42".
VAR_RE = re.compile(r"^[a-z][A-Za-z0-9_]*$")  # pre‑compiled for speed


# ──────────────────────────────── Term class ───────────────────────────────
class Term:
    """Represents either a **variable**, a **constant**, or a **function term**.

    * ``functor`` : str
        - For a **variable** this is the variable's *name* (e.g. "x").
        - For a **constant** it is the constant's *symbol* (e.g. "Liar").
        - For a **function term** it is the *function symbol*.

    * ``args`` : tuple[Term, ...]
        - Empty ``args`` means the term is a **variable/constant**.
        - Non‑empty ``args`` means the term is a **compound** term.

    The class is *hashable* so ``Term`` objects can live inside sets and be
    dictionary keys (crucial for representing clauses).
    """

    __slots__ = ("functor", "args")

    def __init__(self, functor: str, args: list["Term"] | tuple["Term", ...] | None = None):
        self.functor = functor
        self.args = tuple(args) if args else tuple()

    # ----------------------------- Predicates ----------------------------- #
    def is_var(self) -> bool:
        """Return *True* iff the term is a **variable** (per ``VAR_RE``)."""
        return VAR_RE.match(self.functor) and not self.args

    # --------------------------- Magic methods --------------------------- #
    def __repr__(self) -> str:
        """Code‑style representation (unambiguous)."""
        if not self.args:  # variable or constant
            return self.functor
        arg_str = ", ".join(map(repr, self.args))
        return f"{self.functor}({arg_str})"

    def __hash__(self) -> int:
        return hash((self.functor, self.args))

    def __eq__(self, other: object) -> bool:
        return isinstance(other, Term) and (self.functor, self.args) == (other.functor, other.args)


# ─────────────────────────────── Literal class ─────────────────────────────
class Literal:
    """Represents an *possibly negated* atomic predicate.

    For example, the *positive* literal ``T(Liar)`` is::

        Literal("T", [Term("Liar")], neg=False)

    The *negative* literal ¬T(x) is::

        Literal("T", [Term("x")], neg=True)
    """

    __slots__ = ("pred", "args", "neg")

    def __init__(self, pred: str, args: list[Term], neg: bool = False):
        self.pred = pred            # predicate symbol
        self.args = tuple(args)     # arguments
        self.neg = neg              # polarity

    # Factory‑like helpers ------------------------------------------------- #
    def negate(self) -> "Literal":
        """Return a *new* :class:`Literal` with flipped polarity."""
        return Literal(self.pred, self.args, not self.neg)

    def substitute(self, θ: dict[Term, Term]) -> "Literal":
        """Return a *new* literal after applying substitution θ."""
        return Literal(self.pred,
                       [substitute(a, θ) for a in self.args],
                       self.neg)

    # Magic methods -------------------------------------------------------- #
    def __repr__(self) -> str:
        args_repr = ", ".join(map(repr, self.args))
        return ("¬" if self.neg else "") + f"{self.pred}({args_repr})"

    def __hash__(self) -> int:
        return hash((self.pred, self.args, self.neg))

    def __eq__(self, other: object) -> bool:
        return (isinstance(other, Literal) and
                (self.pred, self.args, self.neg) ==
                (other.pred, other.args, other.neg))


# A *clause* is simply a **frozenset** of literals.
# The empty set ∅ represents the contradiction/falsehood ⊥.
Clause = frozenset[Literal]


# ───────────────────────────── Mini‑parser utils ───────────────────────────
def parse_literal(txt: str) -> Literal:
    """Parse a literal from text of the form ``T(x,y)`` or ``¬T(x)``."""
    txt = txt.strip()

    # Handle optional negation prefix.
    neg = txt.startswith(('¬', '~'))
    if neg:
        txt = txt[1:].strip()

    # Very small ad‑hoc grammar via regex.
    m = re.match(r"^([A-Za-z0-9_]+)\(([^)]*)\)$", txt)
    if not m:
        raise ValueError(f"Bad literal syntax: {txt}")

    pred, args_str = m.groups()
    args = [Term(arg.strip()) for arg in args_str.split(',') if arg.strip()]
    return Literal(pred, args, neg)


def parse_clause(line: str) -> Clause:
    """Parse a *disjunction* of literals separated by the ``|`` symbol."""
    return frozenset(parse_literal(part) for part in line.split('|'))


# ───────────────────────────── Unification logic ───────────────────────────
def substitute(t: Term, θ: dict[Term, Term]) -> Term:
    """Recursively apply substitution θ to term *t*.

    Uses *union‑find* style path‑compression so repeated lookups are cheap.
    """
    if t.is_var():
        # Chase variable bindings until a fixed point is reached.
        while t in θ:
            t = θ[t]
        return t

    # Recurse into arguments for compound terms.
    if t.args:
        return Term(t.functor, [substitute(a, θ) for a in t.args])
    return t  # constant


def occurs(v: Term, t: Term, θ: dict[Term, Term]) -> bool:
    """Check the *occurs check* to prevent infinite terms (ω‑terms)."""
    if v == t:
        return True
    if t.is_var() and t in θ:
        return occurs(v, θ[t], θ)
    return any(occurs(v, a, θ) for a in t.args)


def unify(x: Term, y: Term, θ: dict[Term, Term] | None = None) -> dict[Term, Term] | None:
    """Classic *Martelli‑Montanari* unification algorithm (linear).

    Returns *None* on failure, else a dictionary mapping variables to terms.
    The dictionary is *destructively updated* for efficiency.
    """
    if θ is None:
        θ = {}

    x, y = substitute(x, θ), substitute(y, θ)  # work with the current images

    # 1. Identical terms unify trivially.
    if x == y:
        return θ

    # 2. Variable cases ---------------------------------------------------- #
    if x.is_var():
        if occurs(x, y, θ):
            return None  # cyclic
        θ[x] = y
        return θ

    if y.is_var():
        if occurs(y, x, θ):
            return None
        θ[y] = x
        return θ

    # 3. Compound term: check functor & arity, then recurse element‑wise.
    if x.functor != y.functor or len(x.args) != len(y.args):
        return None

    for xi, yi in zip(x.args, y.args):
        θ = unify(xi, yi, θ)
        if θ is None:
            return None
    return θ


def unify_tuple(a1: tuple[Term, ...], a2: tuple[Term, ...]) -> dict[Term, Term] | None:
    """Unify two *tuples* of terms (used for predicate argument lists)."""
    θ: dict[Term, Term] = {}
    for s, t in zip(a1, a2):
        θ = unify(s, t, θ)
        if θ is None:
            return None
    return θ


# ───────────────────── Deterministic resolution rules ──────────────────────
def clause_str(c: Clause) -> str:
    """Pretty render a clause for console output."""
    return "⊥" if not c else " | ".join(sorted(map(repr, c)))


def resolve(ci: Clause, cj: Clause):
    """Generate *all* non‑tautological resolvents of clauses *ci* and *cj*.

    A **resolvent** is built by
        1. picking a pair of complementary literals,
        2. unifying their arguments,
        3. merging the remaining literals and
        4. applying the most general unifier.

    Tautological resolvents (containing both a literal and its negation) are
    discarded immediately.
    """
    # We sort to make the procedure fully deterministic and reproducible.
    for Li in sorted(ci, key=repr):
        for Lj in sorted(cj, key=repr):
            if Li.pred == Lj.pred and Li.neg != Lj.neg:
                θ = unify_tuple(Li.args, Lj.args)
                if θ is None:
                    continue  # incompatible

                # Combine all *other* literals, exclude the resolved pair.
                resolvent = frozenset((ci | cj) - {Li, Lj})
                # Apply substitution to every literal.
                resolvent = frozenset(L.substitute(θ) for L in resolvent)

                # Skip tautologies like *P | ¬P*.
                if any(L.negate() in resolvent for L in resolvent):
                    continue

                yield resolvent


def prove(kb: list[Clause], neg_goal: Clause) -> bool:
    """Breadth‑first *Set‑of‑Support* resolution refutation.

    * ``kb``       : list of *background* clauses (assumed consistent)
    * ``neg_goal`` : a *single* clause representing the **negated goal**

    The *set‑of‑support* (sos) starts with ``neg_goal`` only and each new
    resolvent derived from sos with any clause (sos or kb) is *added* to sos.
    This keeps the search focussed on the goal and guarantees completeness
    for refutations when the background theory is satisfiable.
    """

    sos = [neg_goal]            # FIFO queue for breadth‑first search
    all_clauses = set(kb) | {neg_goal}

    step = 0  # step counter for nice printing
    while sos:
        Ci = sos.pop(0)         # breadth‑first = pop from front
        for Cj in sorted(all_clauses, key=clause_str):
            for R in resolve(Ci, Cj):
                if R in all_clauses:
                    continue  # skip duplicates

                step += 1
                print(f"{step:02d}. {clause_str(R)}   (from {clause_str(Ci)} , {clause_str(Cj)})")

                if not R:       # derived the empty clause ⊥
                    print("\nEmpty clause derived – contradiction established.")
                    return True

                sos.append(R)
                all_clauses.add(R)

    # Exhausted sos without contradiction → either kb ∪ {¬goal} is satisfiable
    # (should *not* happen for the Liar paradox) or we reached the resource limit.
    print("No contradiction found (this should not happen).")  # safeguard
    return False


# ──────────────────── Knowledge base + negated goal clauses ────────────────
KB_TEXT = [
    "T(x) | T(Liar)",       # encoding of rule 1
    "¬T(Liar) | ¬T(x)",     # encoding of rule 2
]

KB: list[Clause] = [parse_clause(line) for line in KB_TEXT]
NEGATED_GOAL: Clause = parse_clause("¬T(Liar)")


# ────────────────────────────────── Entrypoint ─────────────────────────────
if __name__ == "__main__":
    prove(KB, NEGATED_GOAL)

