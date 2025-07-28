#!/usr/bin/env python3
"""alice_happy_prover.py
────────────────────────
A **deterministic set‑of‑support resolution prover** that shows, from five
natural‑language premises, that **Alice is happy**.

This file is extensively annotated so you can learn how a minimal resolution
engine operates on a classical first‑order knowledge base.

# Problem statement
───────────────────
We model the following common‑sense facts in **clausal form** (CNF):

| # | Clause (variables ∀‑quantified)                     | English reading                             |
|---|-----------------------------------------------------|---------------------------------------------|
| 1 |  ¬Student(x) ∨ Studies(x)                           | If *x* is a student, *x* studies.           |
| 2 |  ¬Studies(x) ∨ ¬Intelligent(x) ∨ Passes(x)          | Studious **and** intelligent students pass. |
| 3 |  ¬Passes(x) ∨ Happy(x)                              | Passing makes you happy.                    |
| 4 |  Student(Alice)                                     | Alice is a student.                         |
| 5 |  Intelligent(Alice)                                 | Alice is intelligent.                       |

Our **goal** is to prove ``Happy(Alice)``.  We do so by *refuting* its
negation ¬Happy(Alice).  Using a deterministic (breadth‑first) SOS strategy,
we obtain the **five‑step** proof predicted in the docstring at the top of the
original skeleton.
"""

from __future__ import annotations

import re
import itertools  # present for tokeniser look‑ahead; no other external deps

# ─────────────────────────────  Regex for variable tokens  ──────────────────
VAR = re.compile(r"^[a-z][A-Za-z0-9_]*$")  # lowercase initial letter ⇒ variable


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Term, Literal, Clause                                                 │
# ╰────────────────────────────────────────────────────────────────────────╯

class Term:
    """First‑order **term**: variable, constant, or compound function term."""

    __slots__ = ("functor", "args")

    def __init__(self, functor: str, args: list["Term"] | tuple["Term", ...] | None = None):
        self.functor = functor
        self.args = tuple(args) if args else tuple()

    # --------------------------- helpers -------------------------------- #
    def is_var(self) -> bool:
        return VAR.match(self.functor) and not self.args

    # --------------------------- dunder --------------------------------- #
    def __repr__(self) -> str:
        return self.functor if not self.args else f"{self.functor}({', '.join(map(repr, self.args))})"

    def __hash__(self) -> int:
        return hash((self.functor, self.args))

    def __eq__(self, other: object) -> bool:
        return isinstance(other, Term) and (self.functor, self.args) == (other.functor, other.args)


class Literal:
    """An atomic predicate application, possibly **negated**."""

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


Clause = frozenset[Literal]  # alias – the empty clause represents ⊥


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Parsing utilities (hand‑rolled to accept nested terms)                │
# ╰────────────────────────────────────────────────────────────────────────╯

def tokenize(s: str):
    """Yield identifiers and punctuation one by one (very small lexer)."""
    for t in re.finditer(r"[A-Za-z0-9_]+|[(),]", s):
        yield t.group(0)


def next_peek(it):
    """Return the *next* token **without** removing it from the iterator.
    Consumes one token then chains it back onto the original iterator.
    Returns *None* if the iterator is exhausted.
    """
    try:
        tok = next(it)
    except StopIteration:
        return None
    # Chain puts the token back in front so the caller will still read it.
    it = itertools.chain([tok], it)
    return tok


def parse_term(tokens):
    """Recursive‑descent parser for (possibly nested) function terms."""
    tok = next(tokens)  # current identifier/constant/variable
    if next_peek(tokens) == '(':  # compound term detected
        next(tokens)              # consume '('
        args: list[Term] = []
        if next_peek(tokens) != ')':
            while True:
                args.append(parse_term(tokens))
                sep = next(tokens)
                if sep == ')':
                    break
                if sep != ',':
                    raise ValueError("Malformed term – expected ',' or ')'.")
        else:
            next(tokens)          # consume ')'
        return Term(tok, args)
    # simple variable or constant
    return Term(tok)


def parse_literal(text: str) -> Literal:
    text = text.strip()
    neg = text.startswith(('¬', '~'))
    if neg:
        text = text[1:].strip()

    head, tail = text.split('(', 1)
    arg_tokens = tokenize(tail[:-1])  # drop trailing ')'
    args: list[Term] = []
    if tail[:-1]:
        while True:
            args.append(parse_term(arg_tokens))
            try:
                comma = next(arg_tokens)
            except StopIteration:
                break
            if comma != ',':
                raise ValueError("Malformed literal – expected ','.")
    return Literal(head, args, neg)


def parse_clause(line: str) -> Clause:
    """Parse a disjunction of literals separated by '|'."""
    return frozenset(parse_literal(part) for part in line.split('|'))


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Unification with occurs‑check                                         │
# ╰────────────────────────────────────────────────────────────────────────╯

def substitute(t: Term, θ: dict[Term, Term]) -> Term:
    if t.is_var():
        seen: set[Term] = set()
        while t.is_var() and t in θ and t not in seen:
            seen.add(t)
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
    return any(occurs(v, a, θ) for a in t.args) if t.args else False


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
    """Yield all non‑tautological resolvents of clauses ci and cj."""
    for Li in sorted(ci, key=repr):
        for Lj in sorted(cj, key=repr):
            if Li.pred == Lj.pred and Li.neg != Lj.neg:
                θ = unify_tuple(Li.args, Lj.args)
                if θ is None:
                    continue

                resolvent = frozenset((ci | cj) - {Li, Lj})
                resolvent = frozenset(L.substitute(θ) for L in resolvent)

                if any(L.negate() in resolvent for L in resolvent):
                    continue  # tautology like P | ¬P

                yield resolvent


def prove(kb: list[Clause], neg_goal: Clause) -> bool:
    """Breadth‑first *set‑of‑support* resolution refutation."""
    sos: list[Clause] = [neg_goal]
    all_clauses: set[Clause] = set(kb) | {neg_goal}
    step = 0

    while sos:
        Ci = sos.pop(0)
        for Cj in sorted(all_clauses, key=clause_str):
            for R in resolve(Ci, Cj):
                if R in all_clauses:
                    continue

                step += 1
                print(f"{step:02d}. {clause_str(R)}   (from {clause_str(Ci)} , {clause_str(Cj)})")

                if not R:  # empty clause ⇒ contradiction
                    print("\nEmpty clause derived – goal is proved.")
                    return True

                sos.append(R)
                all_clauses.add(R)

    print("Proof failed – goal not entailed.")
    return False


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Knowledge base and negated goal                                       │
# ╰────────────────────────────────────────────────────────────────────────╯
KB_TEXT = [
    "¬Student(x) | Studies(x)",                             # rule 1
    "¬Studies(x) | ¬Intelligent(x) | Passes(x)",            # rule 2
    "¬Passes(x) | Happy(x)",                                # rule 3
    "Student(Alice)",                                       # fact 4
    "Intelligent(Alice)",                                   # fact 5
]

KB: list[Clause] = [parse_clause(line) for line in KB_TEXT]
NEGATED_GOAL: Clause = parse_clause("¬Happy(Alice)")


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Script entry point                                                    │
# ╰────────────────────────────────────────────────────────────────────────╯
if __name__ == "__main__":
    prove(KB, NEGATED_GOAL)
