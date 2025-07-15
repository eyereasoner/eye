#!/usr/bin/env python3
"""unicorn_prover.py
────────────────────
A **deterministic set‑of‑support resolution prover** that shows—using six
clauses of background knowledge—that **the Unicorn is magical**.

# Knowledge base (already in CNF)
─────────────────────────────────
| # | Clause (variables ∀‑quantified)          | English reading                                    |
|---|------------------------------------------|----------------------------------------------------|
| 1 |  ¬Mythical(x) ∨ Immortal(x)              | If *x* is mythical then *x* is immortal.           |
| 2 |   Mythical(x) ∨ Mortal(x)                | If *x* is not mythical then *x* is mortal.         |
| 3 |   Mythical(x) ∨ Mammal(x)                | Mythical creatures are mammals (inclusive).        |
| 4 |  ¬Immortal(x) ∨ Horned(x)                | Non‑immortals are horned.                          |
| 5 |  ¬Mammal(x)   ∨ Horned(x)                | Non‑mammals are horned.                            |
| 6 |  ¬Horned(x)   ∨ Magical(x)               | Horned creatures are magical.                      |

Goal: prove ``Magical(Unicorn)`` by contradiction.  Negate the goal and run
breadth‑first SOS resolution.  The expected eight‑step proof is printed in the
original skeleton and reproduced by this script.
"""

from __future__ import annotations

import re
import itertools

# ─────────────────────────────  Variable regex  ────────────────────────────
VAR = re.compile(r"^[a-z][A-Za-z0-9_]*$")


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Term, Literal, Clause                                                 │
# ╰────────────────────────────────────────────────────────────────────────╯

class Term:
    __slots__ = ("functor", "args")

    def __init__(self, functor: str, args: list["Term"] | tuple["Term", ...] | None = None):
        self.functor = functor
        self.args = tuple(args) if args else tuple()

    def is_var(self) -> bool:
        return VAR.match(self.functor) and not self.args

    def __repr__(self):
        return self.functor if not self.args else f"{self.functor}({', '.join(map(repr, self.args))})"

    def __hash__(self):
        return hash((self.functor, self.args))

    def __eq__(self, other: object):
        return isinstance(other, Term) and (self.functor, self.args) == (other.functor, other.args)


class Literal:
    __slots__ = ("pred", "args", "neg")

    def __init__(self, pred: str, args: list[Term], neg: bool = False):
        self.pred = pred
        self.args = tuple(args)
        self.neg = neg

    def negate(self):
        return Literal(self.pred, self.args, not self.neg)

    def substitute(self, θ: dict[Term, Term]):
        return Literal(self.pred, [substitute(a, θ) for a in self.args], self.neg)

    def __repr__(self):
        return ("¬" if self.neg else "") + f"{self.pred}({', '.join(map(repr, self.args))})"

    def __hash__(self):
        return hash((self.pred, self.args, self.neg))

    def __eq__(self, other: object):
        return isinstance(other, Literal) and (self.pred, self.args, self.neg) == (other.pred, other.args, other.neg)


Clause = frozenset[Literal]  # the empty clause represents ⊥


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Parsing utilities                                                     │
# ╰────────────────────────────────────────────────────────────────────────╯

def tokenize(s: str):
    for m in re.finditer(r"[A-Za-z0-9_]+|[(),]", s):
        yield m.group(0)


def next_peek(it):
    try:
        tok = next(it)
    except StopIteration:
        return None, it
    it = itertools.chain([tok], it)
    return tok, it


def parse_term(tokens):
    tok = next(tokens)
    peek, tokens = next_peek(tokens)
    if peek == '(':  # compound term
        next(tokens)  # consume '('
        args: list[Term] = []
        if next_peek(tokens)[0] != ')':
            while True:
                args.append(parse_term(tokens))
                sep = next(tokens)
                if sep == ')':
                    break
                if sep != ',':
                    raise ValueError("Malformed term – expected ',' or ')'.")
        else:
            next(tokens)  # consume ')'
        return Term(tok, args)
    return Term(tok)  # constant or variable


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


def unify(x: Term, y: Term, θ: dict[Term, Term] | None = None):
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


def unify_tuple(a1: tuple[Term, ...], a2: tuple[Term, ...]):
    θ: dict[Term, Term] = {}
    for s, t in zip(a1, a2):
        θ = unify(s, t, θ)
        if θ is None:
            return None
    return θ


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Resolution engine                                                     │
# ╰────────────────────────────────────────────────────────────────────────╯

def clause_str(c: Clause):
    return "⊥" if not c else " | ".join(sorted(map(repr, c)))


def resolve(ci: Clause, cj: Clause):
    for Li in sorted(ci, key=repr):
        for Lj in sorted(cj, key=repr):
            if Li.pred == Lj.pred and Li.neg != Lj.neg:
                θ = unify_tuple(Li.args, Lj.args)
                if θ is None:
                    continue
                resolvent = frozenset((ci | cj) - {Li, Lj})
                resolvent = frozenset(L.substitute(θ) for L in resolvent)
                if any(L.negate() in resolvent for L in resolvent):
                    continue
                yield resolvent


def prove(kb: list[Clause], neg_goal: Clause):
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
                if not R:
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
    "¬Mythical(x) | Immortal(x)",
    "Mythical(x) | Mortal(x)",
    "Mythical(x) | Mammal(x)",
    "¬Immortal(x) | Horned(x)",
    "¬Mammal(x) | Horned(x)",
    "¬Horned(x) | Magical(x)",
]

KB: list[Clause] = [parse_clause(line) for line in KB_TEXT]
NEGATED_GOAL: Clause = parse_clause("¬Magical(Unicorn)")


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Script entry point                                                    │
# ╰────────────────────────────────────────────────────────────────────────╯
if __name__ == "__main__":
    prove(KB, NEGATED_GOAL)

