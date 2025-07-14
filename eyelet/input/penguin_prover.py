#!/usr/bin/env python3
"""
penguin_prover.py
──────────────────
Deterministic set-of-support resolution proof that *Polly the
penguin must waddle*.

Knowledge-base (CNF)                                  English reading
───────────────────────────────────────────────────── ──────────────────────────
1. ¬Bird(x) ∨ Animal(x)                               Every bird is an animal
2. ¬Penguin(x) ∨ Bird(x)                              Penguins are birds
3. ¬Penguin(x) ∨ ¬Fly(x)                              Penguins don’t fly
4. ¬Bird(x) ∨ Fly(x) ∨ Waddle(x)                      Birds that can’t fly waddle
5. Penguin(Polly)                                     Polly is a penguin

Goal (proved by refutation):  **Waddle(Polly)**

Expected output (always identical):

01. Fly(Polly) | ¬Bird(Polly)   (from ¬Waddle(Polly) , Fly(x) | Waddle(x) | ¬Bird(x))
02. Fly(Polly) | ¬Penguin(Polly)   (from Fly(Polly) | ¬Bird(Polly) , Bird(x) | ¬Penguin(x))
03. ¬Bird(Polly) | ¬Penguin(Polly)   (from Fly(Polly) | ¬Bird(Polly) , ¬Fly(x) | ¬Penguin(x))
04. Fly(Polly)   (from Fly(Polly) | ¬Penguin(Polly) , Penguin(Polly))
05. ¬Bird(Polly)   (from ¬Bird(Polly) | ¬Penguin(Polly) , Penguin(Polly))
06. ¬Penguin(Polly)   (from Fly(Polly) , ¬Fly(x) | ¬Penguin(x))
07. ⊥   (from ¬Penguin(Polly) , Penguin(Polly))

Empty clause derived – goal is proved.
"""

import re, itertools
# ─────────────────────────────────────────────────────────────────────────────
#  Tiny term / literal data types
# ─────────────────────────────────────────────────────────────────────────────
VAR = re.compile(r"^[a-z][A-Za-z0-9_]*$")             # variable ⇒ lower-case token


class Term:
    __slots__ = ("functor", "args")

    def __init__(self, functor, args=None):
        self.functor = functor
        self.args = tuple(args) if args else tuple()

    def is_var(self):
        return VAR.match(self.functor) and not self.args

    def __repr__(self):
        return (self.functor if not self.args
                else f"{self.functor}({', '.join(map(repr, self.args))})")

    def __hash__(self):  return hash((self.functor, self.args))

    def __eq__(self, other):  return (self.functor, self.args) == (other.functor, other.args)


class Literal:
    __slots__ = ("pred", "args", "neg")

    def __init__(self, pred, args, neg=False):
        self.pred, self.args, self.neg = pred, tuple(args), neg

    def negate(self):  return Literal(self.pred, self.args, not self.neg)

    def substitute(self, θ):
        return Literal(self.pred, [substitute(a, θ) for a in self.args], self.neg)

    def __repr__(self):
        return ("¬" if self.neg else "") + f"{self.pred}({', '.join(map(repr, self.args))})"

    def __hash__(self):  return hash((self.pred, self.args, self.neg))

    def __eq__(self, o):  return (self.pred, self.args, self.neg) == (o.pred, o.args, o.neg)


Clause = frozenset                                   # immutable set of Literals


# ─────────────────────────────────────────────────────────────────────────────
#  Parsing helpers
# ─────────────────────────────────────────────────────────────────────────────
def tokenize(s):
    for tok in re.finditer(r"[A-Za-z0-9_]+|[(),]", s):
        yield tok.group(0)


def next_peek(it):
    try:
        tok = next(it)
    except StopIteration:
        return None
    it = itertools.chain([tok], it)
    return tok


def parse_term(tokens):
    tok = next(tokens)
    if next_peek(tokens) == '(':
        next(tokens)                                     # consume '('
        args = []
        if next_peek(tokens) != ')':
            while True:
                args.append(parse_term(tokens))
                sep = next(tokens)
                if sep == ')':
                    break
                if sep != ',':
                    raise ValueError("expected ',' in term")
        else:
            next(tokens)                                 # consume ')'
        return Term(tok, args)
    return Term(tok)


def parse_literal(text):
    text = text.strip()
    neg = text[0] in {'~', '¬'}
    if neg:
        text = text[1:].strip()
    head, tail = text.split('(', 1)
    arg_tokens = tokenize(tail[:-1])
    args = []
    if tail[:-1]:
        while True:
            args.append(parse_term(arg_tokens))
            try:
                comma = next(arg_tokens)
            except StopIteration:
                break
            if comma != ',':
                raise ValueError("expected ',' between arguments")
    return Literal(head, args, neg)


def parse_clause(line):                                # "A(x) | ¬B(x)"
    return frozenset(parse_literal(part) for part in line.split('|'))


# ─────────────────────────────────────────────────────────────────────────────
#  Unification
# ─────────────────────────────────────────────────────────────────────────────
def substitute(term, θ):
    if term.is_var():
        seen = set()
        while term.is_var() and term in θ and term not in seen:
            seen.add(term)
            term = θ[term]
        return term
    if term.args:
        return Term(term.functor, [substitute(a, θ) for a in term.args])
    return term


def occurs(var, term, θ):
    if var == term:
        return True
    if term.is_var() and term in θ:
        return occurs(var, θ[term], θ)
    return any(occurs(var, a, θ) for a in term.args) if term.args else False


def unify(x, y, θ=None):
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


# ─────────────────────────────────────────────────────────────────────────────
#  Resolution
# ─────────────────────────────────────────────────────────────────────────────
def clause_str(c):
    return "⊥" if not c else " | ".join(sorted(map(repr, c)))


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
                    continue
                yield resolvent


def prove(kb, neg_goal):
    sos = [neg_goal]
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
                    print("\nEmpty clause derived – goal is proved.")
                    return True
                sos.append(resolvent)
                all_clauses.add(resolvent)
    print("Proof failed – goal not entailed.")
    return False


# ─────────────────────────────────────────────────────────────────────────────
#  Embedded KB and (negated) goal
# ─────────────────────────────────────────────────────────────────────────────
KB_TEXT = [
    "¬Bird(x) | Animal(x)",
    "¬Penguin(x) | Bird(x)",
    "¬Penguin(x) | ¬Fly(x)",
    "¬Bird(x) | Fly(x) | Waddle(x)",
    "Penguin(Polly)",
]
KB = [parse_clause(line) for line in KB_TEXT]

NEGATED_GOAL = parse_clause("¬Waddle(Polly)")


# ─────────────────────────────────────────────────────────────────────────────
#  Main
# ─────────────────────────────────────────────────────────────────────────────
if __name__ == "__main__":
    prove(KB, NEGATED_GOAL)

