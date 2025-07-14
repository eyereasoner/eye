#!/usr/bin/env python3
"""
unicorn_prover.py
─────────────────
A minimal deterministic first‑order resolution prover that outputs the
complete goal‑directed proof for the “Unicorn is magical” example.

Knowledge‑base (CNF)             Query (proved by refutation)
──────────────────────────────── ─────────────────────────────
1. ¬Mythical(x) ∨ Immortal(x)    Magical(Unicorn)      ← we negate this
2. Mythical(x)  ∨ Mortal(x)
3. Mythical(x)  ∨ Mammal(x)
4. ¬Immortal(x) ∨ Horned(x)
5. ¬Mammal(x)   ∨ Horned(x)
6. ¬Horned(x)   ∨ Magical(x)

Expected output (always identical):

01. ¬Horned(Unicorn)   (from ¬Magical(Unicorn) , Magical(x) | ¬Horned(x))
02. ¬Immortal(Unicorn) (from ¬Horned(Unicorn) , Horned(x) | ¬Immortal(x))
03. ¬Mammal(Unicorn)   (from ¬Horned(Unicorn) , Horned(x) | ¬Mammal(x))
04. ¬Mythical(Unicorn) (from ¬Immortal(Unicorn) , Immortal(x) | ¬Mythical(x))
05. Mythical(Unicorn)  (from ¬Mammal(Unicorn) , Mythical(x) | Mammal(x))
06. Mammal(Unicorn)    (from ¬Mythical(Unicorn) , Mythical(x) | Mammal(x))
07. Mortal(Unicorn)    (from ¬Mythical(Unicorn) , Mythical(x) | Mortal(x))
08. ⊥                  (from ¬Mythical(Unicorn) , Mythical(Unicorn))

Empty clause derived – goal is proved.
"""

import re, itertools
# ─────────────────────────────────────────────────────────────────────────────
#  Tiny term / literal data types
# ─────────────────────────────────────────────────────────────────────────────
VAR = re.compile(r"^[a-z][A-Za-z0-9_]*$")           # variable ⇒ lower‑case token


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

    def substitute(self, θ):  # apply substitution
        return Literal(self.pred, [substitute(a, θ) for a in self.args], self.neg)

    def __repr__(self):
        return ("¬" if self.neg else "") + f"{self.pred}({', '.join(map(repr, self.args))})"

    def __hash__(self):  return hash((self.pred, self.args, self.neg))

    def __eq__(self, o):  return (self.pred, self.args, self.neg) == (o.pred, o.args, o.neg)


Clause = frozenset  # just an immutable set of Literal objects


# ─────────────────────────────────────────────────────────────────────────────
#  Parsing helpers  (very small but good enough for this demo)
# ─────────────────────────────────────────────────────────────────────────────
def tokenize(s):
    for tok in re.finditer(r"[A-Za-z0-9_]+|[(),]", s):
        yield tok.group(0)


def next_peek(it):
    try:
        tok = next(it)
    except StopIteration:
        return None
    it = itertools.chain([tok], it)        # put it back
    return tok


def parse_term(tokens):
    tok = next(tokens)
    if next_peek(tokens) == '(':           # function symbol
        next(tokens)                       # consume '('
        args = []
        if next_peek(tokens) != ')':       # any arguments?
            while True:
                args.append(parse_term(tokens))
                sep = next(tokens)
                if sep == ')': break
                if sep != ',': raise ValueError("expected ',' in term")
        else:
            next(tokens)                   # consume ')'
        return Term(tok, args)
    return Term(tok)                       # constant or variable


def parse_literal(text):
    text = text.strip()
    neg = text[0] in {'~', '¬'}
    if neg:
        text = text[1:].strip()
    head, tail = text.split('(', 1)
    arg_tokens = tokenize(tail[:-1])       # strip final ')'
    args = []
    if tail[:-1]:                          # non‑empty argument list
        while True:
            args.append(parse_term(arg_tokens))
            try:
                comma = next(arg_tokens)
            except StopIteration:
                break
            if comma != ',':
                raise ValueError("expected ',' between arguments")
    return Literal(head, args, neg)


def parse_clause(line):                    # "A(x) | ¬B(x)"
    return frozenset(parse_literal(part) for part in line.split('|'))


# ─────────────────────────────────────────────────────────────────────────────
#  Unification   (Robinson 1965, with occurs‑check)
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


def unify_tuple(args1, args2):
    θ = {}
    for a, b in zip(args1, args2):
        θ = unify(a, b, θ)
        if θ is None:
            return None
    return θ


# ─────────────────────────────────────────────────────────────────────────────
#  Resolution (set‑of‑support, deterministic)
# ─────────────────────────────────────────────────────────────────────────────
def clause_str(c):
    return "⊥" if not c else " | ".join(sorted(map(repr, c)))


def resolve(ci, cj):                       # deterministic ordering
    for Li in sorted(ci, key=repr):
        for Lj in sorted(cj, key=repr):
            if Li.pred == Lj.pred and Li.neg != Lj.neg:
                θ = unify_tuple(Li.args, Lj.args)
                if θ is None:
                    continue
                resolvent = frozenset(
                    (ci | cj) - {Li, Lj}
                )
                resolvent = frozenset(L.substitute(θ) for L in resolvent)
                # drop tautologies
                if any(L.negate() in resolvent for L in resolvent):
                    continue
                yield resolvent


def prove(kb, negated_goal):
    sos = [negated_goal]                   # queue keeps insertion order
    all_clauses = set(kb) | {negated_goal}
    derived_from = {}
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
                derived_from[resolvent] = (Ci, Cj)
                if not resolvent:          # empty clause found
                    print("\nEmpty clause derived – goal is proved.")
                    return True
                sos.append(resolvent)
                all_clauses.add(resolvent)
    print("Proof failed – goal not entailed.")
    return False


# ─────────────────────────────────────────────────────────────────────────────
#  Embedded knowledge‑base and (negated) goal
# ─────────────────────────────────────────────────────────────────────────────
KB_TEXT = [
    "¬Mythical(x) | Immortal(x)",
    "Mythical(x) | Mortal(x)",
    "Mythical(x) | Mammal(x)",
    "¬Immortal(x) | Horned(x)",
    "¬Mammal(x) | Horned(x)",
    "¬Horned(x) | Magical(x)",
]
KB = [parse_clause(line) for line in KB_TEXT]

NEGATED_GOAL = parse_clause("¬Magical(Unicorn)")


# ─────────────────────────────────────────────────────────────────────────────
#  Main
# ─────────────────────────────────────────────────────────────────────────────
if __name__ == "__main__":
    prove(KB, NEGATED_GOAL)

