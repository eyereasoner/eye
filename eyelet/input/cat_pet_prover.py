#!/usr/bin/env python3
"""
cat_pet_prover.py
──────────────────
Deterministic set‑of‑support resolution proof that **Whiskers is a pet**.

Knowledge‑base (CNF)                                   English reading
─────────────────────────────────────────────────────  ─────────────────────────
1. ¬Cat(x)       ∨ Mammal(x)                           Cats are mammals
2. ¬Mammal(x)    ∨ Animal(x)                           Mammals are animals
3. ¬Carnivore(x) ∨ Animal(x)                           Carnivores are animals
4. ¬Animal(x) ∨ ¬Domestic(x) ∨ Pet(x)                  Domestic animals are pets
5. Cat(Whiskers)                                       Whiskers is a cat
6. Carnivore(Whiskers)                                 Whiskers is a carnivore
7. Domestic(Whiskers)                                  Whiskers is domestic

Goal (proved by refutation): **Pet(Whiskers)**

Expected output (always identical):

01. ¬Animal(Whiskers) | ¬Domestic(Whiskers)   (from ¬Pet(Whiskers) , Pet(x) | ¬Animal(x) | ¬Domestic(x))
02. ¬Carnivore(Whiskers) | ¬Domestic(Whiskers)   (from ¬Animal(Whiskers) | ¬Domestic(Whiskers) , Animal(x) | ¬Carnivore(x))
03. ¬Domestic(Whiskers) | ¬Mammal(Whiskers)   (from ¬Animal(Whiskers) | ¬Domestic(Whiskers) , Animal(x) | ¬Mammal(x))
04. ¬Animal(Whiskers)   (from ¬Animal(Whiskers) | ¬Domestic(Whiskers) , Domestic(Whiskers))
05. ¬Domestic(Whiskers)   (from ¬Carnivore(Whiskers) | ¬Domestic(Whiskers) , Carnivore(Whiskers))
06. ¬Carnivore(Whiskers)   (from ¬Carnivore(Whiskers) | ¬Domestic(Whiskers) , Domestic(Whiskers))
07. ¬Mammal(Whiskers)   (from ¬Domestic(Whiskers) | ¬Mammal(Whiskers) , Domestic(Whiskers))
08. ¬Cat(Whiskers) | ¬Domestic(Whiskers)   (from ¬Domestic(Whiskers) | ¬Mammal(Whiskers) , Mammal(x) | ¬Cat(x))
09. ⊥   (from ¬Domestic(Whiskers) , Domestic(Whiskers))

Empty clause derived – goal is proved.
"""

import re, itertools

# ──────────────────────────────  Basic data types  ───────────────────────────
VAR = re.compile(r"^[a-z][A-Za-z0-9_]*$")           # variable ⇒ lower‑case token


class Term:
    __slots__ = ("functor", "args")

    def __init__(self, functor, args=None):
        self.functor = functor
        self.args = tuple(args) if args else tuple()

    def is_var(self):  return VAR.match(self.functor) and not self.args

    def __repr__(self):
        return self.functor if not self.args else f"{self.functor}({', '.join(map(repr, self.args))})"

    def __hash__(self):  return hash((self.functor, self.args))
    def __eq__(self, o):  return (self.functor, self.args) == (o.functor, o.args)


class Literal:
    __slots__ = ("pred", "args", "neg")

    def __init__(self, pred, args, neg=False):
        self.pred, self.args, self.neg = pred, tuple(args), neg

    def negate(self):        return Literal(self.pred, self.args, not self.neg)
    def substitute(self, θ): return Literal(self.pred, [substitute(a, θ) for a in self.args], self.neg)

    def __repr__(self):
        return ("¬" if self.neg else "") + f"{self.pred}({', '.join(map(repr, self.args))})"

    def __hash__(self):  return hash((self.pred, self.args, self.neg))
    def __eq__(self, o):  return (self.pred, self.args, self.neg) == (o.pred, o.args, o.neg)


Clause = frozenset                                   # an immutable set of Literals

# ──────────────────────────────  Tiny parser  ────────────────────────────────
def tokenize(s):
    for t in re.finditer(r"[A-Za-z0-9_]+|[(),]", s):
        yield t.group(0)


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
        next(tokens)                                  # consume '('
        args = []
        if next_peek(tokens) != ')':
            while True:
                args.append(parse_term(tokens))
                sep = next(tokens)
                if sep == ')': break
                if sep != ',': raise ValueError("bad term syntax")
        else:
            next(tokens)                              # consume ')'
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
            if comma != ',': raise ValueError("bad literal syntax")
    return Literal(head, args, neg)


def parse_clause(line):  # e.g.  "A(x) | ¬B(x)"
    return frozenset(parse_literal(part) for part in line.split('|'))

# ──────────────────────────────  Unification  ───────────────────────────────
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


def occurs(v, t, θ):
    if v == t: return True
    if t.is_var() and t in θ: return occurs(v, θ[t], θ)
    return any(occurs(v, a, θ) for a in t.args) if t.args else False


def unify(x, y, θ=None):
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

# ───────────────────────  Deterministic resolution engine  ───────────────────
def clause_str(c):  return "⊥" if not c else " | ".join(sorted(map(repr, c)))


def resolve(ci, cj):
    for Li in sorted(ci, key=repr):
        for Lj in sorted(cj, key=repr):
            if Li.pred == Lj.pred and Li.neg != Lj.neg:
                θ = unify_tuple(Li.args, Lj.args)
                if θ is None: continue
                resolvent = frozenset((ci | cj) - {Li, Lj})
                resolvent = frozenset(L.substitute(θ) for L in resolvent)
                if any(L.negate() in resolvent for L in resolvent):  # tautology
                    continue
                yield resolvent


def prove(kb, neg_goal):
    sos = [neg_goal]                           # queue (FIFO) – deterministic
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

# ───────────────────────────  Embedded KB & goal  ───────────────────────────
KB_TEXT = [
    "¬Cat(x) | Mammal(x)",
    "¬Mammal(x) | Animal(x)",
    "¬Carnivore(x) | Animal(x)",
    "¬Animal(x) | ¬Domestic(x) | Pet(x)",
    "Cat(Whiskers)",
    "Carnivore(Whiskers)",
    "Domestic(Whiskers)",
]
KB = [parse_clause(line) for line in KB_TEXT]
NEGATED_GOAL = parse_clause("¬Pet(Whiskers)")

# ──────────────────────────────────  Main  ───────────────────────────────────
if __name__ == "__main__":
    prove(KB, NEGATED_GOAL)

