#!/usr/bin/env python3
"""
Bell‑type contradiction prover (deterministic first‑order resolution)
====================================================================
This stand‑alone script shows—purely in propositional/first‑order logic—why a
single *local‑realist* hidden‑variable assignment cannot reproduce the quartet
of pairwise correlations that quantum mechanics predicts for a maximally
entangled two‑qubit state (e.g. the singlet or CHSH/GHZ scenarios).

Logical encodings
-----------------
We label the four **binary** measurement outcomes as variables

    A0  – Alice’s outcome if she chooses setting 0
    A1  – Alice’s outcome if she chooses setting 1
    B0  – Bob’s   outcome if he chooses setting 0
    B1  – Bob’s   outcome if he chooses setting 1

A *local deterministic* hidden‑variable model would pre‑assign each of these
four variables a definite bit (0 or 1) **independently of the distant
party’s measurement choice**.  Quantum mechanics, however, predicts that a
maximally entangled state can satisfy the following correlations in a single
run of the experiment:

    (1)  A0 = B0
    (2)  A0 = B1
    (3)  A1 = B0
    (4)  A1 ≠ B1   (this is the CHSH/Clauser–Horne–Shimony–Holt clash)

If one tries to satisfy all four simultaneously with fixed 0/1 values, the
first three equalities chain together to imply A1 = B1, contradicting (4).

CNF translation
~~~~~~~~~~~~~~~
We use a unary predicate **T(x)** meaning "x takes the value 1".  Equality
constraints become pairs of clauses, e.g. A0 = B0 translates to

    (¬T(A0) ∨ T(B0)) ∧ (T(A0) ∨ ¬T(B0))

and the inequality A1 ≠ B1 translates to

    (¬T(A1) ∨ ¬T(B1)) ∧ (T(A1) ∨ T(B1)).

Goal and proof
--------------
*Goal.*  Show that the assumption **¬T(A1)** is inconsistent with the eight
clauses above.  (Equivalently, prove T(A1).)

A deterministic set‑of‑support resolution strategy derives the empty clause ⊥
in **five steps**:

    01. ¬T(B0)
    02. ¬T(A0)
    03. ¬T(B1)
    04.  T(A1)
    05.  ⊥

Line 05 establishes the contradiction: no deterministic local assignment can
fulfil the quantum correlations.
"""

import re, itertools

# ──────────────────────  Tiny term & literal data types  ──────────────────────
VAR_RE = re.compile(r"^[a-z][A-Za-z0-9_]*$")  # variables start lower‑case


class Term:
    """Constant, variable or (unused) functional term."""
    __slots__ = ("functor", "args")

    def __init__(self, functor, args=None):
        self.functor = functor
        self.args = tuple(args) if args else tuple()

    # A variable is identified purely by its *lower‑case* token with no args.
    def is_var(self):
        return VAR_RE.match(self.functor) and not self.args

    # Nicely formatted representation
    def __repr__(self):
        return self.functor if not self.args else f"{self.functor}({', '.join(map(repr, self.args))})"

    # Hash/eq make Term usable as dictionary key & set element
    def __hash__(self):
        return hash((self.functor, self.args))

    def __eq__(self, other):
        return (self.functor, self.args) == (other.functor, other.args)


class Literal:
    """Positive or negated predicate applied to terms."""
    __slots__ = ("pred", "args", "neg")

    def __init__(self, pred, args, neg=False):
        self.pred, self.args, self.neg = pred, tuple(args), neg

    def negate(self):
        return Literal(self.pred, self.args, not self.neg)

    def substitute(self, theta):
        return Literal(self.pred, [substitute(a, theta) for a in self.args], self.neg)

    def __repr__(self):
        return ("¬" if self.neg else "") + f"{self.pred}({', '.join(map(repr, self.args))})"

    def __hash__(self):
        return hash((self.pred, self.args, self.neg))

    def __eq__(self, other):
        return (self.pred, self.args, self.neg) == (other.pred, other.args, other.neg)


Clause = frozenset  # for clarity: a clause is an immutable set of Literals

# ───────────────────────────────  Micro parser  ──────────────────────────────

def parse_literal(text: str) -> Literal:
    """Parse a string like '¬T(A0)' or 'T(B1)' into a Literal object."""
    text = text.strip()
    neg = text.startswith(("¬", "~"))
    if neg:
        text = text[1:].strip()

    # Simple regex: predicate followed by parenthesised arg list.
    m = re.match(r"^([A-Za-z0-9_]+)\(([^)]*)\)$", text)
    if not m:
        raise ValueError(f"Bad literal syntax: {text}")

    pred, arg_str = m.group(1), m.group(2)
    args = [Term(a.strip()) for a in arg_str.split(',') if a.strip()]
    return Literal(pred, args, neg)


def parse_clause(line: str) -> Clause:
    """Split on '|' and parse each literal into a Clause."""
    return frozenset(parse_literal(part) for part in line.split('|'))

# ─────────────────────────────  Unification  ────────────────────────────────

def substitute(term: Term, theta):
    """Recursively apply substitution *theta* to a term."""
    if term.is_var():
        while term in theta:
            term = theta[term]
        return term
    if term.args:
        return Term(term.functor, [substitute(a, theta) for a in term.args])
    return term


def occurs(var: Term, term: Term, theta) -> bool:
    """Occurs‑check to prevent infinite substitutions."""
    if var == term:
        return True
    if term.is_var() and term in theta:
        return occurs(var, theta[term], theta)
    return any(occurs(var, a, theta) for a in term.args)


def unify(x: Term, y: Term, theta=None):
    """Standard Robinson unification with occurs‑check."""
    if theta is None:
        theta = {}
    x, y = substitute(x, theta), substitute(y, theta)
    if x == y:
        return theta
    if x.is_var():
        if occurs(x, y, theta):
            return None
        theta[x] = y
        return theta
    if y.is_var():
        if occurs(y, x, theta):
            return None
        theta[y] = x
        return theta
    if x.functor != y.functor or len(x.args) != len(y.args):
        return None
    for xi, yi in zip(x.args, y.args):
        theta = unify(xi, yi, theta)
        if theta is None:
            return None
    return theta


def unify_tuple(a1, a2):
    theta = {}
    for s, t in zip(a1, a2):
        theta = unify(s, t, theta)
        if theta is None:
            return None
    return theta

# ─────────────────────────  Deterministic resolution  ──────────────────────

def clause_str(clause):
    """Pretty‑print a clause; use '⊥' for the empty clause."""
    return "⊥" if not clause else " | ".join(sorted(map(repr, clause)))


def resolve(ci: Clause, cj: Clause):
    """Generate all resolvents of two clauses (deterministic order)."""
    for Li in sorted(ci, key=repr):
        for Lj in sorted(cj, key=repr):
            if Li.pred == Lj.pred and Li.neg != Lj.neg:
                theta = unify_tuple(Li.args, Lj.args)
                if theta is None:
                    continue
                resolvent = frozenset((ci | cj) - {Li, Lj})
                resolvent = frozenset(L.substitute(theta) for L in resolvent)
                # Skip tautologies like P | ¬P.
                if any(L.negate() in resolvent for L in resolvent):
                    continue
                yield resolvent


def prove(kb, negated_goal):
    """Set‑of‑support resolution with FIFO queue for determinism."""
    sos = [negated_goal]
    all_clauses = set(kb) | {negated_goal}
    step = 0

    while sos:
        Ci = sos.pop(0)
        for Cj in sorted(all_clauses, key=clause_str):
            for R in resolve(Ci, Cj):
                if R in all_clauses:
                    continue
                step += 1
                print(f"{step:02d}. {clause_str(R)}   (from {clause_str(Ci)} , {clause_str(Cj)})")
                if not R:  # empty clause => contradiction
                    print("\nEmpty clause derived – contradiction established.")
                    return True
                sos.append(R)
                all_clauses.add(R)
    print("No contradiction found (should not happen in this Bell example).")
    return False

# ───────────────────  Knowledge‑base (eight clauses)  ──────────────────────
# Each pair encodes equality or inequality of two bits as explained above.
KB_TEXT = [
    # (1)  A0 = B0  →  two clauses
    "¬T(A0) | T(B0)",
    "T(A0)  | ¬T(B0)",

    # (2)  A0 = B1
    "¬T(A0) | T(B1)",
    "T(A0)  | ¬T(B1)",

    # (3)  A1 = B0
    "¬T(A1) | T(B0)",
    "T(A1)  | ¬T(B0)",

    # (4)  A1 ≠ B1  (inequality – the CHSH twist)
    "¬T(A1) | ¬T(B1)",
    "T(A1)  |  T(B1)",
]

KB = [parse_clause(line) for line in KB_TEXT]

# Negated goal: assume A1 is 0 ⇒ prove contradiction.
NEGATED_GOAL = parse_clause("¬T(A1)")

# ───────────────────────────────────  Main  ─────────────────────────────────
if __name__ == "__main__":
    prove(KB, NEGATED_GOAL)

