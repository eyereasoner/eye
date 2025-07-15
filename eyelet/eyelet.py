#!/usr/bin/env python3
"""
eyelet.py – a *self‑contained* first‑order logic resolution prover
────────────────────────────────────────────────────────────────────────
This script demonstrates, line‑by‑line, how a **set‑of‑support (SOS) binary
resolution** engine works on *small* knowledge bases written in ordinary
ASCII text files.

INPUT FORMAT
-----------------------------------------------------------------------
* One **clause per line**. Literals are separated by the vertical bar
  ‘|’.  A literal that starts with ‘¬’ (or ‘~’) is *negative*.
* Variables are implicitly universally‑quantified and recognised by their
  lowercase initial letter (e.g. `x`, `driver2`).  Constants and functors
  can start with any other character.
* Lines beginning with “#” or blank lines are ignored.
* The last non‑comment line must start with
      GOAL: <single‑literal>
  This declares the query that should be proven *true*.  The program
  negates that literal internally, adds it to the set‑of‑support, and
  launches a refutation search.  If the empty clause ⊥ is derived, the
  goal is entailed by the KB.

USAGE
-----------------------------------------------------------------------
    python eyelet.py kb_file.txt

The output is a numbered derivation like
    01. ¬Blue(beetle) | Beautiful(beetle)   (from …)
    …
    09. ⊥                                   (from …)
followed by a success/failure message.
"""

from __future__ import annotations  # allows forward references in type hints

import sys              # For command‑line argument handling.
import re               # Regular expressions used in the tiny parser.
import itertools        # Utility for peeking into iterators.
import pathlib          # Portable filesystem path handling.

# ╭────────────────────────────────────────────────────────────────────────╮
# │  Lexical/Grammar basics                                                │
# ╰────────────────────────────────────────────────────────────────────────╯
# In *first‑order logic* a **variable** by convention starts with a lowercase
# letter, while constants / function symbols / predicate symbols usually start
# with an uppercase letter.  The next pattern recognises identifiers that
# *could* be variables – we rely on caller discipline for the rest.
#
# NOTE: We do *not* restrict constants/predicates to uppercase in code – it is
# purely a stylistic choice in the knowledge‑base.
# -------------------------------------------------------------------------
VAR = re.compile(r"^[a-z][A-Za-z0-9_]*$")


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Term, Literal and Clause data structures                              │
# ╰────────────────────────────────────────────────────────────────────────╯
# We implement three nested levels:
#   • Term    – variable, constant, or function application f(t1,…,tn)
#   • Literal – (possibly negated) predicate application P(t1,…,tn)
#   • Clause  – a finite *set* of literals, interpreted as their disjunction
#               Clause( {L1, L2, …, Ln} )  ≡  (L1 ∨ L2 ∨ … ∨ Ln)
#   • The *empty* clause  {}  is written "⊥" and represents contradiction.
#
# All objects are *immutable* (tuples, frozensets) so they can be freely hashed
# and stored in sets/dicts – very handy for the bookkeeping required by the
# resolution algorithm.
# -------------------------------------------------------------------------
class Term:
    """A **first‑order term**: variable, constant, or n‑ary function term."""

    __slots__ = ("functor", "args")  # memory optimisation – optional

    def __init__(self,
                 functor: str,
                 args: list["Term"] | tuple["Term", ...] | None = None):
        self.functor = functor               # the symbol name
        self.args = tuple(args or ())        # 0‑ary ⇒ constant/variable

    # ‑‑‑ Convenience helpers ‑‑‑
    def is_var(self) -> bool:
        """Return *True* iff this term syntactically looks like a variable."""
        return VAR.match(self.functor) and not self.args

    # ‑‑‑ Representation & value semantics (so Terms are hashable, comparable) ‑‑‑
    def __repr__(self) -> str:
        return (self.functor if not self.args else
                f"{self.functor}({', '.join(map(repr, self.args))})")

    def __hash__(self) -> int:
        return hash((self.functor, self.args))

    def __eq__(self, other: object) -> bool:
        return isinstance(other, Term) and (
            self.functor, self.args) == (other.functor, other.args)


class Literal:
    """An **atomic predicate** (optionally negated)."""

    __slots__ = ("pred", "args", "neg")

    def __init__(self,
                 pred: str,
                 args: list[Term],
                 neg: bool = False):
        self.pred = pred
        self.args = tuple(args)
        self.neg = neg           # True  →  literal is negated (¬P)

    # ‑‑‑ Small factory helpers ‑‑‑
    def negate(self) -> "Literal":
        """Return the complementary literal (¬L if L, else remove ¬)."""
        return Literal(self.pred, self.args, not self.neg)

    def substitute(self, theta: dict[Term, Term]) -> "Literal":
        """Apply a substitution θ to *all* term arguments."""
        return Literal(self.pred,
                       [substitute(a, theta) for a in self.args],
                       self.neg)

    # ‑‑‑ Representation & value semantics ‑‑‑
    def __repr__(self) -> str:
        return ("¬" if self.neg else "") + \
               f"{self.pred}({', '.join(map(repr, self.args))})"

    def __hash__(self) -> int:
        return hash((self.pred, self.args, self.neg))

    def __eq__(self, other: object) -> bool:
        return (isinstance(other, Literal) and
                (self.pred, self.args, self.neg) ==
                (other.pred, other.args, other.neg))


# A **clause** is simply an *immutable set* of literals.
Clause = frozenset[Literal]


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Parsing utilities                                                     │
# ╰────────────────────────────────────────────────────────────────────────╯
# We write a *very* small parser capable of understanding strings like
# "¬Square(a, f(b)) | Pythagoras(a, b, c)" in the knowledge‑base.
# No external parser‑generator needed – Python’s iterator protocol is enough.
# -------------------------------------------------------------------------

def tokenize(s: str):
    """Yield identifiers (A…Z a…z 0…9 _) and punctuation one by one."""
    for t in re.finditer(r"[A-Za-z0-9_]+|[(),]", s):
        yield t.group(0)


def peek(it):
    """Return the *next* element of an iterator **without** consuming it.
    Internally we create a tiny two‑part iterator: a buffer w/ the peeked token
    plus the remainder chain so the caller can continue reading seamlessly."""
    it = iter(it)                      # ensure 'it' is an iterator (not list)
    try:
        tok = next(it)
    except StopIteration:
        return None, iter(())          # exhausted
    return tok, itertools.chain([tok], it)


def parse_term(tokens) -> Term:
    """Recursive‑descent parser for *terms* (may contain nested sub‑terms)."""
    tok = next(tokens)                 # current identifier (variable/const)

    nxt, tokens = peek(tokens)         # look ahead – is this a function term?
    if nxt == '(':                     # yes → we have  functor(arg1,…,argN)
        next(tokens)                   # consume '('
        args: list[Term] = []
        nxt, _ = peek(tokens)
        if nxt != ')':                 # function of arity > 0
            while True:
                args.append(parse_term(tokens))  # recursive call
                nxt, _ = peek(tokens)
                if nxt == ',':
                    next(tokens)       # consume separator and loop
                    continue
                break
        next(tokens)                   # consume ')'
        return Term(tok, args)

    # simple variable or constant (0‑ary term)
    return Term(tok)


def parse_literal(text: str) -> Literal:
    """Convert one textual literal into an actual Literal object."""
    text = text.strip()

    # detect optional negation symbol at the front
    neg = text.startswith(('¬', '~'))
    if neg:
        text = text[1:].strip()

    # split head "Pred" from argument list "(… )"
    head, tail = text.split('(', 1)
    tokens = tokenize(tail[:-1])   # drop trailing ')'
    tokens = iter(tokens)          # guarantee iterator semantics for peek()

    # parse comma‑separated arguments (if any)
    args: list[Term] = []
    nxt, tokens = peek(tokens)
    if nxt is not None:
        while True:
            args.append(parse_term(tokens))
            nxt, _ = peek(tokens)
            if nxt == ',':
                next(tokens)
                continue
            break

    return Literal(head.strip(), args, neg)


def parse_clause(line: str) -> Clause:
    """Parse a **clause** written as literals separated by vertical bars.
    Example:  "¬P(x) | Q(x,y) | R" → frozenset{¬P(x), Q(x,y), R}"""
    return frozenset(parse_literal(part) for part in line.split('|'))


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Unification – heart of resolution                                     │
# ╰────────────────────────────────────────────────────────────────────────╯
# Unification tries to find a *substitution* θ that makes two terms syntactically
# identical.  We include the **occurs‑check** so that a variable can’t be
# unified with a term that *contains* that same variable (avoids endless terms).
# -------------------------------------------------------------------------

def substitute(t: Term, theta: dict[Term, Term]) -> Term:
    """Recursively apply substitution θ to a term (chase chained bindings)."""
    if t.is_var():
        # follow any variable bindings already recorded in θ
        seen: set[Term] = set()
        while t.is_var() and t in theta and t not in seen:
            seen.add(t)
            t = theta[t]
        return t

    # compound term: recurse on all sub‑arguments
    if t.args:
        return Term(t.functor, [substitute(a, theta) for a in t.args])

    # constant – nothing to do
    return t


def occurs(v: Term, t: Term, theta: dict[Term, Term]) -> bool:
    """Return True iff *variable* v occurs inside term t **after** subs."""
    if v == t:
        return True
    if t.is_var() and t in theta:
        return occurs(v, theta[t], theta)    # follow indirection
    return any(occurs(v, a, theta) for a in t.args) if t.args else False


def unify(x: Term, y: Term, theta: dict[Term, Term] | None = None):
    """Classic *Martelli‑Montanari* style unification (depth‑first)."""
    if theta is None:
        theta = {}

    # apply substitution already accumulated
    x = substitute(x, theta)
    y = substitute(y, theta)

    # identical → success, no new bindings
    if x == y:
        return theta

    # Case 1: x is a variable
    if x.is_var():
        if occurs(x, y, theta):        # occurs‑check – rejection
            return None
        theta[x] = y
        return theta

    # Case 2: y is a variable (symmetric)
    if y.is_var():
        if occurs(y, x, theta):
            return None
        theta[y] = x
        return theta

    # Case 3: two compound terms – must match functor & arity, then recurse
    if x.functor != y.functor or len(x.args) != len(y.args):
        return None

    for xi, yi in zip(x.args, y.args):
        theta = unify(xi, yi, theta)  # may return None
        if theta is None:
            return None
    return theta


def unify_tuple(a1: tuple[Term, ...], a2: tuple[Term, ...]):
    """Unify *lists* (tuples) of equal length argument‑wise."""
    theta: dict[Term, Term] = {}
    for s, t in zip(a1, a2):
        theta = unify(s, t, theta)
        if theta is None:
            return None
    return theta


# ╭────────────────────────────────────────────────────────────────────────╮
# │  Resolution engine                                                     │
# ╰────────────────────────────────────────────────────────────────────────╯
# We implement **binary resolution**:
#   Given two clauses Ci and Cj, choose complementary literals Li, Lj, find a
#   unifier θ, delete them, merge the remaining literals, apply θ – done.
#
# We adopt the *set‑of‑support* (SOS) strategy: only clauses that depend (even
# indirectly) on the *negated goal* may resolve further.  This keeps the search
# space finite in simple examples and ensures refutation completeness.
# -------------------------------------------------------------------------

def clause_str(c: Clause) -> str:
    """Pretty formatter for logging (empty clause → ⊥)."""
    return "⊥" if not c else " | ".join(sorted(map(repr, c)))


def resolve(ci: Clause, cj: Clause):
    """Yield all non‑tautological *resolvents* of Ci and Cj."""
    for Li in sorted(ci, key=repr):              # deterministic ordering
        for Lj in sorted(cj, key=repr):
            if Li.pred == Lj.pred and Li.neg != Lj.neg:
                theta = unify_tuple(Li.args, Lj.args)
                if theta is None:
                    continue                    # args couldn’t be unified

                # remove the complementary pair, merge rest, apply substitution
                resolvent = frozenset((ci | cj) - {Li, Lj})
                resolvent = frozenset(L.substitute(theta) for L in resolvent)

                # tautology check: skip clauses containing P | ¬P after rename
                if any(L.negate() in resolvent for L in resolvent):
                    continue

                yield resolvent


def prove(kb: list[Clause], neg_goal: Clause) -> bool:
    """Breadth‑first SOS *refutation* – returns True on contradiction (⊥)."""
    sos          = [neg_goal]               # queue – only SOS clauses reside
    all_clauses  = set(kb) | {neg_goal}    # for duplication checks
    step         = 0

    while sos:
        Ci = sos.pop(0)
        for Cj in sorted(all_clauses, key=clause_str):  # deterministic
            for R in resolve(Ci, Cj):
                if R in all_clauses:
                    continue               # already derived earlier

                step += 1
                print(f"{step:02d}. {clause_str(R)}   (from {clause_str(Ci)} , {clause_str(Cj)})")

                if not R:                  # empty clause ⊥ → contradiction!
                    print("\nEmpty clause derived — theorem proved.")
                    return True

                sos.append(R)              # keep deriving from SOS
                all_clauses.add(R)

    print("Proof failed — goal not entailed.")
    return False


# ╭────────────────────────────────────────────────────────────────────────╮
# │  File loading                                                          │
# ╰────────────────────────────────────────────────────────────────────────╯

def load_kb(path: str):
    kb_lines=[]; goal=None
    with open(path,'r',encoding='utf-8') as fh:
        for raw in fh:
            line=raw.strip()
            if not line or line.startswith('#'): continue
            if line.upper().startswith('GOAL:'):
                goal=line.split(':',1)[1].strip()
            else:
                kb_lines.append(line)
    if goal is None:
        raise ValueError("KB file must contain a line starting with 'GOAL:'")
    return kb_lines, goal

# ╭────────────────────────────────────────────────────────────────────────╮
# │  Main driver                                                           │
# ╰────────────────────────────────────────────────────────────────────────╯
if __name__=='__main__':
    file_path=sys.argv[1] if len(sys.argv)>1 else sys.exit("Usage: python eyelet.py kb_file.txt")
    if not pathlib.Path(file_path).exists():
        sys.exit(f"File '{file_path}' not found.")

    kb_lines, goal_lit = load_kb(file_path)
    KB = [parse_clause(l) for l in kb_lines]
    NEG_GOAL = parse_clause(f"¬{goal_lit}")   # refutation style

    print(f"Knowledge base loaded from '{file_path}'.  Goal: {goal_lit}\n")
    prove(KB, NEG_GOAL)

