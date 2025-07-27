#!/usr/bin/env python3
"""
Self-contained translation of
https://github.com/eyereasoner/eye/tree/master/reasoning/proof-by-contradiction

The script implements a tiny forward-chaining engine with
*reductio ad absurdum* (proof by contradiction):

    – literals   :  predicate(args…)  or the special atom FALSE
    – variables  :  strings that start with “?“, e.g. ?X
    – rules      :  body  ⇒  head      (lists of literals)
    – contradiction is signalled by deriving the atom FALSE

`prove_by_contradiction(kb, goal)` tries to prove `goal`:
    1.  If it is already entailed, succeed.
    2.  Otherwise, temporarily assume ¬goal; if that drives the
        knowledge base to a contradiction (derives FALSE), succeed.

The two EYE cases become:

Example 1
    FACT   Human(Socrates)
    RULE   Human(x)  ⇒  Mortal(x)
    RULE   Mortal(x) ⇒  FALSE
    GOAL   Mortal(Socrates)

Example 2
    FACT   notLessThan(n,0)   for n=0…4
    RULE   notLessThan(x,0) ⇒ Positive(x)
    RULE   Positive(x)      ⇒ FALSE
    GOAL   Positive(n)       for n=0…4
"""

from __future__ import annotations
from dataclasses import dataclass, field
from copy import deepcopy
from typing import Any, Dict, Iterable, List, Optional, Set, Tuple


# ──────────────────────────────────────────────────────────────
# Basic term & literal machinery
# ──────────────────────────────────────────────────────────────
def is_var(t: Any) -> bool:
    return isinstance(t, str) and t.startswith("?")


@dataclass(frozen=True)
class Literal:
    predicate: str
    args: Tuple[Any, ...] = field(default_factory=tuple)
    negated: bool = False  # we only use this for the temporary ¬goal assumption

    def substitute(self, θ: Dict[str, Any]) -> "Literal":
        """Apply a substitution θ to the literal."""
        new_args = tuple(θ.get(a, a) if is_var(a) else a for a in self.args)
        return Literal(self.predicate, new_args, self.negated)

    # Deriving (positive) FALSE signals contradiction
    @property
    def is_false(self) -> bool:
        return self.predicate == "FALSE" and not self.negated

    def __str__(self) -> str:  # pretty-printer
        if self.is_false:
            return "FALSE"
        sign = "¬" if self.negated else ""
        return f"{sign}{self.predicate}({', '.join(map(str, self.args))})"


@dataclass
class Rule:
    body: List[Literal]
    head: List[Literal]

    def __str__(self) -> str:
        b = ", ".join(map(str, self.body)) or "TRUE"
        h = ", ".join(map(str, self.head)) or "TRUE"
        return f"{b}  ⇒  {h}"


# ──────────────────────────────────────────────────────────────
# Unification helpers
# ──────────────────────────────────────────────────────────────
def _unify_var(v: str, x: Any, θ: Dict[str, Any]) -> Optional[Dict[str, Any]]:
    """Bind variable v to x, respecting an existing substitution θ."""
    if v in θ:
        return unify_terms(θ[v], x, θ)
    if is_var(x) and x in θ:
        return unify_terms(v, θ[x], θ)
    θ2 = θ.copy()
    θ2[v] = x
    return θ2


def unify_terms(a: Any, b: Any, θ: Dict[str, Any]) -> Optional[Dict[str, Any]]:
    """Robinson unification for two terms under θ."""
    if θ is None:
        return None
    if a == b:
        return θ
    if is_var(a):
        return _unify_var(a, b, θ)
    if is_var(b):
        return _unify_var(b, a, θ)
    return None  # different constants


def unify_literals(p: Literal, f: Literal, θ: Dict[str, Any]) -> Optional[Dict[str, Any]]:
    """Unify a pattern literal p with a ground fact f, extending θ."""
    if p.negated != f.negated or p.predicate != f.predicate or len(p.args) != len(f.args):
        return None
    for a, b in zip(p.args, f.args):
        θ = unify_terms(a, b, θ)
        if θ is None:
            return None
    return θ


def pattern_matches(body: List[Literal], facts: Set[Literal]) -> Iterable[Dict[str, Any]]:
    """All substitutions that satisfy an entire rule body."""
    def backtrack(i: int, θ: Dict[str, Any]):
        if i == len(body):
            yield θ
            return
        pat = body[i]
        for fact in facts:
            θ2 = unify_literals(pat, fact, θ.copy())
            if θ2 is not None:
                yield from backtrack(i + 1, θ2)

    yield from backtrack(0, {})


# ──────────────────────────────────────────────────────────────
# Knowledge base: forward chaining + contradiction detection
# ──────────────────────────────────────────────────────────────
class KnowledgeBase:
    def __init__(self):
        self.facts: Set[Literal] = set()
        self.rules: List[Rule] = []
        self.contradiction = False

    def add_fact(self, lit: Literal):
        if lit in self.facts:
            return
        self.facts.add(lit)
        if lit.is_false:
            self.contradiction = True

    def add_rule(self, rule: Rule):
        self.rules.append(rule)

    # simple semi-naïve forward chaining
    def saturate(self, *, max_iter: int = 10000):
        changed = True
        n = 0
        while changed and not self.contradiction and n < max_iter:
            n += 1
            changed = False
            snapshot = list(self.facts)  # avoid “set changed size” errors
            for rule in self.rules:
                for θ in pattern_matches(rule.body, set(snapshot)):
                    for lit in rule.head:
                        ground = lit.substitute(θ)
                        if ground not in self.facts:
                            self.add_fact(ground)
                            changed = True
        if n >= max_iter:
            raise RuntimeError("Forward chaining did not converge")


# ──────────────────────────────────────────────────────────────
# Proof-by-contradiction wrapper
# ──────────────────────────────────────────────────────────────
def prove_by_contradiction(base_kb: KnowledgeBase, goal: Literal) -> bool:
    """
    Return True iff  goal  is provable from  base_kb  by reductio ad absurdum.

    1.  Try ordinary forward chaining; success if goal derived.
    2.  Otherwise assume ¬goal, saturate, and see whether FALSE appears.
    """
    kb = deepcopy(base_kb)
    kb.saturate()
    if goal in kb.facts:
        return True

    # assume the opposite and retry
    kb.add_fact(Literal(goal.predicate, goal.args, negated=True))
    kb.saturate()
    return kb.contradiction


# ──────────────────────────────────────────────────────────────
# Building the two EYE test cases
# ──────────────────────────────────────────────────────────────
def build_example1():
    kb = KnowledgeBase()
    kb.add_fact(Literal("Human", ("Socrates",)))
    kb.add_rule(Rule([Literal("Human", ("?X",))], [Literal("Mortal", ("?X",))]))
    kb.add_rule(Rule([Literal("Mortal", ("?X",))], [Literal("FALSE")]))
    goal = Literal("Mortal", ("Socrates",))
    return kb, goal


def build_example2():
    kb = KnowledgeBase()
    for n in range(5):  # 0 … 4
        print(n)
        kb.add_fact(Literal("notLessThan", (n, 0)))
    kb.add_rule(
        Rule([Literal("notLessThan", ("?X", 0))], [Literal("Positive", ("?X",))])
    )
    kb.add_rule(Rule([Literal("Positive", ("?X",))], [Literal("FALSE")]))
    goals = [Literal("Positive", (n,)) for n in range(5)]
    return kb, goals


# ──────────────────────────────────────────────────────────────
# Command-line demo
# ──────────────────────────────────────────────────────────────
if __name__ == "__main__":
    print("Proof by contradiction demonstration")
    kb1, g1 = build_example1()
    print("Example 1: Socrates Mortal? ", prove_by_contradiction(kb1, g1))

    kb2, g2s = build_example2()
    ok = all(prove_by_contradiction(kb2, g) for g in g2s)
    print("Example 2: Positive(n) for n=0..4? ", ok)

