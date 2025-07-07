#!/usr/bin/env python3
"""
Self-contained translation of
https://github.com/eyereasoner/eye/tree/master/reasoning/proof-by-cases

Implements a tiny forward-chaining reasoner plus a
*proof-by-cases* wrapper:

    – literals  :  predicate(args…)          (no negation needed here)
    – variables :  strings starting with “?” e.g. ?X
    – rules     :  body  ⇒  head             (lists of literals)
    – proof by cases:
          If  goal  is entailed in *every* case drawn from a
          pre-declared finite list, then  goal  is accepted.

Exactly the facts, rules and case-lists from the two EYE examples are
hard-coded below so the script needs **no external files or libraries**.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from copy import deepcopy
from typing import Dict, Iterable, List, Optional, Set, Tuple


# ───────────────────────────────────────── Basic term/literal
def is_var(t: object) -> bool:
    return isinstance(t, str) and t.startswith("?")


@dataclass(frozen=True)
class Literal:
    predicate: str
    args: Tuple[object, ...]

    def substitute(self, θ: Dict[str, object]) -> "Literal":
        """Apply substitution θ to the literal."""
        return Literal(
            self.predicate,
            tuple(θ.get(a, a) if is_var(a) else a for a in self.args),
        )

    def __str__(self) -> str:  # pretty-printer
        return f"{self.predicate}({', '.join(map(str, self.args))})"


@dataclass
class Rule:
    body: List[Literal]
    head: List[Literal]


# ───────────────────────────────────────── Unification helpers
def _unify_var(v: str, x: object, θ: Dict[str, object]) -> Optional[Dict[str, object]]:
    if v in θ:
        return unify_terms(θ[v], x, θ)
    if is_var(x) and x in θ:
        return unify_terms(v, θ[x], θ)
    θ2 = θ.copy()
    θ2[v] = x
    return θ2


def unify_terms(a: object, b: object, θ: Dict[str, object]) -> Optional[Dict[str, object]]:
    if θ is None:
        return None
    if a == b:
        return θ
    if is_var(a):
        return _unify_var(a, b, θ)
    if is_var(b):
        return _unify_var(b, a, θ)
    return None  # different constants


def unify_literals(pat: Literal, fact: Literal, θ: Dict[str, object]) -> Optional[Dict[str, object]]:
    if pat.predicate != fact.predicate or len(pat.args) != len(fact.args):
        return None
    for a, b in zip(pat.args, fact.args):
        θ = unify_terms(a, b, θ)
        if θ is None:
            return None
    return θ


def pattern_matches(body: List[Literal], facts: Set[Literal]) -> Iterable[Dict[str, object]]:
    """All substitutions satisfying an entire rule body."""

    def backtrack(i: int, θ: Dict[str, object]):
        if i == len(body):
            yield θ
            return
        pat = body[i]
        for fact in facts:
            θ2 = unify_literals(pat, fact, θ.copy())
            if θ2 is not None:
                yield from backtrack(i + 1, θ2)

    yield from backtrack(0, {})


# ───────────────────────────────────────── Knowledge base
class KnowledgeBase:
    def __init__(self):
        self.facts: Set[Literal] = set()
        self.rules: List[Rule] = []

    def add_fact(self, lit: Literal):
        self.facts.add(lit)

    def add_rule(self, rule: Rule):
        self.rules.append(rule)

    # simple forward chaining
    def saturate(self, *, max_iter: int = 5000):
        changed = True
        it = 0
        while changed and it < max_iter:
            it += 1
            changed = False
            snapshot = list(self.facts)          # avoid “set changed” trouble
            for rule in self.rules:
                for θ in pattern_matches(rule.body, set(snapshot)):
                    for h in rule.head:
                        g = h.substitute(θ)
                        if g not in self.facts:
                            self.facts.add(g)
                            changed = True
        if it >= max_iter:
            raise RuntimeError("forward chaining did not converge")


# ───────────────────────────────────────── Proof by cases
def prove_by_cases(base: KnowledgeBase, goal: Literal, cases: List[Literal]) -> bool:
    """
    Return True iff `goal` is entailed in *every* case.

    For each case literal  cᵢ:
        KB ∪ {cᵢ}  ⊨  goal   ?
    """
    for c in cases:
        kb = deepcopy(base)
        kb.add_fact(c)
        kb.saturate()
        if goal not in kb.facts:
            return False
    return True


# ───────────────────────────────────────── Build Example 1
def build_example1() -> Tuple[KnowledgeBase, List[str]]:
    kb = KnowledgeBase()
    X = "x"  # a single anonymous element

    # rules taken verbatim from example1.n3  
    kb.add_rule(Rule([Literal("Negative", (X,))], [Literal("isProvenFor", ("theorem1", X))]))
    kb.add_rule(Rule([Literal("Zero",     (X,))], [Literal("isProvenFor", ("theorem1", X))]))
    kb.add_rule(Rule([Literal("Positive", (X,))], [Literal("isProvenFor", ("theorem1", X))]))

    kb.add_rule(Rule([Literal("Negative", (X,))], [Literal("isProvenFor", ("theorem2", X))]))
    kb.add_rule(Rule([Literal("Positive", (X,))], [Literal("isProvenFor", ("theorem2", X))]))

    kb.add_rule(Rule([Literal("Negative", (X,))], [Literal("isProvenFor", ("theorem3", X))]))
    kb.add_rule(Rule([Literal("Zero",     (X,))], [Literal("isProvenFor", ("theorem3", X))]))
    kb.add_rule(Rule([Literal("Positive", (X,))], [Literal("isProvenFor", ("theorem3", X))]))

    kb.saturate()  # nothing happens yet (no case chosen)
    case_list = [Literal("Negative", (X,)), Literal("Zero", (X,)), Literal("Positive", (X,))]
    theorems = ["theorem1", "theorem2", "theorem3"]
    results = [
        prove_by_cases(kb, Literal("isProvenFor", (t, X)), case_list) for t in theorems
    ]
    return kb, list(zip(theorems, results))


# ───────────────────────────────────────── Build Example 2
def build_example2() -> Tuple[KnowledgeBase, bool]:
    kb = KnowledgeBase()
    A = "water"

    # facts & rules from example2.n3  
    kb.add_fact(Literal("InorganicCompound", (A,)))

    kb.add_rule(Rule([Literal("is", (A, "solid"))],  [Literal("is", (A, "observable"))]))
    kb.add_rule(Rule([Literal("is", (A, "liquid"))], [Literal("is", (A, "observable"))]))
    kb.add_rule(Rule([Literal("is", (A, "gas"))],    [Literal("is", (A, "observable"))]))

    kb.saturate()  # still no state chosen
    cases = [
        Literal("is", (A, "solid")),
        Literal("is", (A, "liquid")),
        Literal("is", (A, "gas")),
    ]
    goal = Literal("is", (A, "observable"))
    return kb, prove_by_cases(kb, goal, cases)


# ───────────────────────────────────────── Demo
if __name__ == "__main__":
    print("Proof-by-cases demonstration")

    # — Example 1 ——————————————————————————————
    _, results1 = build_example1()
    print("Example 1 (generic X)")
    for t, ok in results1:
        print(f"  {t} proven? ", ok)

    # — Example 2 ——————————————————————————————
    _, ok2 = build_example2()
    print("\nExample 2 (water)")
    print("  water observable?", ok2)

