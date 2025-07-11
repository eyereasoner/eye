#!/usr/bin/env python3
"""
Self-contained translation of
https://github.com/eyereasoner/eye/tree/master/reasoning/proof-by-contrapositive

The original N3 uses rules *as data* plus two meta-rules:

    1.  (P ⇒ C)  ∧  (C ⇒ ⊥)          ⟹  (P ⇒ ⊥)         # contrapositive
    2.  ((P ⇒ ⊥) ⇒ ⊥)                 ⟹  P               # double negation

This script implements a tiny forward-chaining reasoner, then adds a
post-processing pass that *deforms* existing rules with those very two
schemata.  That is enough to reproduce the conclusions of EYE’s
`example1.n3` :contentReference[oaicite:0]{index=0} and `example2.n3` :contentReference[oaicite:1]{index=1}.
"""

from __future__ import annotations
from dataclasses import dataclass
from typing import Dict, List, Sequence, Set, Tuple

# ────────────────────────────────────────────  Basic terms
def is_var(x: object) -> bool:
    return isinstance(x, str) and x.startswith("?")

@dataclass(frozen=True)
class Lit:                            # ordinary literal  p(a,b,…)
    pred: str
    args: Tuple[object, ...]

    def subst(self, θ: Dict[str, object]) -> "Lit":
        return Lit(self.pred, tuple(θ.get(a, a) if is_var(a) else a for a in self.args))

    def __str__(self) -> str:
        return f"{self.pred}({', '.join(map(str, self.args))})"

FALSE = Lit("FALSE", ())

@dataclass(frozen=True)
class Rule:                           # body ⇒ head   (both lists)
    body: Tuple[object, ...]         # each item is Lit *or* Rule  (for meta-rules)
    head: Tuple[Lit, ...]

    def __str__(self) -> str:
        b = ", ".join(map(str, self.body)) or "TRUE"
        h = ", ".join(map(str, self.head)) or "TRUE"
        return f"{b}  ⇒  {h}"

# ────────────────────────────────────────────  Unification helpers
def _bind(var: str, val: object, θ: Dict[str, object]) -> Dict[str, object] | None:
    if var in θ:
        return unify(θ[var], val, θ)
    if is_var(val) and val in θ:
        return unify(var, θ[val], θ)
    θ2 = θ.copy(); θ2[var] = val
    return θ2

def unify(a: object, b: object, θ: Dict[str, object]) -> Dict[str, object] | None:
    if θ is None:               return None
    if a == b:                  return θ
    if is_var(a):               return _bind(a, b, θ)
    if is_var(b):               return _bind(b, a, θ)
    return None

def unify_lit(pat: Lit, fact: Lit) -> Dict[str, object] | None:
    if pat.pred != fact.pred or len(pat.args) != len(fact.args):
        return None
    θ: Dict[str, object] = {}
    for a, b in zip(pat.args, fact.args):
        θ = unify(a, b, θ)
        if θ is None: return None
    return θ

# ────────────────────────────────────────────  Knowledge base
class KB:
    def __init__(self):
        self.facts: Set[Lit]  = set()
        self.rules: Set[Rule] = set()

    # — plain forward chaining on ground rules (no vars in these examples) —
    def chase(self):
        changed = True
        while changed:
            changed = False
            for r in list(self.rules):
                if all(x in self.facts for x in r.body):
                    for h in r.head:
                        if h not in self.facts:
                            self.facts.add(h); changed = True

    # — meta-rule deformation pass (contrapositive & double negation) —
    def deform(self):
        new_rules: Set[Rule] = set()
        # contrapositive: (P⇒C) + (C⇒FALSE)  ➜  (P⇒FALSE)
        for r1 in self.rules:
            if len(r1.body)==1 and len(r1.head)==1:
                P = r1.body[0]
                C = r1.head[0]
                if isinstance(P, Lit) and isinstance(C, Lit):
                    for r2 in self.rules:
                        if r2.body==(C,) and r2.head==(FALSE,):
                            new_rules.add(Rule((P,), (FALSE,)))

        # double-neg: ((P⇒FALSE)⇒FALSE)  ➜  P
        for r in self.rules:
            if len(r.body)==1 and r.head==(FALSE,):
                sub = r.body[0]
                if isinstance(sub, Rule) and sub.head==(FALSE,) and len(sub.body)==1:
                    P = sub.body[0]
                    if isinstance(P, Lit):
                        self.facts.add(P)

        before = len(self.rules)
        self.rules |= new_rules
        return len(self.rules) != before    # True if something was added

    # — repeat until fixpoint —
    def saturate(self):
        while True:
            self.chase()
            if not self.deform():
                break

# ────────────────────────────────────────────  Build the two demos
def build_example1() -> KB:
    kb = KB()
    # “the ground is not wet”   Wet(ground) ⇒ FALSE
    kb.rules.add(Rule((Lit("is", ("ground", "wet")),), (FALSE,)))
    # “if it is raining, then the ground is wet”
    kb.rules.add(Rule((Lit("is", ("it", "raining")),), (Lit("is", ("ground", "wet")),)))
    # nothing else – meta-rules live in KB.deform()
    kb.saturate()
    return kb

def build_example2() -> KB:
    kb = KB()
    wet_false = Rule((Lit("is", ("ground", "wet")),), (FALSE,))
    # not wet  (same rule as above)
    kb.rules.add(wet_false)
    # not-not wet   (wet_false ⇒ FALSE)
    kb.rules.add(Rule((wet_false,), (FALSE,)))
    kb.saturate()
    return kb

# ────────────────────────────────────────────  Pretty demo
if __name__ == "__main__":
    print("Proof-by-contrapositive demonstration")

    # —— Example 1 ————————————————————————————————
    kb1 = build_example1()
    derived = [r for r in kb1.rules if r.head==(FALSE,) and
               len(r.body)==1 and isinstance(r.body[0], Lit)]
    print("\nExample 1  – derived rules of the form ‹something› ⇒ FALSE")
    sorted_derived = sorted(derived, key=str)
    for r in sorted_derived:
        print(" ", r)

    # —— Example 2 ————————————————————————————————
    kb2 = build_example2()
    print("\nExample 2  – facts after reasoning")
    for f in sorted(kb2.facts, key=str):
        print(" ", f)

