#!/usr/bin/env python3
"""
water_backward.py
Backward-proof version of water.py (annotated disjunction).

Facts
-----
    inorganic_compound(water).

Exclusive choice
----------------
    exactly one of {solid(water), liquid(water), gas(water)} is true,
    each with weight 1/3.  (Three mutually exclusive worlds.)

Rule
----
    observable(A) :- solid(A) ∨ liquid(A) ∨ gas(A).

Queries
-------
    prop(observable,water)
    prop(solid,water)
    prop(liquid,water)
    prop(gas,water)
"""

from typing import List, Tuple, Dict

# ─────────────────────────────────────────────────────────────
# 1.  Worlds for the annotated disjunction
# ─────────────────────────────────────────────────────────────
WORLD_PHASES: List[Tuple[str, float]] = [
    ("solid",  1/3),
    ("liquid", 1/3),
    ("gas",    1/3),
]

# ─────────────────────────────────────────────────────────────
# 2.  Backward-chaining proof per query
# ─────────────────────────────────────────────────────────────
def prove(query: str) -> float:
    """
    Backward-prove the atom in *query*.
    Prints a trace and returns its probability.
    """
    pred, arg = query.split('(')[1].rstrip(')').split(',')
    pred = pred.strip()            # solid / liquid / gas / observable
    arg  = arg.strip()             # water

    total_prob = 0.0
    print(f"\n=== Proof for prop({pred},{arg}) ===")
    for idx, (phase, weight) in enumerate(WORLD_PHASES, 1):
        print(f"World {idx}: phase = {phase:>6}, weight = {weight:.3f}")
        holds = False

        if pred == "observable":
            # observable(A) :- solid(A) ∨ liquid(A) ∨ gas(A)
            subgoal = phase                       # only that one is true
            print(f"  need solid/liq/gas → sub-goal {subgoal}({arg})")
            holds = True                          # sub-goal matches phase

        else:
            # direct fact check
            holds = (pred == phase)

        print("   ", "✓ holds" if holds else "✗ fails")
        if holds:
            total_prob += weight

    print(f"→  prob = {total_prob:.12f}")
    return total_prob

# ─────────────────────────────────────────────────────────────
# 3.  Execute all four queries
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    results: Dict[str, float] = {}

    for goal in ("observable", "solid", "liquid", "gas"):
        prob = prove(f"prop({goal},water)")
        results[f"prop({goal},water)"] = prob

    # nicely match the order of the original output
    print("\n=== Summary ===")
    for q in (
        "prop(observable,water)",
        "prop(solid,water)",
        "prop(liquid,water)",
        "prop(gas,water)",
    ):
        print(f"{q}: {results[q]}")

