"""
water.py
========

Facts
-----
    inorganic_compound(water).

Exclusive choice
----------------
    For every inorganic compound A:
        exactly one of  {solid(A), liquid(A), gas(A)}  is true.
    We give each branch the same weight 1/3.

Rules
-----
    observable(A) ← solid(A) ∨ liquid(A) ∨ gas(A)

This script enumerates all mutually-exclusive worlds for *water*,
adds up the weights for the worlds in which the queried atom holds.

Expected output
---------------
prop(observable,water): 1.0
prop(solid,water): 0.3333333333333333
prop(liquid,water): 0.3333333333333333
prop(gas,water): 0.3333333333333333
"""

from typing import Dict, List, Tuple

# ───────────────────────────────────────────────────────────────
# 1 ▸  Define the annotated disjunction for “water”
# ───────────────────────────────────────────────────────────────
WORLD_PHASES: List[Tuple[str, float]] = [
    ("solid",  1.0 / 3.0),
    ("liquid", 1.0 / 3.0),
    ("gas",    1.0 / 3.0),
]

# Each tuple represents one *world*: the phase that is true for water
# together with its probability weight.


# ───────────────────────────────────────────────────────────────
# 2 ▸  Collect the probability of each queried atom
# ───────────────────────────────────────────────────────────────
def evaluate_queries() -> Dict[str, float]:
    """Return Prob(query_atom) for the four queries."""
    probs: Dict[str, float] = {
        "prop(observable,water)": 0.0,
        "prop(solid,water)":      0.0,
        "prop(liquid,water)":     0.0,
        "prop(gas,water)":        0.0,
    }

    for phase, weight in WORLD_PHASES:
        # Record the phase itself
        probs[f"prop({phase},water)"] += weight

        # observable(water) is true in *all* worlds
        probs["prop(observable,water)"] += weight

    return probs


# ───────────────────────────────────────────────────────────────
# 3 ▸  Main – enumerate worlds and print results
# ───────────────────────────────────────────────────────────────
def main() -> None:
    results = evaluate_queries()

    for q in (
        "prop(observable,water)",
        "prop(solid,water)",
        "prop(liquid,water)",
        "prop(gas,water)",
    ):
        print(f"{q}: {results[q]}")               # default float formatting


if __name__ == "__main__":
    main()

