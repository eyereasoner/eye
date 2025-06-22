"""
beetle.py
=========

Logic recap
-----------
    car(beetle).
    car → green ⊕ blue         (exclusive, 0.5 each)
    green ∨ blue → beautiful

Because every possible world contains either *green* or *blue*,
*beautiful* is certain (probability 1).

Queries
-------
    prop(beautiful,beetle)
    prop(green,beetle)
    prop(blue,beetle)
"""

from typing import Dict, List, Tuple

# ───────────────────────────────────────────────────────────────
# 1 ▸  Enumerate the two mutually exclusive worlds
# ───────────────────────────────────────────────────────────────
World = Tuple[str, float]          # (colour, probability weight)

WORLD_LIST: List[World] = [
    ("green", 0.5),
    ("blue",  0.5),
]

# ───────────────────────────────────────────────────────────────
# 2 ▸  Collect probabilities for the three queries
# ───────────────────────────────────────────────────────────────
def evaluate_queries(worlds: List[World]) -> Dict[str, float]:
    probs = {
        "prop(beautiful,beetle)": 0.0,
        "prop(green,beetle)":     0.0,
        "prop(blue,beetle)":      0.0,
    }

    for colour, weight in worlds:
        # colour itself
        probs[f"prop({colour},beetle)"] += weight
        # beauty rule (holds in both worlds)
        probs["prop(beautiful,beetle)"] += weight

    return probs


# ───────────────────────────────────────────────────────────────
# 3 ▸  Main
# ───────────────────────────────────────────────────────────────
if __name__ == "__main__":
    results = evaluate_queries(WORLD_LIST)

    for q in (
        "prop(beautiful,beetle)",
        "prop(green,beetle)",
        "prop(blue,beetle)",
    ):
        print(f"{q}: {results[q]}")

