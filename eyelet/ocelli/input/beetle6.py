"""
beetle6.py
==========

Facts
-----
    car(beetle).

Exclusive choices (annotated disjunctions, weight ½ each)
---------------------------------------------------------
    green ∨ blue
    nice  ∨ pretty    (only if green)

Deterministic rules
-------------------
    beautiful ← blue
    beautiful ← pretty

Integrity constraint
--------------------
    bad ← car ∧ beautiful
    evidence(bad,false).   →  drop every world where 'bad' holds
                             and renormalise the remaining weights.

Queries
-------
    prop(nice,beetle)
    prop(beautiful,beetle)
    prop(blue,beetle)
"""

from typing import List, Dict, TypedDict


class World(TypedDict):
    nice: bool
    beautiful: bool
    blue: bool
    weight: float


# ───────────────────────────────────────────────────────────────
# 1 ▸  Enumerate all worlds before the evidence filter
# ───────────────────────────────────────────────────────────────
def generate_worlds() -> List[World]:
    worlds: List[World] = []

    # Root choice: colour (green or blue)
    for colour in ("green", "blue"):
        p_colour = 0.5

        if colour == "blue":
            worlds.append(
                World(nice=False, beautiful=True, blue=True, weight=p_colour)
            )
            continue

        # colour == green ➜ secondary choice: nice or pretty
        for quality in ("nice", "pretty"):
            p_qual = p_colour * 0.5
            worlds.append(
                World(
                    nice=(quality == "nice"),
                    beautiful=(quality == "pretty"),   # pretty ⇒ beautiful
                    blue=False,
                    weight=p_qual,
                )
            )

    return worlds


# ───────────────────────────────────────────────────────────────
# 2 ▸  Apply the evidence (bad must be false) and renormalise
# ───────────────────────────────────────────────────────────────
def apply_evidence(worlds: List[World]) -> List[World]:
    """Throw away worlds where bad holds (beautiful = True)."""
    kept = [w for w in worlds if not w["beautiful"]]  # bad ← beautiful
    total = sum(w["weight"] for w in kept)
    # Renormalise so the remaining worlds sum to probability 1
    for w in kept:
        w["weight"] /= total
    return kept


# ───────────────────────────────────────────────────────────────
# 3 ▸  Compute query probabilities after evidence filtering
# ───────────────────────────────────────────────────────────────
def evaluate_queries(worlds: List[World]) -> Dict[str, float]:
    probs = {
        "prop(nice,beetle)":       0.0,
        "prop(beautiful,beetle)":  0.0,
        "prop(blue,beetle)":       0.0,
    }

    for w in worlds:
        if w["nice"]:
            probs["prop(nice,beetle)"] += w["weight"]
        if w["beautiful"]:
            probs["prop(beautiful,beetle)"] += w["weight"]
        if w["blue"]:
            probs["prop(blue,beetle)"] += w["weight"]

    return probs


# ───────────────────────────────────────────────────────────────
# 4 ▸  Main
# ───────────────────────────────────────────────────────────────
def main() -> None:
    worlds = generate_worlds()
    worlds = apply_evidence(worlds)          # enforce evidence(bad,false)
    results = evaluate_queries(worlds)

    for q in (
        "prop(nice,beetle)",
        "prop(beautiful,beetle)",
        "prop(blue,beetle)",
    ):
        print(f"{q}: {results[q]}")

if __name__ == "__main__":
    main()

