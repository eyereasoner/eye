#!/usr/bin/env python3
"""
beetle.py
Back-chaining proof for the simple Beetle example.

Facts
-----
    car(beetle).

Exclusive choice (annotated disjunction, 0.5 each)
    green(beetle)  ⊕  blue(beetle)

Rule
----
    beautiful(X) ← green(X) ∨ blue(X)

Worlds
------
W₁:  green  (weight 0.5)
W₂:  blue   (weight 0.5)
"""

from typing import Dict, List, Tuple

# ─────────────────────────────────────────────────────────────
# 1.  Two mutually-exclusive worlds
# ─────────────────────────────────────────────────────────────
World = Tuple[str, float]     # (colour, probability)
WORLDS: List[World] = [("green", 0.5), ("blue", 0.5)]

# ─────────────────────────────────────────────────────────────
# 2.  Atom truth lookup inside a world
# ─────────────────────────────────────────────────────────────
def atom_true(atom: str, colour: str) -> bool:
    """Return whether atom holds in the world whose colour is `colour`."""
    if atom == "green":
        return colour == "green"
    if atom == "blue":
        return colour == "blue"
    if atom == "beautiful":
        return colour in ("green", "blue")          # rule satisfied
    return False

# ─────────────────────────────────────────────────────────────
# 3.  Backward-proof for one query
# ─────────────────────────────────────────────────────────────
def backward_prove(atom: str) -> float:
    prob = 0.0
    print(f"\n=== Proving prop({atom},beetle) ===")
    for idx, (colour, weight) in enumerate(WORLDS, 1):
        holds = atom_true(atom, colour)
        print(f"World {idx}: colour={colour:<5} weight={weight:.3f}   "
              f"{'✓' if holds else '✗'} {atom}")
        if holds:
            prob += weight

    print(f"Probability = {prob}\n")
    return prob

# ─────────────────────────────────────────────────────────────
# 4.  Run all three queries and summarise
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    results: Dict[str, float] = {}
    for goal in ("beautiful", "green", "blue"):
        results[f"prop({goal},beetle)"] = backward_prove(goal)

    print("=== Summary ===")
    for q in ("prop(beautiful,beetle)",
              "prop(green,beetle)",
              "prop(blue,beetle)"):
        print(f"{q}: {results[q]}")

