#!/usr/bin/env python3
"""
beetle12_backward.py
Back-chaining probability proof for the “Beetle-12” annotated-disjunction
example.

Exclusive choice tree for *beetle* (weights = 1/2 each branch):
    colour:  green | blue
    under green:  nice | pretty
    under nice/pretty:  1 | 2
    under x1/x2:        1 | 2      (nice11 … pretty22)

Worlds (leaf atoms & weights)
    blue                : 0.50
    nice11 nice12 …     : 0.0625 each  (8 leaves, total 0.50)

Deterministic rules
    beautiful ← blue
    beautiful ← any leaf starting with nice or pretty
"""

from typing import List, Tuple, Dict

# ───────────────────────────────────────────────────────────
# 1.  Enumerate exclusive worlds for *beetle*
# ───────────────────────────────────────────────────────────
World = Tuple[str, float]          # (leaf_atom , probability)

def worlds() -> List[World]:
    """Generate the 9 mutually-exclusive worlds."""
    out: List[World] = [("blue", 0.5)]        # blue ends the branch

    p_leaf = 0.5 * 0.5 * 0.5 * 0.5            # = 0.0625
    for qual in ("nice", "pretty"):
        for i in ("1", "2"):
            for j in ("1", "2"):
                out.append((f"{qual}{i}{j}", p_leaf))
    return out

WORLDS = worlds()

# ───────────────────────────────────────────────────────────
# 2.  Backward-proof routine for each query atom
# ───────────────────────────────────────────────────────────
def prove(goal: str) -> float:
    """
    Print a proof trace for `goal` (beautiful/green/blue) and
    return its probability.
    """
    total = 0.0
    print(f"\n=== Proving prop({goal},beetle) ===")
    for idx, (leaf, w) in enumerate(WORLDS, 1):
        print(f"World {idx:02}  leaf={leaf:<8} weight={w:.4f}")
        holds = False

        if goal == "blue":
            holds = (leaf == "blue")

        elif goal == "green":
            holds = (leaf != "blue")          # any leaf means green

        elif goal == "beautiful":
            if leaf == "blue":
                print("   reason: blue  ⇒ beautiful")
                holds = True
            elif leaf.startswith(("nice", "pretty")):
                print("   reason: leaf is nice*/pretty*  ⇒ beautiful")
                holds = True

        print("   ", "✓ holds" if holds else "✗ fails")
        if holds:
            total += w

    print(f"Probability = {total:.12f}")
    return total

# ───────────────────────────────────────────────────────────
# 3.  Run the three queries & summarise
# ───────────────────────────────────────────────────────────
if __name__ == "__main__":
    probs: Dict[str, float] = {}
    for atom in ("beautiful", "green", "blue"):
        probs[f"prop({atom},beetle)"] = prove(atom)

    print("\n=== Summary ===")
    for q in ("prop(beautiful,beetle)",
              "prop(green,beetle)",
              "prop(blue,beetle)"):
        print(f"{q}: {probs[q]}")

