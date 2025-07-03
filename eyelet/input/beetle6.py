#!/usr/bin/env python3
"""
beetle6_backward.py
Backward-proof version of beetle6.py (exclusive choices + evidence).

Worlds before evidence
----------------------
W1  blue                 weight 0.5
W2  green ∧ nice         weight 0.25
W3  green ∧ pretty       weight 0.25

Rule & evidence
---------------
beautiful ← blue ∨ pretty
bad       ← beautiful
evidence(bad,false)  ⇒  drop any world where beautiful holds.

So only W2 (“green ∧ nice”) survives and its weight renormalises to 1.0.

Queries
-------
prop(nice,beetle)        → 1.0
prop(beautiful,beetle)   → 0.0
prop(blue,beetle)        → 0.0
"""

from typing import List, Tuple, Dict

# ─────────────────────────────────────────────────────────────
# 1.  Enumerate the three exclusive worlds
# ─────────────────────────────────────────────────────────────
World = Tuple[str, float]   # (world_id, weight)

worlds: List[World] = [
    ("blue", 0.5),
    ("nice", 0.25),
    ("pretty", 0.25),
]

# Mapping from world_id to atom truth values
def atoms_in(world: str) -> Dict[str, bool]:
    return {
        "blue":       world == "blue",
        "green":      world in ("nice", "pretty"),
        "nice":       world == "nice",
        "pretty":     world == "pretty",
        # derived predicate
        "beautiful":  world == "blue" or world == "pretty",
    }

# ─────────────────────────────────────────────────────────────
# 2.  Apply evidence   bad ← beautiful  ;  evidence(bad,false)
# ─────────────────────────────────────────────────────────────
valid_worlds: List[Tuple[str,float]] = []
for w_id, wgt in worlds:
    if not atoms_in(w_id)["beautiful"]:      # bad must be false
        valid_worlds.append((w_id, wgt))

# Renormalise
total_w = sum(w for _, w in valid_worlds)
valid_worlds = [(wid, w / total_w) for wid, w in valid_worlds]

# ─────────────────────────────────────────────────────────────
# 3.  Backward-proof for a single query atom
# ─────────────────────────────────────────────────────────────
def prove(atom: str) -> float:
    prob = 0.0
    print(f"\n=== Proving prop({atom},beetle) ===")
    for idx, (wid, w) in enumerate(valid_worlds, 1):
        truth = atoms_in(wid)[atom]          # does atom hold in this world?
        print(f"World {idx}: {wid:<6}  weight={w:.3f}   "
              f"{'✓' if truth else '✗'} {atom}")

        if truth:
            prob += w
    print(f"Probability = {prob}\n")
    return prob

# ─────────────────────────────────────────────────────────────
# 4.  Run the three queries
# ─────────────────────────────────────────────────────────────
results = {
    "prop(nice,beetle)":      prove("nice"),
    "prop(beautiful,beetle)": prove("beautiful"),
    "prop(blue,beetle)":      prove("blue"),
}

# Summary
print("=== Summary ===")
for q in ("prop(nice,beetle)",
          "prop(beautiful,beetle)",
          "prop(blue,beetle)"):
    print(f"{q}: {results[q]}")

