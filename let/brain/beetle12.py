#!/usr/bin/env python3
"""
beetle12.py
Back-chaining *logical* proof for the “Beetle-12” annotated-disjunction example.

Exclusive choice tree for *beetle* (no weights, just exclusive branches):
    colour:  green | blue
    under green:  nice | pretty
    under nice/pretty:  1 | 2
    under x1/x2:        1 | 2      (nice11 … pretty22)

Worlds (leaf atoms)
    blue
    nice11 nice12 nice21 nice22 pretty11 pretty12 pretty21 pretty22

Deterministic rules
    beautiful ← blue
    beautiful ← any leaf starting with nice or pretty
"""

from typing import List, Dict

# ───────────────────────────────────────────────────────────
# 1.  Enumerate exclusive worlds for *beetle*
# ───────────────────────────────────────────────────────────
World = str  # a leaf atom naming an exclusive world

def worlds() -> List[World]:
    """Generate the 9 mutually-exclusive worlds."""
    out: List[World] = ["blue"]  # blue ends that branch
    for qual in ("nice", "pretty"):
        for i in ("1", "2"):
            for j in ("1", "2"):
                out.append(f"{qual}{i}{j}")
    return out

WORLDS = worlds()

# ───────────────────────────────────────────────────────────
# 2.  Logical entailment check per world (no numeric aggregation)
# ───────────────────────────────────────────────────────────
def holds_in_world(goal: str, leaf: World) -> bool:
    """
    Return True iff `goal` holds in the given world `leaf` under the rules.
    Allowed goals: 'beautiful', 'green', 'blue'.
    """
    if goal == "blue":
        return leaf == "blue"

    if goal == "green":
        # 'green' is any non-blue leaf
        return leaf != "blue"

    if goal == "beautiful":
        # Deterministic rules
        if leaf == "blue":
            return True
        if leaf.startswith(("nice", "pretty")):
            return True
        return False

    raise ValueError(f"Unknown goal: {goal!r}")

def prove(goal: str) -> List[World]:
    """
    Print a proof trace for `goal` (beautiful/green/blue)
    and return the list of worlds where it holds.
    """
    print(f"\n=== Proving prop({goal},beetle) ===")
    holds_in: List[World] = []

    for idx, leaf in enumerate(WORLDS, 1):
        print(f"World {idx:02}  leaf={leaf:<8}", end="  ")
        holds = holds_in_world(goal, leaf)

        # Show the reason for the *beautiful* rule cases
        if goal == "beautiful":
            if leaf == "blue":
                print("reason: blue ⇒ beautiful", end="  ")
            elif leaf.startswith(("nice", "pretty")):
                print("reason: nice*/pretty* ⇒ beautiful", end="  ")

        print("✓ holds" if holds else "✗ fails")

        if holds:
            holds_in.append(leaf)

    n = len(WORLDS)
    k = len(holds_in)
    if k == n:
        print(f"Result: VALID — prop({goal},beetle) is true in all {n} worlds.")
    elif k == 0:
        print(f"Result: UNSATISFIABLE — prop({goal},beetle) is false in all {n} worlds.")
    else:
        print(f"Result: SATISFIABLE (but not valid) — holds in {k}/{n} worlds.")

    return holds_in

# ───────────────────────────────────────────────────────────
# 3.  Run the three queries & summarise (logical status only)
# ───────────────────────────────────────────────────────────
if __name__ == "__main__":
    results: Dict[str, List[World]] = {}
    for atom in ("beautiful", "green", "blue"):
        results[f"prop({atom},beetle)"] = prove(atom)

    print("\n=== Summary ===")
    for q in ("prop(beautiful,beetle)",
              "prop(green,beetle)",
              "prop(blue,beetle)"):
        worlds_where = results[q]
        n = len(WORLDS)
        k = len(worlds_where)
        if k == n:
            status = "VALID"
        elif k == 0:
            status = "UNSATISFIABLE"
        else:
            status = f"SATISFIABLE in {k}/{n} worlds"
        print(f"{q}: {status}  {(' ' if k==n or k==0 else '→ ' + ', '.join(worlds_where))}")

