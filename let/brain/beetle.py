#!/usr/bin/env python3
"""
beetle.py
Back-chaining *logical* proof for the simple Beetle example.

Facts
-----
    car(beetle).

Exclusive choice (no weights, just exclusivity)
    green(beetle)  ⊕  blue(beetle)

Rule
----
    beautiful(X) ← green(X) ∨ blue(X)

Worlds
------
W1: green
W2: blue

For each query we print a trace over the worlds and then classify the result as
VALID / SATISFIABLE / UNSATISFIABLE.
"""

from typing import Dict, List

# ─────────────────────────────────────────────────────────────
# 1) Two mutually exclusive worlds
# ─────────────────────────────────────────────────────────────
World = str  # the colour that holds in that world for beetle
WORLDS: List[World] = ["green", "blue"]  # keep the W1/W2 order


# ─────────────────────────────────────────────────────────────
# 2) Atom truth lookup inside a world (deterministic rules only)
# ─────────────────────────────────────────────────────────────
def atoms_in(world: World) -> Dict[str, bool]:
    """
    Truth assignment for atoms, fixed entity X = beetle.
    """
    is_green = (world == "green")
    is_blue = (world == "blue")
    return {
        "car":       True,                     # fact: car(beetle)
        "green":     is_green,
        "blue":      is_blue,
        "beautiful": is_green or is_blue,      # beautiful(X) ← green(X) ∨ blue(X)
    }


# ─────────────────────────────────────────────────────────────
# 3) Backward-proof for a single query atom (logical only)
# ─────────────────────────────────────────────────────────────
def holds_in_world(atom: str, world: World) -> bool:
    mapping = atoms_in(world)
    if atom not in mapping:
        raise ValueError(f"Unknown atom: {atom!r}")
    return mapping[atom]


def prove(atom: str) -> List[World]:
    """
    Print a proof trace for `atom` over the two worlds,
    and return the list of worlds where it holds.
    """
    print(f"\n=== Proving prop({atom},beetle) ===")
    holds_in: List[World] = []
    for idx, w in enumerate(WORLDS, 1):
        truth = holds_in_world(atom, w)
        reason = ""
        if atom == "beautiful":
            if w == "green":
                reason = " (because green ⇒ beautiful)"
            elif w == "blue":
                reason = " (because blue ⇒ beautiful)"
        print(f"World {idx}: colour={w:<5}  {'✓' if truth else '✗'} {atom}{reason}")
        if truth:
            holds_in.append(w)

    n = len(WORLDS)
    k = len(holds_in)
    if k == n:
        print(f"Result: VALID — prop({atom},beetle) is true in all {n} worlds.")
    elif k == 0:
        print(f"Result: UNSATISFIABLE — prop({atom},beetle) is false in all {n} worlds.")
    else:
        print(f"Result: SATISFIABLE (but not valid) — holds in {k}/{n} worlds.")
    return holds_in


# ─────────────────────────────────────────────────────────────
# 4) Run the three queries & summarise (logical status only)
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    queries = ("beautiful", "green", "blue")
    results = {f"prop({q},beetle)": prove(q) for q in queries}

    print("\n=== Summary ===")
    n = len(WORLDS)
    for q in ("prop(beautiful,beetle)",
              "prop(green,beetle)",
              "prop(blue,beetle)"):
        worlds_where = results[q]
        k = len(worlds_where)
        if k == n:
            status = "VALID"
        elif k == 0:
            status = "UNSATISFIABLE"
        else:
            status = f"SATISFIABLE in {k}/{n} worlds"
        extra = "" if k in (0, n) else " → " + ", ".join(worlds_where)
        print(f"{q}: {status}{extra}")

