#!/usr/bin/env python3
"""
water_logic.py
Backward-proof (purely logical) version of water.py.

Facts
-----
    inorganic_compound(water).

Exclusive choice (no weights, just exclusivity)
-----------------------------------------------
    exactly one of {solid(water), liquid(water), gas(water)} is true.
    (Three mutually exclusive worlds.)

Rule
----
    observable(A) ← solid(A) ∨ liquid(A) ∨ gas(A).

Queries
-------
    prop(observable,water)
    prop(solid,water)
    prop(liquid,water)
    prop(gas,water)

For each query we print a trace over the worlds and classify the result as
VALID / SATISFIABLE / UNSATISFIABLE.
"""

from typing import Dict, List, Tuple

# ─────────────────────────────────────────────────────────────
# 1) Worlds for the annotated disjunction
# ─────────────────────────────────────────────────────────────
World = str  # one of: "solid", "liquid", "gas"
WORLDS: List[World] = ["solid", "liquid", "gas"]


# ─────────────────────────────────────────────────────────────
# 2) Deterministic truth assignment inside a world
# ─────────────────────────────────────────────────────────────
def atoms_in(world: World, arg: str = "water") -> Dict[str, bool]:
    """
    Truth assignment for predicates about the constant 'water'
    under the given world (phase).
    """
    is_solid = world == "solid"
    is_liquid = world == "liquid"
    is_gas = world == "gas"
    # observable(A) ← solid(A) ∨ liquid(A) ∨ gas(A)
    is_observable = is_solid or is_liquid or is_gas

    return {
        "inorganic_compound": True,  # fact: inorganic_compound(water)
        "solid": is_solid,
        "liquid": is_liquid,
        "gas": is_gas,
        "observable": is_observable,
    }


# ─────────────────────────────────────────────────────────────
# 3) Parser and proof routine (logical only)
# ─────────────────────────────────────────────────────────────
def parse_query(q: str) -> Tuple[str, str]:
    """
    Parse 'prop(pred,arg)' and return (pred, arg).
    """
    q = q.strip()
    assert q.startswith("prop(") and q.endswith(")"), f"Bad query: {q}"
    inner = q[len("prop("):-1]
    pred, arg = inner.split(",", 1)
    return pred.strip(), arg.strip()

def holds_in_world(pred: str, world: World, arg: str) -> bool:
    mapping = atoms_in(world, arg)
    if pred not in mapping:
        raise ValueError(f"Unknown predicate: {pred!r}")
    return mapping[pred]

def prove(query: str) -> List[World]:
    """
    Print a proof trace for the ground query 'prop(pred,arg)' over all worlds,
    and return the list of worlds where it holds.
    """
    pred, arg = parse_query(query)
    print(f"\n=== Proof for prop({pred},{arg}) ===")
    holds_in: List[World] = []

    for idx, world in enumerate(WORLDS, 1):
        truth = holds_in_world(pred, world, arg)

        # Explanations for derived predicate 'observable'
        reason = ""
        if pred == "observable":
            if world == "solid":
                reason = " (solid ⇒ observable by rule)"
            elif world == "liquid":
                reason = " (liquid ⇒ observable by rule)"
            elif world == "gas":
                reason = " (gas ⇒ observable by rule)"

        print(f"World {idx}: phase={world:<6}  {'✓' if truth else '✗'} {pred}{reason}")

        if truth:
            holds_in.append(world)

    # Classification
    n = len(WORLDS)
    k = len(holds_in)
    if k == n:
        print(f"Result: VALID — prop({pred},{arg}) is true in all {n} worlds.")
    elif k == 0:
        print(f"Result: UNSATISFIABLE — prop({pred},{arg}) is false in all {n} worlds.")
    else:
        print(f"Result: SATISFIABLE (but not valid) — holds in {k}/{n} worlds.")

    return holds_in


# ─────────────────────────────────────────────────────────────
# 4) Execute all four queries & summarise (logical status only)
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    queries = (
        "prop(observable,water)",
        "prop(solid,water)",
        "prop(liquid,water)",
        "prop(gas,water)",
    )
    results: Dict[str, List[World]] = {q: prove(q) for q in queries}

    print("\n=== Summary ===")
    n = len(WORLDS)
    for q in queries:
        worlds_where = results[q]
        k = len(worlds_where)
        if k == n:
            status = "VALID"
            extra = ""
        elif k == 0:
            status = "UNSATISFIABLE"
            extra = ""
        else:
            status = f"SATISFIABLE in {k}/{n} worlds"
            extra = " → " + ", ".join(worlds_where)
        print(f"{q}: {status}{extra}")

