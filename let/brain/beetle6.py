#!/usr/bin/env python3
"""
beetle6.py
Backward-proof (purely logical) version of beetle6.py:
exclusive choices + evidence

Worlds before evidence
----------------------
W1  blue
W2  green ∧ nice      (represented by world id "nice")
W3  green ∧ pretty    (represented by world id "pretty")

Rules
-----
beautiful ← blue ∨ pretty
bad       ← beautiful

Evidence
--------
evidence(bad,false)  ⇒  drop any world where beautiful holds.

Therefore only W2 (“green ∧ nice”) survives.

Queries
-------
prop(nice,beetle)
prop(beautiful,beetle)
prop(blue,beetle)

For each query we print a proof trace over the surviving worlds and
report whether the query is VALID / SATISFIABLE / UNSATISFIABLE.
"""

from typing import List, Dict

# ─────────────────────────────────────────────────────────────
# 1) Enumerate the three exclusive worlds
# ─────────────────────────────────────────────────────────────
World = str  # world id

WORLDS_BEFORE: List[World] = ["blue", "nice", "pretty"]


# Mapping from world_id to atom truth values (deterministic rules)
def atoms_in(world: World) -> Dict[str, bool]:
    """
    Truth assignment for primitive atoms and derived predicates
    under the given world id.
    """
    beautiful = (world == "blue") or (world == "pretty")
    return {
        "blue":       world == "blue",
        "green":      world in ("nice", "pretty"),
        "nice":       world == "nice",
        "pretty":     world == "pretty",
        "beautiful":  beautiful,
        "bad":        beautiful,   # bad ← beautiful
    }


# ─────────────────────────────────────────────────────────────
# 2) Apply evidence: bad must be false
#    ⇒ keep only worlds where beautiful is false.
# ─────────────────────────────────────────────────────────────
def apply_evidence(worlds: List[World]) -> List[World]:
    print("Applying evidence: bad ← beautiful ; evidence(bad,false)")
    kept: List[World] = []
    for idx, w in enumerate(worlds, 1):
        is_beautiful = atoms_in(w)["beautiful"]
        if is_beautiful:
            print(f"W{idx}: {w:<7}  beautiful holds ⇒ bad would be true ⇒ DROP")
        else:
            print(f"W{idx}: {w:<7}  beautiful false ⇒ bad false ⇒ KEEP")
            kept.append(w)
    if not kept:
        print("No worlds survive the evidence.")
    else:
        print("Surviving worlds:", ", ".join(kept))
    return kept


VALID_WORLDS: List[World] = apply_evidence(WORLDS_BEFORE)


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
    Print a proof trace for `atom` over the surviving worlds,
    and return the list of worlds where it holds.
    """
    print(f"\n=== Proving prop({atom},beetle) ===")
    if not VALID_WORLDS:
        print("No surviving worlds — query is UNSATISFIABLE by vacuity.")
        return []

    holds_in: List[World] = []
    for idx, w in enumerate(VALID_WORLDS, 1):
        truth = holds_in_world(atom, w)

        # Show reasons for the derived predicates where helpful
        reason = ""
        if atom == "beautiful":
            # After evidence, this should never hold, but explain anyway.
            if w == "blue":
                reason = " (because blue ⇒ beautiful)"
            elif w == "pretty":
                reason = " (because pretty ⇒ beautiful)"
        elif atom == "green":
            if w in ("nice", "pretty"):
                reason = " (green holds under nice/pretty)"

        print(f"World {idx}: {w:<7}  {'✓' if truth else '✗'} {atom}{reason}")
        if truth:
            holds_in.append(w)

    n = len(VALID_WORLDS)
    k = len(holds_in)
    if k == n:
        print(f"Result: VALID — prop({atom},beetle) is true in all {n} surviving worlds.")
    elif k == 0:
        print(f"Result: UNSATISFIABLE — prop({atom},beetle) is false in all {n} surviving worlds.")
    else:
        print(f"Result: SATISFIABLE (but not valid) — holds in {k}/{n} surviving worlds.")

    return holds_in


# ─────────────────────────────────────────────────────────────
# 4) Run the three queries & summarise (logical status only)
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    queries = ("nice", "beautiful", "blue")
    results: Dict[str, List[World]] = {f"prop({q},beetle)": prove(q) for q in queries}

    print("\n=== Summary ===")
    n = len(VALID_WORLDS)
    for q in ("prop(nice,beetle)",
              "prop(beautiful,beetle)",
              "prop(blue,beetle)"):
        worlds_where = results[q]
        k = len(worlds_where)
        if n == 0:
            status = "UNSATISFIABLE (no surviving worlds)"
        elif k == n:
            status = "VALID"
        elif k == 0:
            status = "UNSATISFIABLE"
        else:
            status = f"SATISFIABLE in {k}/{n} worlds"
        extra = "" if k in (0, n) else " → " + ", ".join(worlds_where)
        print(f"{q}: {status}{extra}")

