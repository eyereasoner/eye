#!/usr/bin/env python3
"""
diamond_property.py
Purely logical version of the diamond-property example.

Domain   : {a, b, c}  (fixed iteration order)
Base     : base_re(a,b), base_re(a,c)
Choice   : Each base edge independently labeled:
             e (equality, reflexive & symmetric) OR
             r (directed relation)
           → 4 possible worlds.

Rules
-----
  • e is reflexive (∀x. e(x,x)) and symmetric (e(x,y) ⇒ e(y,x))
  • re = e ∪ r, closed under:  e(x,y) ∧ re(y,z) ⇒ re(x,z)
  • Diamond-property violation (dp_viol):
      ∃x. r(x,y) ∧ r(x,z), with y≠z and y,z have no outgoing r-edges.
  • Evidence: keep only worlds with ¬dp_viol.

Query
-----
  goal :- ∃U. re(b,U) ∧ re(c,U)

We print a trace showing which worlds survive the evidence and whether the
goal holds in each surviving world, then classify the query as
VALID / SATISFIABLE / UNSATISFIABLE over the surviving worlds.
"""

from itertools import product
from typing import Dict, List, Set, Tuple, Optional

# ───────────────────────────────────────────────────────────────
# 1) Domain & Base (deterministic iteration order)
# ───────────────────────────────────────────────────────────────
DOM_ORDER: Tuple[str, ...] = ("a", "b", "c")  # fixed order for iteration/printing
DOM_SET: Set[str] = set(DOM_ORDER)            # for O(1) membership when needed

BASE_EDGES: List[Tuple[str, str]] = [("a", "b"), ("a", "c")]  # ordered

# ───────────────────────────────────────────────────────────────
# 2) Core Logic
# ───────────────────────────────────────────────────────────────
def closure_re(e: Set[Tuple[str, str]], r: Set[Tuple[str, str]]) -> Set[Tuple[str, str]]:
    """
    Compute re(x,z) by closing under:
        e(x,y) ∧ re(y,z) ⇒ re(x,z)
    with re initially e ∪ r.
    (Iteration order is made deterministic; result is the same.)
    """
    re = set(e) | set(r)
    changed = True
    while changed:
        changed = False
        # iterate in a deterministic order
        for (x, y1) in sorted(e):
            # snapshot of current re, iterated in a deterministic order
            for (y2, z) in sorted(list(re)):
                if y1 == y2 and (x, z) not in re:
                    re.add((x, z))
                    changed = True
    return re

def dp_violates(r: Set[Tuple[str, str]]) -> bool:
    """
    Diamond-property violation:
        ∃x. r(x,y) ∧ r(x,z) with y≠z, and y,z have no outgoing r-edges (r-leaves).
    """
    succ: Dict[str, Set[str]] = {n: set() for n in DOM_ORDER}  # deterministic key order
    for x, y in r:
        succ[x].add(y)

    for x in DOM_ORDER:  # deterministic iteration over x
        ys = sorted(succ[x])  # deterministic list of successors
        for i in range(len(ys)):
            for j in range(i + 1, len(ys)):
                y, z = ys[i], ys[j]
                if not succ[y] and not succ[z]:
                    return True
    return False

def goal_holds(re: Set[Tuple[str, str]]) -> Tuple[bool, Optional[str]]:
    """
    goal :- ∃U. re(b,U) ∧ re(c,U)
    Returns (holds?, witness_U_or_None).
    Witness selection uses DOM_ORDER for determinism.
    """
    for u in DOM_ORDER:
        if ("b", u) in re and ("c", u) in re:
            return True, u
    return False, None

# ───────────────────────────────────────────────────────────────
# 3) World Enumeration (no weights; deterministic order)
# ───────────────────────────────────────────────────────────────
def world_name(flags: Tuple[bool, bool]) -> str:
    """
    Name worlds by how each base edge is labeled:
      True  = equality (e), False = relation (r).
    """
    parts = []
    for (x, y), is_eq in zip(BASE_EDGES, flags):
        parts.append(f"{x}{y}={'e' if is_eq else 'r'}")
    return "[" + ", ".join(parts) + "]"

def enumerate_worlds() -> List[Dict]:
    """
    Generate all 4 worlds (edge labelings) in a deterministic order:
      flags iterate lexicographically over (True, False) × (True, False).
    Compute re, dp_viol, and goal.
    """
    worlds: List[Dict] = []
    for flags in product([True, False], repeat=len(BASE_EDGES)):  # (e/e), (e/r), (r/e), (r/r)
        # e is always reflexive; add symmetric pairs for e-labeled base edges
        e: Set[Tuple[str, str]] = {(x, x) for x in DOM_ORDER}  # reflexivity in fixed order
        r: Set[Tuple[str, str]] = set()

        for (a, b), is_eq in zip(BASE_EDGES, flags):
            if is_eq:
                e.add((a, b))
                e.add((b, a))  # symmetry for e
            else:
                r.add((a, b))  # directed

        re = closure_re(e, r)
        violates = dp_violates(r)
        holds, witness = goal_holds(re)

        worlds.append({
            "id": world_name(flags),
            "e": e,
            "r": r,
            "re": re,
            "violates": violates,
            "goal": holds,
            "witness": witness,
        })
    return worlds

# ───────────────────────────────────────────────────────────────
# 4) Evidence filtering and logical proof (deterministic printing)
# ───────────────────────────────────────────────────────────────
def apply_evidence(worlds: List[Dict]) -> List[Dict]:
    """
    Evidence: keep only worlds with no diamond-property violation.
    Printing is deterministic by construction of `worlds`.
    """
    print("Applying evidence: require ¬dp_viol (drop worlds that violate the diamond property).")
    kept: List[Dict] = []
    for i, w in enumerate(worlds, 1):
        if w["violates"]:
            print(f"W{i}: {w['id']:<18}  dp_viol = TRUE  → DROP")
        else:
            print(f"W{i}: {w['id']:<18}  dp_viol = FALSE → KEEP")
            kept.append(w)
    if not kept:
        print("No worlds survive the evidence.")
    else:
        print("Surviving worlds:", ", ".join(w["id"] for w in kept))
    return kept

def prove_goal(valid_worlds: List[Dict]) -> List[str]:
    """
    Print a proof trace for goal :- ∃U. re(b,U) ∧ re(c,U)
    over the surviving worlds and return the list of world ids where it holds.
    Witness selection is deterministic.
    """
    print("\n=== Proving goal: ∃U. re(b,U) ∧ re(c,U) ===")
    holds_in: List[str] = []
    if not valid_worlds:
        print("No surviving worlds — query is UNSATISFIABLE by vacuity.")
        return holds_in

    for i, w in enumerate(valid_worlds, 1):
        if w["goal"]:
            witness = w["witness"]
            print(f"World {i}: {w['id']:<18}  ✓ goal  (witness U = {witness})")
            holds_in.append(w["id"])
        else:
            print(f"World {i}: {w['id']:<18}  ✗ goal")

    n = len(valid_worlds)
    k = len(holds_in)
    if k == n:
        print(f"Result: VALID given evidence — goal holds in all {n} surviving worlds.")
    elif k == 0:
        print(f"Result: UNSATISFIABLE — goal fails in all {n} surviving worlds.")
    else:
        print(f"Result: SATISFIABLE (but not valid) — holds in {k}/{n} surviving worlds.")
    return holds_in

# ───────────────────────────────────────────────────────────────
# 5) Main
# ───────────────────────────────────────────────────────────────
if __name__ == "__main__":
    all_worlds = enumerate_worlds()
    valid_worlds = apply_evidence(all_worlds)

    # Deterministic summary of surviving E and R edges
    if valid_worlds:
        print("\nSurviving worlds detail (e and r edges):")
        for w in valid_worlds:
            e_edges = sorted(w["e"])  # deterministic
            r_edges = sorted(w["r"])  # deterministic
            print(f"  {w['id']}: e={e_edges}, r={r_edges}")

    # Prove the query over the surviving worlds
    holds_in = prove_goal(valid_worlds)

    # Summary
    print("\n=== Summary ===")
    total = len(all_worlds)
    valid = len(valid_worlds)
    invalid = total - valid
    print(f"Total worlds: {total}  |  Violating: {invalid}  |  Surviving: {valid}")
    if valid == 0:
        print("goal: UNSATISFIABLE (no surviving worlds)")
    else:
        k = len(holds_in)
        if k == valid:
            print("goal: VALID (in all surviving worlds)")
        elif k == 0:
            print("goal: UNSATISFIABLE (in surviving worlds)")
        else:
            print(f"goal: SATISFIABLE in {k}/{valid} surviving worlds")

