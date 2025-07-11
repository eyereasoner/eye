"""
diamond_property.py
───────────────────

Illustrates the "Diamond property under reflexive closure" from logic and knowledge representation.

Domain   : {a, b, c}
Base     : base_re(a,b), base_re(a,c)
Choice   : Each base edge is classified independently as either:
             e (equality, with reflexive/symmetric behavior) OR
             r (directed relation)
           → 2 edges × 2 choices → 4 possible worlds.

Rules    :
  • e is reflexive (∀x. e(x,x)) and symmetric (e(x,y) ⇒ e(y,x))
  • re = e ∪ r, closed under:  e(x,y) ∧ re(y,z) ⇒ re(x,z)
  • Diamond-property violation: ∃x. r(x,y), r(x,z), where y≠z and y,z are r-leaves.
  • Evidence: Only keep worlds where diamond-property is *not* violated.

Query    : goal :- ∃U. re(b,U) ∧ re(c,U)

Expected :
    After conditioning on no diamond-property violation (dp_viol = false),
    the goal should hold with posterior probability 1.0.
"""

from itertools import product
from typing import Dict, List, Set, Tuple

# ───────────────────────────────────────────────────────────────
# 1 ▸ Domain Setup
# ───────────────────────────────────────────────────────────────
DOM = {"a", "b", "c"}                           # finite domain
BASE_EDGES = [("a", "b"), ("a", "c")]           # edges in base relation
WORLD_WEIGHT = 0.25                             # uniform prior over 4 possible edge labelings


# ───────────────────────────────────────────────────────────────
# 2 ▸ Core Logic Helpers
# ───────────────────────────────────────────────────────────────
def closure_re(e: Set[Tuple[str, str]], r: Set[Tuple[str, str]]) -> Set[Tuple[str, str]]:
    """
    Compute re(x,z) by closing under:
        e(x,y) ∧ re(y,z) ⇒ re(x,z)
    """
    re = set(e) | set(r)
    changed = True
    while changed:
        changed = False
        for (x, y1) in e:
            for (y2, z) in list(re):
                if y1 == y2 and (x, z) not in re:
                    re.add((x, z))
                    changed = True
    return re


def dp_violates(r: Set[Tuple[str, str]]) -> bool:
    """
    Check for diamond-property violation:
        ∃x. r(x,y), r(x,z) with y ≠ z, and both y,z have no outgoing r-edges.
    """
    succ = {n: set() for n in DOM}
    for x, y in r:
        succ[x].add(y)

    for x in DOM:
        ys = list(succ[x])
        for i in range(len(ys)):
            for j in range(i + 1, len(ys)):
                y, z = ys[i], ys[j]
                if not succ[y] and not succ[z]:
                    return True
    return False


def goal_holds(re: Set[Tuple[str, str]]) -> bool:
    """
    goal :- ∃U. re(b,U) ∧ re(c,U)
    """
    for u in DOM:
        if ("b", u) in re and ("c", u) in re:
            return True
    return False


# ───────────────────────────────────────────────────────────────
# 3 ▸ World Enumeration
# ───────────────────────────────────────────────────────────────
def enumerate_worlds() -> List[Dict]:
    """
    Generate all possible worlds (edge labelings),
    compute re closure, check goal and dp_viol violation.
    """
    worlds: List[Dict] = []

    # Each base edge has 2 options: equality (e) or relation (r)
    for flags in product([True, False], repeat=2):  # 4 worlds total
        e: Set[Tuple[str, str]] = {(x, x) for x in DOM}   # reflexivity
        r: Set[Tuple[str, str]] = set()

        for (edge, is_eq) in zip(BASE_EDGES, flags):
            if is_eq:
                e.add(edge)
                e.add((edge[1], edge[0]))  # symmetry
            else:
                r.add(edge)

        re = closure_re(e, r)
        violates = dp_violates(r)
        goal = goal_holds(re)

        worlds.append({
            "e": e,
            "r": r,
            "re": re,
            "valid": not violates,
            "goal": goal,
            "weight": WORLD_WEIGHT
        })

    return worlds


# ───────────────────────────────────────────────────────────────
# 4 ▸ Posterior Inference
# ───────────────────────────────────────────────────────────────
def posterior_goal() -> float:
    """
    Compute posterior probability of goal after conditioning on dp_viol = false.
    """
    worlds = enumerate_worlds()
    valid_worlds = [w for w in worlds if w["valid"]]
    total_weight = sum(w["weight"] for w in valid_worlds)

    goal_weight = sum(w["weight"] for w in valid_worlds if w["goal"])
    return goal_weight / total_weight


# ───────────────────────────────────────────────────────────────
# 5 ▸ Main Entry Point with Explanation
# ───────────────────────────────────────────────────────────────
if __name__ == "__main__":
    worlds = enumerate_worlds()
    valid_worlds = [w for w in worlds if w["valid"]]
    invalid_worlds = [w for w in worlds if not w["valid"]]
    total_valid = len(valid_worlds)
    total_invalid = len(invalid_worlds)

    prob_goal = posterior_goal()

    print(f"Posterior P(goal | ¬dp_viol) = {prob_goal:.1f}")
    print("\nExplanation:")
    print("─────────────")
    print(f"Total possible worlds:       {len(worlds)}")
    print(f"Worlds violating dp_viol:    {total_invalid}")
    print(f"Worlds consistent with ¬dp:  {total_valid}")
    print(f"In all valid worlds, the goal holds: {[w['goal'] for w in valid_worlds]}")

    if all(w["goal"] for w in valid_worlds):
        print("\nSince all valid worlds satisfy the goal,")
        print("we conclude that P(goal | ¬dp_viol) = 1.0.")
    else:
        print("\nSome valid worlds do not satisfy the goal,")
        print(f"so P(goal | ¬dp_viol) = {prob_goal:.2f}")

