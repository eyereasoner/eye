"""
diamond_property.py
───────────────────

Python implementation of the “Diamond property under reflexive closure” example.

Domain  : {a, b, c}
Base    : base_re(a,b), base_re(a,c)
Choice  : each base edge is classified as  e  (equality)  or  r  (relation)
          independently with P = 0.5.  →  4 possible worlds.

Rules   : • e is reflexive + symmetric
          • re = e ∪ r
          • congruence: e(x,y) ∧ re(y,z) ⇒ re(x,z)
          • diamond-property violation  dp_viol and eliminated via evidence.

Query   : goal :- dom(U), re(b,U), re(c,U).

Expected : goal has posterior probability 1.0 after conditioning on
           dp_viol = false.
"""

from itertools import product
from typing import Dict, List, Set, Tuple

# ───────────────────────────────────────────────────────────────
# 1 ▸  Static data
# ───────────────────────────────────────────────────────────────
DOM = {"a", "b", "c"}           # finite domain
BASE_EDGES = [("a", "b"), ("a", "c")]   # base_re/2 facts
WORLD_WEIGHT = 0.25             # 0.5 × 0.5 for each of the 4 worlds


# ───────────────────────────────────────────────────────────────
# 2 ▸  Helper: build e, r, re, then test dp_viol & goal
# ───────────────────────────────────────────────────────────────
def closure_re(e: Set[Tuple[str, str]], r: Set[Tuple[str, str]]) -> Set[Tuple[str, str]]:
    """Compute the least fixed point of re using congruence."""
    re = set(e) | set(r)
    changed = True
    while changed:
        changed = False
        for (x, y1) in e:
            for (y2, z) in list(re):         # iterate over a snapshot
                if y1 == y2 and (x, z) not in re:
                    re.add((x, z))
                    changed = True
    return re


def dp_violates(r: Set[Tuple[str, str]]) -> bool:
    """Return True iff the diamond-property violation predicate fires."""
    # quick index of r-successors
    succ = {n: set() for n in DOM}
    for x, y in r:
        succ[x].add(y)

    for x in DOM:
        ys = list(succ[x])
        for i in range(len(ys)):
            for j in range(i + 1, len(ys)):
                y, z = ys[i], ys[j]
                # violation if BOTH y and z have no outgoing r-edges
                if not succ[y] and not succ[z]:
                    return True
    return False


def goal_holds(re: Set[Tuple[str, str]]) -> bool:
    """goal :- dom(U), re(b,U), re(c,U)."""
    for u in DOM:
        if ("b", u) in re and ("c", u) in re:
            return True
    return False


# ───────────────────────────────────────────────────────────────
# 3 ▸  Enumerate the four possible worlds
# ───────────────────────────────────────────────────────────────
def enumerate_worlds() -> List[Dict]:
    """
    Return a list of worlds, each a dict with:
        e, r, re  (sets of edges)
        weight    (prior probability)
        valid     (False if dp_viol)
        goal      (boolean)
    """
    worlds: List[Dict] = []
    # choose classification (e vs r) for the two base edges
    for flags in product([True, False], repeat=2):   # True=equality, False=relation
        e: Set[Tuple[str, str]] = {(x, x) for x in DOM}   # reflexive
        r: Set[Tuple[str, str]] = set()

        # assign the two edges
        for (edge, is_eq) in zip(BASE_EDGES, flags):
            if is_eq:
                e.add(edge)
                e.add((edge[1], edge[0]))            # symmetry
            else:
                r.add(edge)

        # symmetry for any equality we just added (already done)

        re = closure_re(e, r)
        violates = dp_violates(r)
        goal = goal_holds(re)

        worlds.append(
            dict(e=e, r=r, re=re,
                 weight=WORLD_WEIGHT,
                 valid=not violates,
                 goal=goal)
        )
    return worlds


# ───────────────────────────────────────────────────────────────
# 4 ▸  Compute posterior P(goal) given evidence(dp_viol,false)
# ───────────────────────────────────────────────────────────────
def posterior_goal() -> float:
    worlds = enumerate_worlds()
    kept = [w for w in worlds if w["valid"]]      # filter evidence
    total_weight = sum(w["weight"] for w in kept)

    p_goal = sum(w["weight"] for w in kept if w["goal"]) / total_weight
    return p_goal


# ───────────────────────────────────────────────────────────────
# 5 ▸  Main
# ───────────────────────────────────────────────────────────────
if __name__ == "__main__":
    prob_goal = posterior_goal()
    print(f"goal: {prob_goal}")

