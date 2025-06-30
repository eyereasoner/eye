#!/usr/bin/env python3
"""
Answer the Prolog-style query  ?- path(X, nantes).
For every solution X, print one shortest path X → … → nantes
as proof evidence.

No external libraries required – just the Python std-lib.
"""

from collections import defaultdict, deque

# ──────────────────────────────────────────────────────────────
# 1.  Ground “oneway” facts  (directed graph edges)
# ──────────────────────────────────────────────────────────────
EDGES: list[tuple[str, str]] = [
    ("paris",   "orleans"),
    ("paris",   "chartres"),
    ("paris",   "amiens"),
    ("orleans", "blois"),
    ("orleans", "bourges"),
    ("blois",   "tours"),
    ("chartres","lemans"),
    ("lemans",  "angers"),
    ("lemans",  "tours"),
    ("angers",  "nantes"),
]

GOAL = "nantes"

# Build forward and reverse adjacency lists once -----------------------------
succs: dict[str, set[str]] = defaultdict(set)   # u → {v, …}
preds: dict[str, set[str]] = defaultdict(set)   # v → {u, …}
for u, v in EDGES:
    succs[u].add(v)
    preds[v].add(u)

# ──────────────────────────────────────────────────────────────
# 2.  Reverse-BFS from the goal
#    parent[U] = V  means   U → … → V → … → GOAL   (shortest chain)
# ──────────────────────────────────────────────────────────────
def bfs_backwards(goal: str) -> dict[str, str]:
    parent: dict[str, str] = {}
    q: deque[str] = deque([goal])

    while q:
        v = q.popleft()
        for u in preds[v]:            # follow edge  u → v
            if u not in parent:       # not visited yet
                parent[u] = v
                q.append(u)
    return parent                     # keys = all sources that reach goal

# ──────────────────────────────────────────────────────────────
# 3.  Reconstruct one shortest path  src → … → goal
# ──────────────────────────────────────────────────────────────
def path_to_goal(src: str, goal: str, parent: dict[str, str]) -> list[str]:
    path = [src]
    while path[-1] != goal:
        path.append(parent[path[-1]])
    return path

# ──────────────────────────────────────────────────────────────
# 4.  Main
# ──────────────────────────────────────────────────────────────
if __name__ == "__main__":
    parent_map = bfs_backwards(GOAL)

    sources = sorted(parent_map.keys())        # all X with path(X, nantes)

    print("Solutions to  ?- path(X, nantes).   (and one proof each)\n")
    for src in sources:
        proof = path_to_goal(src, GOAL, parent_map)
        chain = " → ".join(proof)
        # left-align src in an 8-char field just for tidy columns
        print(f"X = {src:<8}   proof:  {chain}")

