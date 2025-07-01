#!/usr/bin/env python3
"""
graph_backward.py  –  Prolog-style proofs for  ?- path(X, nantes)

Rules
-----
C1  path(U,V) :- oneway(U,V).
C2  path(U,V) :- oneway(U,Z), path(Z,V).

Data
----
Directed oneway/2 edges (French cities, very small graph).

For every solution X the program:
    1. resets the proof step counter,
    2. prints a backward-chaining trace showing how  path(X,nantes)
       is established,
    3. prints ONE shortest path X → … → nantes as compact evidence.

No external libraries needed.
"""

from collections import defaultdict, deque
from itertools import count

# ── 0. Ground oneway facts ──────────────────────────────────────────────
EDGES = [
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

oneway = set(EDGES)                         # fact base
succs  = defaultdict(set)
preds  = defaultdict(set)
for u, v in oneway:
    succs[u].add(v)
    preds[v].add(u)

# ── 1. Pre-compute shortest-path parents (reverse BFS) ───────────────────
def bfs_parents(goal: str) -> dict[str, str]:
    parent = {}
    dq = deque([goal])
    while dq:
        v = dq.popleft()
        for u in preds[v]:
            if u not in parent:
                parent[u] = v
                dq.append(u)
    return parent

PARENT = bfs_parents(GOAL)

def shortest_path(src: str, goal: str) -> list[str]:
    path = [src]
    while path[-1] != goal:
        path.append(PARENT[path[-1]])
    return path

# ── 2. Unification helpers (only ground/fact vs. patterns) ───────────────
def unify_head(head: tuple[str, str], goal: tuple[str, str]):
    """Unify path(U,V) head with ground goal path(A,B)."""
    (u, v) = goal
    (H1, H2) = head
    θ = {}
    if H2 != v:                       # second arg is fixed in each call
        return None
    # H1 may be variable or constant
    if H1.startswith("?"):
        θ[H1] = u
    elif H1 != u:
        return None
    return θ

# ── 3. Backward-chaining path/2 prover ───────────────────────────────────
def bc_path(goal: tuple[str,str],
            depth: int,
            seen: set[tuple[str,str]],
            step_counter) -> bool:
    """Return True on first proof; print trace as it goes."""
    u, v = goal
    indent = "  " * depth
    print(f"{indent}Step {next(step_counter):02}: prove path({u}, {v})")

    # base case  C1  path(U,V) :- oneway(U,V).
    if (u, v) in oneway:
        print(f"{indent}  ✓ oneway fact")
        return True

    # recursive rule  C2
    for z in sorted(succs[u]):                 # deterministic order
        if (u, z) in seen:                     # avoid looping on same edge
            continue
        print(f"{indent}  → via oneway({u}, {z})")
        if bc_path((z, v), depth + 1, seen | {(u,z)}, step_counter):
            return True
    return False

# ── 4. Enumerate all X such that path(X, nantes) holds ───────────────────
solutions = sorted(PARENT.keys())             # nodes that reach GOAL

# ── 5. Proof for each solution ───────────────────────────────────────────
print(f"\n=== All proofs for  ?- path(X, {GOAL}) ===\n")

for src in solutions:
    print(f"\n--- Proof for X = {src} ---")
    steps = count(1)                          # reset numbering
    proved = bc_path((src, GOAL), 0, frozenset(), steps)
    print("✔ PROVED" if proved else "✗ NOT PROVED")
    chain = " → ".join(shortest_path(src, GOAL))
    print(f"Shortest chain: {chain}\n")

# ── 6. Compact summary table ─────────────────────────────────────────────
print("\n=== Summary ===")
for src in solutions:
    chain = " → ".join(shortest_path(src, GOAL))
    print(f"X = {src:<8}  path: {chain}")

