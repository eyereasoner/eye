#!/usr/bin/env python3
"""
Graph reachability — ARC (Answer / Reason / Check), self-contained

Query:
  ?- path(X, nantes)

Rules:
  C1: path(U,V) :- oneway(U,V).
  C2: path(U,V) :- oneway(U,Z), path(Z,V).

Answer prints all X that reach 'nantes' with one shortest chain.
Reason why shows a backward-chaining proof for X = paris.
Check (harness) independently validates results by forward search and
verifies shortest-path minimality.
"""

from collections import defaultdict, deque
from itertools import count

# ───────────────────────────────── 0) Data ─────────────────────────────────
EDGES = [
    ("paris", "orleans"),
    ("paris", "chartres"),
    ("paris", "amiens"),
    ("orleans", "blois"),
    ("orleans", "bourges"),
    ("blois", "tours"),
    ("chartres", "lemans"),
    ("lemans", "angers"),
    ("lemans", "tours"),
    ("angers", "nantes"),
]
GOAL = "nantes"

oneway = set(EDGES)
succs = defaultdict(set)   # u -> {v}
preds = defaultdict(set)   # v -> {u}
NODES = set()
for u, v in oneway:
    succs[u].add(v)
    preds[v].add(u)
    NODES.add(u); NODES.add(v)

# ─────────────────────────────── 1) Reverse BFS ─────────────────────────────
def bfs_parents_and_dist(goal: str):
    """From goal, walk predecessors to find every node that can reach goal.
    Returns (parent, dist) where:
      • parent[u] = next hop on a shortest path u→…→goal
      • dist[u]   = shortest hop-count distance u→goal
    """
    parent = {}
    dist = {goal: 0}
    dq = deque([goal])
    while dq:
        v = dq.popleft()
        for u in sorted(preds[v]):            # sort for determinism
            if u not in dist:
                dist[u] = dist[v] + 1
                parent[u] = v                # first time seen = shortest
                dq.append(u)
    return parent, dist

PARENT, DIST = bfs_parents_and_dist(GOAL)

def shortest_path(src: str, goal: str):
    """Reconstruct shortest path using PARENT; assumes src reaches goal."""
    path = [src]
    while path[-1] != goal:
        path.append(PARENT[path[-1]])
    return path

# ───────────────────────── 2) Tiny backward chainer ─────────────────────────
def bc_path(goal, depth, seen_edges, step_counter):
    """Backward-chaining proof for path(U,V). Prints a trace; returns True/False."""
    u, v = goal
    indent = " " * depth
    print(f"{indent}Step {next(step_counter):02}: prove path({u}, {v})")

    # C1: base case
    if (u, v) in oneway:
        print(f"{indent}  ✓ by fact oneway({u}, {v})")
        return True

    # C2: recursive case
    for z in sorted(succs[u]):  # deterministic branching
        if (u, z) in seen_edges:
            continue
        print(f"{indent}  → try oneway({u}, {z}), then prove path({z}, {v})")
        if bc_path((z, v), depth + 1, seen_edges | {(u, z)}, step_counter):
            return True
    return False

# ──────────────────────────────── 3) ARC I/O ────────────────────────────────
def print_answer():
    print("Answer")
    print("======")
    print(f"Query: ?- path(X, {GOAL})\n")
    solutions = sorted(PARENT.keys())  # all nodes that reach GOAL
    if not solutions:
        print("No solutions.")
        return
    print("All solutions (with one shortest chain each):")
    for src in solutions:
        chain = " → ".join(shortest_path(src, GOAL))
        hops = len(chain.split(" → ")) - 1
        print(f"  X = {src:<9} path: {chain}   (hops = {hops})")
    print(f"\nTotal solutions: {len(solutions)}")

def print_reason():
    print("\nReason why")
    print("==========")
    print("We use two Horn clauses:")
    print("  C1  path(U,V) :- oneway(U,V).")
    print("  C2  path(U,V) :- oneway(U,Z), path(Z,V).")
    print("Backward-chaining tries C1 (a fact) or applies C2 by choosing a")
    print("successor Z of U and recursively proving path(Z,V).\n")

    demo_src = "paris"
    if demo_src not in PARENT:
        print(f"(Demo: {demo_src} does not reach {GOAL} in this graph.)")
        return
    print(f"Demo proof for X = {demo_src}:")
    steps = count(1)
    ok = bc_path((demo_src, GOAL), 0, frozenset(), steps)
    print("  ✔ PROVED" if ok else "  ✗ NOT PROVED")
    chain = " → ".join(shortest_path(demo_src, GOAL))
    print(f"Shortest chain: {chain}")

def print_check():
    print("\nCheck (harness)")
    print("===============")
    # A) Independent forward search from each node
    def reaches_goal_forward(u: str) -> bool:
        if u == GOAL:
            return False  # by convention we list X ≠ GOAL
        seen = {u}
        dq = deque([u])
        while dq:
            x = dq.popleft()
            for y in succs.get(x, ()):
                if y == GOAL:
                    return True
                if y not in seen:
                    seen.add(y); dq.append(y)
        return False

    forward_reach = {u for u in NODES if reaches_goal_forward(u)}
    solver_reach = set(PARENT.keys())

    same_set = forward_reach == solver_reach
    print(f"Forward search nodes == solver nodes ? {same_set}")
    if not same_set:
        print(f"  forward: {sorted(forward_reach)}")
        print(f"  solver : {sorted(solver_reach)}")

    # B) Validate each reported shortest path
    def is_valid_chain(chain):
        return all((chain[i], chain[i+1]) in oneway for i in range(len(chain)-1))

    all_valid = True
    all_minimal = True
    for src in sorted(solver_reach):
        chain = shortest_path(src, GOAL)
        if not is_valid_chain(chain):
            all_valid = False
            print(f"  invalid chain for {src}: {chain}")
        # minimality: hop-count equals reverse-BFS distance
        if len(chain) - 1 != DIST[src]:
            all_minimal = False
            print(f"  non-minimal chain for {src}: {chain} (dist={DIST[src]})")

    print(f"All chains follow oneway edges? {all_valid}")
    print(f"All chains are hop-minimal?    {all_minimal}")

# ────────────────────────────────── Main ────────────────────────────────────
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

