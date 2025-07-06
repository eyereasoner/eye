#!/usr/bin/env python3
"""
Show independent proof traces for the query

    ?- path(Gent, Oostende).

Constraints (as in Jos De Roo’s GPS demo):
    duration ≤ 5000
    cost     ≤ 5.0
    belief   ≥ 0.2
    comfort  ≥ 0.4
    stagecount ≤ 1   (all edges from same map)
"""

from dataclasses import dataclass
from itertools import count
from typing import List, Dict, Tuple, Callable

# ───────────────────────────────────────────────────────────────
# 1. Edge data: model the map as directed edges with properties
# ───────────────────────────────────────────────────────────────

@dataclass(frozen=True)
class Edge:
    map_id:  str    # Unique identifier for the map (e.g., region)
    src:     str    # Source node (city)
    dst:     str    # Destination node (city)
    action:  str    # Action description (used in proof trace)
    dur:     float  # Duration of edge traversal
    cost:    float  # Monetary cost of traversal
    belief:  float  # Probability/belief factor for edge
    comfort: float  # Comfort metric for traversal

# Edge list: define available connections in the map
edges: List[Edge] = [
    Edge("map-BE", "Gent",     "Brugge",   "drive_gent_brugge",     1500, 0.006, 0.96, 0.99),
    Edge("map-BE", "Gent",     "Kortrijk", "drive_gent_kortrijk",   1600, 0.007, 0.96, 0.99),
    Edge("map-BE", "Kortrijk", "Brugge",   "drive_kortrijk_brugge", 1600, 0.007, 0.96, 0.99),
    Edge("map-BE", "Brugge",   "Oostende", "drive_brugge_oostende",  900, 0.004, 0.98, 1.00),
]

# Build an adjacency map for efficient traversal
OUT: Dict[str, List[Edge]] = {}
for edge in edges:
    OUT.setdefault(edge.src, []).append(edge)

# ───────────────────────────────────────────────────────────────
# 2. Constraint helpers and path property computations
# ───────────────────────────────────────────────────────────────

LIMITS = dict(
    max_dur=5000.0,   # maximum allowed total duration
    max_cost=5.0,     # maximum allowed total cost
    min_bel=0.2,      # minimum allowed path belief
    min_comf=0.4,     # minimum allowed path comfort
    max_stage=1       # maximum allowed number of different maps ("stages")
)

def stagecount(maps: List[str]) -> int:
    """Count the number of consecutive unique map_ids (stages) in a path."""
    if not maps:
        return 0
    cnt = 1
    for prev, curr in zip(maps, maps[1:]):
        if curr != prev:
            cnt += 1
    return cnt

# ───────────────────────────────────────────────────────────────
# 3. Backward-chaining proof engine with trace
# ───────────────────────────────────────────────────────────────

def prove(start: str, goal: str) -> List[Tuple[List[str], float, float, float, float]]:
    """
    Find all admissible proofs (paths) from start to goal under constraints.

    Returns a list of tuples:
        (action_sequence, total_duration, total_cost, final_belief, final_comfort)

    Prints proof steps as it goes, showing goal-oriented, backward search.
    """
    step = count(1)
    solutions: List[Tuple[List[str], float, float, float, float]] = []

    def bc(
        state: str, dur: float, cost: float, bel: float, comf: float,
        maps: List[str], acts: List[str], depth: int, visited: set
    ) -> None:
        """Recursive backward-chaining search with proof printing."""
        indent = "  " * depth
        print(f"{indent}Step {next(step):02}: at {state}"
              f" (dur={dur}, cost={cost:.3f}, bel={bel:.3f}, comf={comf:.3f})")

        # Goal reached: check all constraints
        if state == goal:
            if (dur <= LIMITS["max_dur"] and cost <= LIMITS["max_cost"]
                and bel >= LIMITS["min_bel"] and comf >= LIMITS["min_comf"]
                and stagecount(maps) <= LIMITS["max_stage"]):
                print(f"{indent}✓ goal & constraints OK")
                solutions.append((acts, dur, cost, bel, comf))
            else:
                print(f"{indent}✗ constraints fail at goal")
            return

        # Cycle prevention: avoid revisiting same state in path
        if state in visited:
            print(f"{indent}↻ already visited {state}, skipping")
            return

        # Explore outgoing edges, sorted for deterministic proofs
        for e in sorted(OUT.get(state, []), key=lambda e: e.action):
            ndur  = dur  + e.dur
            ncost = cost + e.cost
            nbel  = bel  * e.belief
            ncomf = comf * e.comfort
            nmaps = maps + [e.map_id]
            nacts = acts + [e.action]

            # Early constraint pruning
            if (ndur > LIMITS["max_dur"] or ncost > LIMITS["max_cost"]
                or nbel < LIMITS["min_bel"] or ncomf < LIMITS["min_comf"]
                or stagecount(nmaps) > LIMITS["max_stage"]):
                print(f"{indent}→ {e.action} (pruned: constraint fail)")
                continue

            print(f"{indent}→ {e.action}")
            bc(e.dst, ndur, ncost, nbel, ncomf, nmaps, nacts, depth + 1, visited | {state})

    # Start proof search from the start state, with empty path and full belief/comfort
    bc(start, 0, 0, 1.0, 1.0, [], [], 0, set())
    return solutions

# ───────────────────────────────────────────────────────────────
# 4. Run proof search and display all solutions from Gent to Oostende
# ───────────────────────────────────────────────────────────────

if __name__ == "__main__":
    print("Proof search for ?- path(Gent, Oostende) under all constraints\n")
    all_proofs = prove("Gent", "Oostende")

    if not all_proofs:
        print("\nNo admissible paths.")
    else:
        print("\nSolutions (Gent → Oostende):\n")
        for idx, (acts, dur, cost, bel, comf) in enumerate(all_proofs, 1):
            chain = " → ".join(acts)
            print(f"{idx}. {chain}")
            print(f"   dur={dur:.0f}, cost={cost:.3f}, bel={bel:.3f}, comf={comf:.3f}\n")

