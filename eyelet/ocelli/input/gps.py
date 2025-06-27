"""
Python translation of EYE/N3 GPS - Goal driven Parallel Sequences -- Jos De Roo

This script replicates the logic of the original N3 rules:
 - "Maps" are edges that describe state transitions.
 - A *state* is simply a location string (e.g. "Gent").
 - An *action* is the descriptive name of the transition (e.g. "drive_gent_brugge").
 - The search procedure (`find_paths`) performs a depth‑first search,
   maintaining cumulative values for duration, cost, belief and comfort.
 - Constraints (max‑duration, max‑cost, min‑belief, min‑comfort, max‑stagecount)
   are enforced exactly as in the original rules.
 - The helper `stagecount` matches the N3 rule set: it counts how many
   contiguous runs of identical `map_id`s occur in a path.

Running the file as a script prints every admissible path from Gent to
Oostende under the same query limits used in the N3 example.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, List, Sequence, Tuple


@dataclass(frozen=True)
class Edge:
    """A description triple extracted from a <map> gps:description … rule."""

    map_id: str
    from_loc: str
    to_loc: str
    action: str
    duration: float
    cost: float
    belief: float
    comfort: float


# ----- Belgian road map ----------------------------------------------------
# Matches the four description rules in the N3 source one‑for‑one

edges: List[Edge] = [
    Edge("map-BE", "Gent", "Brugge", "drive_gent_brugge", 1500.0, 0.006, 0.96, 0.99),
    Edge("map-BE", "Gent", "Kortrijk", "drive_gent_kortrijk", 1600.0, 0.007, 0.96, 0.99),
    Edge("map-BE", "Kortrijk", "Brugge", "drive_kortrijk_brugge", 1600.0, 0.007, 0.96, 0.99),
    Edge("map-BE", "Brugge", "Oostende", "drive_brugge_oostende", 900.0, 0.004, 0.98, 1.00),
]


# ----- Helper matching the list‑based stagecount rules ---------------------

def stagecount(maps: Sequence[str]) -> int:
    """Return the number of *stages* in a path, where a stage is a maximal
    subsequence of identical map identifiers (exactly the behaviour of the
    three gps:stagecount rules).
    """

    if not maps:
        return 0

    count = 1
    for prev, nxt in zip(maps, maps[1:]):
        if prev != nxt:
            count += 1
    return count


# ----- Core search procedure (findpaths) -----------------------------------

def find_paths(
    start_loc: str,
    goal: Callable[[str], bool],
    *,
    max_duration: float,
    max_cost: float,
    min_belief: float,
    min_comfort: float,
    max_stagecount: int,
    graph: Sequence[Edge],
) -> List[Tuple[List[str], float, float, float, float]]:
    """Generate all admissible paths in *depth‑first* order (mirrors EYE).

    Returns a list of tuples: (action_list, duration, cost, belief, comfort).
    """

    # Each stack frame: (current_location, path_actions, path_maps,
    #                    duration, cost, belief, comfort)
    stack: List[
        Tuple[str, List[str], List[str], float, float, float, float]
    ] = [
        (start_loc, [], [], 0.0, 0.0, 1.0, 1.0)
    ]

    solutions: List[
        Tuple[List[str], float, float, float, float]
    ] = []

    while stack:
        loc, actions, maps, dur, cst, bel, com = stack.pop()

        # Goal reached ⇒ record solution
        if goal(loc):
            solutions.append((actions, dur, cst, bel, com))
            # *No cut* – continue searching for alternative proofs
            continue

        # Try every edge that originates from the current location
        for edge in graph:
            if edge.from_loc != loc:
                continue

            ndur = dur + edge.duration
            if ndur > max_duration:
                continue

            ncst = cst + edge.cost
            if ncst > max_cost:
                continue

            nbel = bel * edge.belief
            if nbel < min_belief:
                continue

            ncom = com * edge.comfort
            if ncom < min_comfort:
                continue

            nmaps = maps + [edge.map_id]
            if stagecount(nmaps) > max_stagecount:
                continue

            nactions = actions + [edge.action]
            stack.append(
                (
                    edge.to_loc,
                    nactions,
                    nmaps,
                    ndur,
                    ncst,
                    nbel,
                    ncom,
                )
            )

    return solutions


# ----- Demo reproducing the N3 log:query -----------------------------------

if __name__ == "__main__":
    # Current state of :i1 in the original data
    initial_location = "Gent"

    # Goal pattern {?SUBJECT :location :Oostende}
    is_goal = lambda loc: loc == "Oostende"

    # Limits taken from the query conjunct (5000.0 5.0 0.2 0.4 1)
    constraints = dict(
        max_duration=5000.0,
        max_cost=5.0,
        min_belief=0.2,
        min_comfort=0.4,
        max_stagecount=1,
    )

    paths = find_paths(
        initial_location,
        is_goal,
        graph=edges,
        **constraints,
    )

    if not paths:
        print("No path satisfies the constraints.")
    else:
        for i, (acts, dur, cst, bel, com) in enumerate(paths, 1):
            print(f"Solution {i}:\n  path     = {acts}\n  duration = {dur}\n  cost     = {cst}\n  belief   = {bel}\n  comfort  = {com}")

