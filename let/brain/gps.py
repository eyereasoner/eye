#!/usr/bin/env python3
# ──────────────────────────────────────────────────────────────
# GPS (roads) — N3 rules + tolerant parser → loop-free BFS
# -------------------------------------------------------------
# This file is for a simple road-map domain. It:
#   1) embeds an N3 RULE BLOCK describing transitions on a map,
#   2) parses those rules into a lightweight Edge graph,
#   3) runs a loop-free BFS that respects global constraints,
#   4) prints all admissible solutions and highlights the fastest one.
#
# Notes:
# • We assume each rule encodes one directed edge with attributes:
#     (map_id, src_city, dst_city, action_name, duration, cost, belief, comfort)
# • Belief/comfort are multiplicative over a path (as in the source files).
# • Stage counting: how many times the map_id changes along the path; we cap it.
# • BFS is over action sequences (ordered plans), not just states, so we can
#   return multiple distinct plans that reach the same destination.
# ──────────────────────────────────────────────────────────────

from __future__ import annotations

import re
import textwrap
from dataclasses import dataclass
from typing import List, Tuple, Dict, Deque, Set
from collections import deque

# ╔═══════════════════════════════════════╗
# ║ 1. DATA MODEL (edges & nodes)         ║
# ╚═══════════════════════════════════════╝

@dataclass(frozen=True)
class Edge:
    """
    One directed transition extracted from a single N3 gps:description rule.

    Attributes
    ----------
    map_id  : str    Symbolic map identifier (e.g., "map-BE").
    src     : str    Source city/location label (e.g., "Gent").
    dst     : str    Destination city/location label (e.g., "Brugge").
    action  : str    Action/step name (e.g., "drive_gent_brugge").
    dur_s   : float  Duration in seconds for this leg.
    cost    : float  Monetary (or abstract) cost for this leg.
    belief  : float  Multiplicative reliability ∈ (0, 1]; path belief is product.
    comfort : float  Multiplicative comfort    ∈ (0, 1]; path comfort is product.
    """
    map_id : str
    src    : str
    dst    : str
    action : str
    dur_s  : float
    cost   : float
    belief : float
    comfort: float

@dataclass
class Node:
    """
    A search frontier node representing a *partial plan* ending in `city`.

    We accumulate totals (dur, cost, bel, comf), track the map_id sequence in
    `maps` (for stage counting), and maintain the ordered `path` of actions.
    """
    city : str
    dur  : float
    cost : float
    bel  : float
    comf : float
    maps : List[str]
    path : List[str]

# ╔═══════════════════════════════════════╗
# ║ 2. N3 RULE BLOCK                      ║
# ╚═══════════════════════════════════════╝
# Paste rules as-is; the parser below reads them verbatim.
# The rule *shape* we expect is (whitespace flexible):
#
#   { :<MAP> gps:description ( { ?S :location :<SRC> } true { ?S :location :<DST> }
#                              :<ACTION> <DUR> <COST> <BELIEF> <COMFORT> ) } <= true .
#
# The content inside (...) is a tuple:
#   ( precondition1  true  precondition2  action  dur  cost  belief  comfort )
#
# We only read the locations (SRC,DST), the action, and the 4 scalars.

N3_RULES = textwrap.dedent("""
PREFIX math: <http://www.w3.org/2000/10/swap/math#>
PREFIX gps: <https://eyereasoner.github.io/eye/reasoning/gps/gps-schema#> 
PREFIX : <https://eyereasoner.github.io/eye/reasoning#> 

# map of Belgium
{:map-BE gps:description ({?S :location :Gent} true {?S :location :Brugge} :drive_gent_brugge 1500.0 0.006 0.96 0.99)} <= true.
{:map-BE gps:description ({?S :location :Gent} true {?S :location :Kortrijk} :drive_gent_kortrijk 1600.0 0.007 0.96 0.99)} <= true.
{:map-BE gps:description ({?S :location :Kortrijk} true {?S :location :Brugge} :drive_kortrijk_brugge 1600.0 0.007 0.96 0.99)} <= true.
{:map-BE gps:description ({?S :location :Brugge} true {?S :location :Oostende} :drive_brugge_oostende 900.0 0.004 0.98 1.0)} <= true.
""").strip()

if not N3_RULES:
    raise RuntimeError("⚠️ You forgot to paste the N3 rules into N3_RULES!")

# ╔═══════════════════════════════════════╗
# ║ 3. RULE STRUCTURE                     ║
# ╚═══════════════════════════════════════╝
# We keep parsing concerns separate from search by converting each
# rule into an Edge. The search layer stays agnostic of N3 syntax.

# ╔═══════════════════════════════════════╗
# ║ 4. RULE PARSER (very tolerant)        ║
# ╚═══════════════════════════════════════╝
# This regex is intentionally whitespace-tolerant and only matches
# the specific "gps:description (...) <= true." shape we need.
#
# Capture groups:
#   map     : "map-BE"
#   src     : "Gent"
#   dst     : "Brugge"
#   action  : "drive_gent_brugge"
#   dur     : "1500.0"   (string -> float)
#   cost    : "0.006"    (string -> float)
#   belief  : "0.96"     (string -> float)
#   comfort : "0.99"     (string -> float)
#
# If your rules evolve (e.g., new terms between these fields), adjust the
# regex accordingly or switch to a proper N3 parser.

RULE_RE = re.compile(
    r"""
    \{\s*:(?P<map>[A-Za-z0-9_-]+)\s+gps:description\s+\(\s*
        \{\?S\s+:location\s+:(?P<src>[A-Za-z0-9_-]+)\}\s*
        true\s*
        \{\?S\s+:location\s+:(?P<dst>[A-Za-z0-9_-]+)\}\s*
        :(?P<action>[A-Za-z0-9_]+)\s*
        (?P<dur>[0-9]+(?:\.[0-9]+)?)\s*
        (?P<cost>[0-9]+(?:\.[0-9]+)?)\s*
        (?P<belief>[0-9]+(?:\.[0-9]+)?)\s*
        (?P<comfort>[0-9]+(?:\.[0-9]+)?)\s*
    \)\s*\}\s*<=\s*true\s*\.
    """,
    re.S | re.X,
)

def _parse_rules(n3: str) -> List[Edge]:
    """
    Convert an N3 rule block into a list of Edge objects.

    Parameters
    ----------
    n3 : str
        The entire N3 text block.

    Returns
    -------
    List[Edge]
        One Edge per matching gps:description rule.
    """
    edges: List[Edge] = []
    for m in RULE_RE.finditer(n3):
        map_id  = m.group("map")
        src     = m.group("src")
        dst     = m.group("dst")
        action  = m.group("action")
        dur_s   = float(m.group("dur"))
        cost    = float(m.group("cost"))
        belief  = float(m.group("belief"))
        comfort = float(m.group("comfort"))
        edges.append(Edge(map_id, src, dst, action, dur_s, cost, belief, comfort))
    return edges

EDGES: List[Edge] = _parse_rules(N3_RULES)
print(f"→ Parsed {len(EDGES)} transition edges from the N3 block.\n")

# Build adjacency list: OUT[src] -> [edges starting at src].
OUT: Dict[str, List[Edge]] = {}
for e in EDGES:
    OUT.setdefault(e.src, []).append(e)

# ╔════════════════════════════════════════════╗
# ║ 5. SEARCH PARAMETERS (limits & utilities)  ║
# ╚════════════════════════════════════════════╝

# Start/goal are kept inline for clarity; trivially configurable.
START_CITY = "Gent"
GOAL_CITY  = "Oostende"

# Global constraints (identical to gps.py):
MAX_DUR_S   = 5000.0   # seconds
MAX_COST    = 5.0      # euros
MIN_BELIEF  = 0.2
MIN_COMFORT = 0.4
MAX_STAGECT = 1        # max number of consecutive unique map segments

def stagecount(maps: List[str]) -> int:
    """
    Count how many *segments* of identical map_id appear when walking `maps`.
    Example: ["map-BE","map-BE","map-FR","map-FR","map-BE"] → 3.
    This is effectively "number of times the active map changes + 1", unless empty.
    """
    if not maps:
        return 0
    cnt = 1
    for prev, curr in zip(maps, maps[1:]):
        if curr != prev:
            cnt += 1
    return cnt

def within_limits(dur: float, cost: float, bel: float, comf: float, maps: List[str]) -> bool:
    """
    Check all global limits in one place so the search loop stays clean.
    """
    return (
        dur   <= MAX_DUR_S and
        cost  <= MAX_COST  and
        bel   >= MIN_BELIEF and
        comf  >= MIN_COMFORT and
        stagecount(maps) <= MAX_STAGECT
    )

# ╔═══════════════════════════════════════╗
# ║ 6. LOOP-FREE BFS OVER ACTION SEQS     ║
# ╚═══════════════════════════════════════╝
# BFS explores partial plans in increasing number of steps.
# We *prevent trivial cycles* by remembering (city, path-as-tuple).
# This forbids exact repetition of the same action list leading to same city.
# (That’s sufficient here; for larger graphs, you might add dominance checks.)

def search_all(start_city: str, goal_city: str) -> List[Node]:
    """
    Enumerate every distinct action sequence that satisfies the limits
    and ends in `goal_city`.

    Returns
    -------
    List[Node]
        Each Node encodes a complete plan with accumulated totals.
    """
    start = Node(start_city, dur=0.0, cost=0.0, bel=1.0, comf=1.0, maps=[], path=[])
    queue: Deque[Node] = deque([start])
    sols: List[Node] = []

    # Visited signatures: prevent infinite loops of the *same* action sequence.
    visited: Set[Tuple[str, Tuple[str, ...]]] = {(start.city, tuple())}

    while queue:
        cur = queue.popleft()

        # Record solutions as we see them (don’t early-exit; other plans may exist).
        if cur.city == goal_city and within_limits(cur.dur, cur.cost, cur.bel, cur.comf, cur.maps):
            sols.append(cur)

        # Expand admissible outgoing edges.
        for e in sorted(OUT.get(cur.city, []), key=lambda x: x.action):
            nxt_dur = cur.dur  + e.dur_s         # additive
            nxt_cos = cur.cost + e.cost          # additive
            nxt_bel = cur.bel  * e.belief        # multiplicative
            nxt_com = cur.comf * e.comfort       # multiplicative
            nxt_maps = cur.maps + [e.map_id]     # append map segment

            # Prune early if any limit is violated; keeps the frontier small.
            if not within_limits(nxt_dur, nxt_cos, nxt_bel, nxt_com, nxt_maps):
                continue

            nxt_path = tuple(cur.path + [e.action])
            signature = (e.dst, nxt_path)
            if signature in visited:
                # Same destination by the exact same action sequence — skip.
                continue
            visited.add(signature)

            queue.append(Node(
                e.dst, nxt_dur, nxt_cos, nxt_bel, nxt_com, nxt_maps, list(nxt_path)
            ))

    return sols

# ╔═══════════════════════════════════════╗
# ║ 7. PRETTY PRINTING OF RESULTS         ║
# ╚═══════════════════════════════════════╝

def print_solutions(sols: List[Node]) -> None:
    """
    Print all admissible plans in a human-friendly way, then show the fastest.

    Ordering
    --------
    We sort by (duration, steps, lexicographic action list) to reproduce the
    "optimal by minimal duration" behavior from the original gps.py.
    (If you prefer the clinical script’s triage, adjust the key accordingly.)
    """
    if not sols:
        print("⛔ No admissible paths under the limits.")
        return

    sols.sort(key=lambda n: (n.dur, len(n.path), n.path))

    print(f"✅ Found {len(sols)} admissible route(s) for path({START_CITY}, {GOAL_CITY}):\n")
    for idx, n in enumerate(sols, 1):
        print(f"Route #{idx}")
        print(f" Steps    : {len(n.path)}")
        print(f" Duration : {n.dur:.0f} s (≤ {MAX_DUR_S:.0f})")
        print(f" Cost     : {n.cost:.3f} (≤ {MAX_COST:.1f})")
        print(f" Belief   : {n.bel:.3f} (≥ {MIN_BELIEF})")
        print(f" Comfort  : {n.comf:.3f} (≥ {MIN_COMFORT})")
        print(f" Stages   : {stagecount(n.maps)} (≤ {MAX_STAGECT})")
        for i, act in enumerate(n.path, 1):
            print(f"  {i:2d}. {act}")
        print()

    best = sols[0]
    print(f"Optimal (minimal duration): {' → '.join(best.path)}  "
          f"[dur={best.dur:.0f} s]\n")

# ╔═══════════════════════════════════════╗
# ║ 8. ARC HELPERS (apps & replay)        ║
# ╚═══════════════════════════════════════╝

def applicable_from_city(city: str) -> List[Edge]:
    """
    Return all edges that could fire from `city` (parser-derived graph view).
    """
    return sorted(OUT.get(city, []), key=lambda e: e.action)

def replay_totals(start_city: str, actions: List[str]) -> Tuple[float,float,float,float,List[str],str]:
    """
    Recompute totals by replaying a given action list from `start_city`.
    This is a useful cross-check to ensure the accumulated metrics match.

    Returns
    -------
    (dur, cost, belief, comfort, maps, end_city)
    """
    # Index for quick lookups: in this dataset, (src, action) pairs are unique.
    idx: Dict[Tuple[str, str], List[Edge]] = {}
    for e in EDGES:
        idx.setdefault((e.src, e.action), []).append(e)

    cur = start_city
    dur=0.0; cost=0.0; bel=1.0; comf=1.0
    maps: List[str] = []

    for a in actions:
        cand = idx.get((cur, a), [])
        if not cand:
            raise RuntimeError(f"Action {a} not applicable from {cur}")
        e = cand[0]
        cur = e.dst
        dur += e.dur_s; cost += e.cost; bel *= e.belief; comf *= e.comfort
        maps.append(e.map_id)

    return dur, cost, bel, comf, maps, cur

# ╔═══════════════════════════════════════╗
# ║ 9. MAIN — Answer / Reason / Check     ║
# ╚═══════════════════════════════════════╝

def approx_eq(a: float, b: float, tol: float = 1e-12) -> bool:
    """
    Tiny helper for float comparisons in the harness.
    """
    d = a - b
    if d < 0:
        d = -d
    return d <= tol

if __name__ == "__main__":
    # ---------------- Preamble ----------------
    print("============================================")
    print("GPS (roads, N3 rules) — Answer / Reason why / Check (harness)")
    print("============================================\n")

    print(f"Start city : {START_CITY}")
    print(f"Goal city  : {GOAL_CITY}\n")

    # ---------------- Answer ----------------
    solutions = search_all(START_CITY, GOAL_CITY)

    print("Answer")
    print("======")
    if not solutions:
        print("No admissible path exists under the limits.\n")
    else:
        print_solutions(solutions)

    # ---------------- Reason why ----------------
    # This section explains *why* the engine produced the above answer by
    # showing limits and applicable actions from the start city.
    print("Reason why")
    print("==========")
    print("Global limits (as in original gps.py):")
    print(f" duration ≤ {MAX_DUR_S:.0f} s, cost ≤ €{MAX_COST:.1f}, "
          f"belief ≥ {MIN_BELIEF}, comfort ≥ {MIN_COMFORT}, stagecount ≤ {MAX_STAGECT}\n")

    apps = applicable_from_city(START_CITY)
    print(f"From the start city {START_CITY}, applicable actions: {len(apps)}")
    for e in apps:
        print(f" • {e.action}: {e.src} → {e.dst} "
              f"(+{e.dur_s:.0f}s, +€{e.cost:.3f}, ×bel {e.belief:.3f}, ×comf {e.comfort:.3f})")
    print()

    # ---------------- Check (harness) ----------------
    # A lightweight test suite that verifies:
    #   • the exact solution set (by action names),
    #   • that all limits (including stagecount) are satisfied,
    #   • that re-playing the plan reproduces the same totals,
    #   • that the “best” plan is the two-step route via Brugge.
    print("Check (harness)")
    print("===============")
    expected = {
        ("drive_gent_brugge", "drive_brugge_oostende"),
        ("drive_gent_kortrijk", "drive_kortrijk_brugge", "drive_brugge_oostende"),
    }
    found = set(tuple(n.path) for n in solutions)
    assert found == expected, f"Unexpected solution set: {found}"
    print("• Solution set equality ✓")

    for n in solutions:
        assert within_limits(n.dur, n.cost, n.bel, n.comf, n.maps)
        assert stagecount(n.maps) == 1
    print("• Limits & stagecount checks ✓")

    for n in solutions:
        D, C, Bf, Cf, Ms, end = replay_totals(START_CITY, n.path)
        assert end == GOAL_CITY
        assert approx_eq(D, n.dur) and approx_eq(C, n.cost) and \
               approx_eq(Bf, n.bel) and approx_eq(Cf, n.comf)
        assert stagecount(Ms) == 1
    print("• Totals recomputation ✓")

    sols_sorted = sorted(solutions, key=lambda s: (s.dur, len(s.path), s.path))
    assert sols_sorted[0].path == ["drive_gent_brugge", "drive_brugge_oostende"], \
        "Shortest route not first as expected."
    print("• Optimal-by-duration ordering ✓")

    print("\nHarness complete: parsing, limits, search, and optimality verified. ✓")

# ──────────────────────────────────────────────────────────────
# HOW TO EXTEND
#  • Add more N3 rules to N3_RULES with the same pattern; the parser will pick
#    them up automatically.
#  • If you introduce a second map (e.g., :map-FR), remember MAX_STAGECT controls
#    how many consecutive map segments you can use in one plan.
#  • If the N3 structure changes, update RULE_RE to match the new shape or replace
#    the regex with an actual N3 parser.
# ──────────────────────────────────────────────────────────────

