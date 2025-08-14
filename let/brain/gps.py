#!/usr/bin/env python3
# ──────────────────────────────────────────────────────────────
# GPS-style reasoner for weighted state transitions (road map)
# -------------------------------------------------------------
# • Uses a small inline road graph (edges with duration, cost,
#   belief, comfort, and a map_id for stage counting).
# • Runs a loop-free BFS that:
#   – accumulates the 4 metrics,
#   – enforces the original limits (≤ 5000s, ≤ €5.0, ≥ 0.2, ≥ 0.4),
#   – returns every distinct ordered action list that reaches GOAL
#     while respecting stagecount ≤ 1 (consecutive map changes).
# • Prints the solutions and highlights the optimal one by
#   minimal duration (as in the original gps.py).
# • Includes “Answer / Reason why / Check (harness)” sections.
# ──────────────────────────────────────────────────────────────

from __future__ import annotations

import re
import textwrap
from dataclasses import dataclass
from typing import List, Tuple, Dict, Deque
from collections import deque

# ╔═══════════════════════════════════════╗
# ║ 1. DATA MODEL (edges & nodes)         ║
# ╚═══════════════════════════════════════╝

@dataclass(frozen=True)
class Edge:
    map_id : str
    src    : str
    dst    : str
    action : str
    dur_s  : float      # seconds
    cost   : float      # euros
    belief : float      # ∈ (0,1]
    comfort: float      # ∈ (0,1]

@dataclass
class Node:
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

# We keep the search layer decoupled from parsing by converting each
# gps:description(...) rule into a plain Edge object.

# ╔═══════════════════════════════════════╗
# ║ 4. RULE PARSER (very tolerant)        ║
# ╚═══════════════════════════════════════╝
# Extracts: map_id, src, dst, action, duration, cost, belief, comfort
# Pattern mirrors the style used in gps_clinical_bench_10.py’s parser.

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

# Build adjacency
OUT: Dict[str, List[Edge]] = {}
for e in EDGES:
    OUT.setdefault(e.src, []).append(e)

# ╔════════════════════════════════════════════╗
# ║ 5. SEARCH PARAMETERS (limits & utilities)  ║
# ╚════════════════════════════════════════════╝

START_CITY = "Gent"
GOAL_CITY  = "Oostende"

MAX_DUR_S   = 5000.0   # seconds
MAX_COST    = 5.0      # euros
MIN_BELIEF  = 0.2
MIN_COMFORT = 0.4
MAX_STAGECT = 1        # consecutive unique map_ids along the path

def stagecount(maps: List[str]) -> int:
    if not maps:
        return 0
    cnt = 1
    for prev, curr in zip(maps, maps[1:]):
        if curr != prev:
            cnt += 1
    return cnt

def within_limits(dur: float, cost: float, bel: float, comf: float, maps: List[str]) -> bool:
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

def search_all(start_city: str, goal_city: str) -> List[Node]:
    start = Node(start_city, dur=0.0, cost=0.0, bel=1.0, comf=1.0, maps=[], path=[])
    queue: Deque[Node] = deque([start])
    sols: List[Node] = []

    visited: set[Tuple[str, Tuple[str, ...]]] = set([(start.city, tuple())])

    while queue:
        cur = queue.popleft()

        if cur.city == goal_city and within_limits(cur.dur, cur.cost, cur.bel, cur.comf, cur.maps):
            sols.append(cur)

        for e in sorted(OUT.get(cur.city, []), key=lambda x: x.action):
            nxt_dur = cur.dur  + e.dur_s
            nxt_cos = cur.cost + e.cost
            nxt_bel = cur.bel  * e.belief
            nxt_com = cur.comf * e.comfort
            nxt_maps = cur.maps + [e.map_id]
            if not within_limits(nxt_dur, nxt_cos, nxt_bel, nxt_com, nxt_maps):
                continue

            nxt_path = tuple(cur.path + [e.action])
            signature = (e.dst, nxt_path)
            if signature in visited:
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
    if not sols:
        print("⛔ No admissible paths under the limits.")
        return

    # Keep gps.py’s “optimal by minimal duration” spirit:
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
    return sorted(OUT.get(city, []), key=lambda e: e.action)

def replay_totals(start_city: str, actions: List[str]) -> Tuple[float,float,float,float,List[str],str]:
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
    d = a - b
    if d < 0:
        d = -d
    return d <= tol

if __name__ == "__main__":
    print("============================================")
    print("GPS (roads, N3 rules) — Answer / Reason why / Check (harness)")
    print("============================================\n")

    print(f"Start city : {START_CITY}")
    print(f"Goal city  : {GOAL_CITY}\n")

    solutions = search_all(START_CITY, GOAL_CITY)

    print("Answer")
    print("======")
    if not solutions:
        print("No admissible path exists under the limits.\n")
    else:
        print_solutions(solutions)

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

