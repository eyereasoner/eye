#!/usr/bin/env python3
# ============================================
# Explain-and-Check (GPS): ?- path(Gent, Oostende)
# ============================================
# What this program prints:
#   • Answer – the admissible routes with totals (duration, cost, belief, comfort, stages)
#              and which one is optimal by minimal duration.
#   • Reason why – the constraints + how edge attributes aggregate + per-route feasibility.
#   • Check (harness) – independent proof traces per route, solution-set equality,
#                       totals recomputation, constraint rechecks, and optimality.
#
# Model notes
# -----------
# • Graph is directed; each edge carries (duration seconds, cost euros, belief, comfort).
# • Aggregation along a path:
#       duration, cost: add
#       belief, comfort: multiply
#       stagecount: count consecutive distinct map_ids
# • Constraints (must hold on the whole path):
#       duration ≤ 5000,  cost ≤ 5.0,  belief ≥ 0.2,  comfort ≥ 0.4,  stagecount ≤ 1
#
# No imports. No user input.

# -----------------------------
# 0) Data model (edges)
# -----------------------------
class Edge:
    def __init__(self, map_id, src, dst, action, dur, cost, belief, comfort):
        self.map_id  = map_id
        self.src     = src
        self.dst     = dst
        self.action  = action
        self.dur     = float(dur)
        self.cost    = float(cost)
        self.belief  = float(belief)
        self.comfort = float(comfort)

# Edges (as in the demo)
EDGES = [
    Edge("map-BE", "Gent",     "Brugge",   "drive_gent_brugge",     1500, 0.006, 0.96, 0.99),
    Edge("map-BE", "Gent",     "Kortrijk", "drive_gent_kortrijk",   1600, 0.007, 0.96, 0.99),
    Edge("map-BE", "Kortrijk", "Brugge",   "drive_kortrijk_brugge", 1600, 0.007, 0.96, 0.99),
    Edge("map-BE", "Brugge",   "Oostende", "drive_brugge_oostende",  900, 0.004, 0.98, 1.00),
]

# Build adjacency
OUT = {}
for e in EDGES:
    OUT.setdefault(e.src, []).append(e)

# -----------------------------
# 1) Constraints & helpers
# -----------------------------
LIMITS = dict(
    max_dur = 5000.0,   # ≤ 5000
    max_cost = 5.0,     # ≤ 5.0
    min_bel = 0.2,      # ≥ 0.2
    min_comf = 0.4,     # ≥ 0.4
    max_stage = 1       # ≤ 1 stage (all edges from same map)
)

def stagecount(maps):
    """Count consecutive unique map_ids (stages)."""
    if not maps: return 0
    cnt = 1
    for prev, curr in zip(maps, maps[1:]):
        if curr != prev:
            cnt += 1
    return cnt

def within_limits(dur, cost, bel, comf, maps):
    return (dur  <= LIMITS["max_dur"] and
            cost <= LIMITS["max_cost"] and
            bel  >= LIMITS["min_bel"] and
            comf >= LIMITS["min_comf"] and
            stagecount(maps) <= LIMITS["max_stage"])

# -----------------------------
# 2) Silent search (collect all admissible paths)
# -----------------------------
def find_all_paths(start, goal):
    """
    Return a list of solutions:
      each = (actions, dur, cost, bel, comf, maps)
    """
    sols = []
    def dfs(city, dur, cost, bel, comf, maps, actions, visited):
        # Early prune on constraints
        if not within_limits(dur, cost, bel, comf, maps):
            return
        if city == goal:
            sols.append((actions[:], dur, cost, bel, comf, maps[:]))
            return
        if city in visited:
            return
        visited.add(city)
        for e in sorted(OUT.get(city, []), key=lambda x: x.action):
            dfs(
                e.dst,
                dur + e.dur,
                cost + e.cost,
                bel  * e.belief,
                comf * e.comfort,
                maps + [e.map_id],
                actions + [e.action],
                visited.copy()
            )
    dfs(start, 0.0, 0.0, 1.0, 1.0, [], [], set())
    return sols

# -----------------------------
# 3) Pretty traces (independent per route)
# -----------------------------
def print_trace_for_actions(start, goal, actions):
    """Replay a specific action sequence, printing state + running totals."""
    print(f"Trace for route: {' → '.join(actions)}")
    city = start
    dur, cost, bel, comf = 0.0, 0.0, 1.0, 1.0
    maps = []
    step = 1
    print(f"  Step {step:02}: at {city} (dur=0, cost=0.000, bel=1.000, comf=1.000)")
    for act in actions:
        step += 1
        candidates = [e for e in OUT.get(city, []) if e.action == act]
        if not candidates:
            print("  ✗ action not found from here")
            return
        e = candidates[0]
        dur  += e.dur; cost += e.cost; bel *= e.belief; comf *= e.comfort
        maps.append(e.map_id); city = e.dst
        print(f"  → {act}")
        print(f"  Step {step:02}: at {city} (dur={dur:.0f}, cost={cost:.3f}, bel={bel:.3f}, comf={comf:.3f})")
    if city == goal and within_limits(dur, cost, bel, comf, maps):
        print("  ✓ goal & constraints OK\n")
    else:
        print("  ✗ constraints failed or wrong goal\n")

# -----------------------------
# 4) Compute solutions
# -----------------------------
START, GOAL = "Gent", "Oostende"
solutions = find_all_paths(START, GOAL)

# sort by optimality: duration, then hop-count, then lexicographic on actions
solutions.sort(key=lambda s: (s[1], len(s[0]), s[0]))

# -----------------------------
# 5) ANSWER
# -----------------------------
print("============================================")
print("GPS case — Answer / Reason why / Check (harness)")
print("============================================\n")

if not solutions:
    print("Answer")
    print("======\nNo admissible paths.\n")
else:
    print("Answer")
    print("======")
    print(f"Admissible routes for path({START}, {GOAL}):\n")
    for idx, (acts, dur, cost, bel, comf, maps) in enumerate(solutions, 1):
        print(f"{idx}. {' → '.join(acts)}")
        print(f"   dur={dur:.0f}, cost={cost:.3f}, bel={bel:.3f}, comf={comf:.3f}, stages={stagecount(maps)}")
    # optimal by minimal duration is first due to sorting
    best = solutions[0]
    print(f"\nOptimal (minimal duration): {' → '.join(best[0])}  [dur={best[1]:.0f}]\n")

# -----------------------------
# 6) REASON WHY
# -----------------------------
print("Reason why")
print("==========")
print("Constraints (must all hold on the whole path):")
print("  duration ≤ 5000,  cost ≤ 5.0,  belief ≥ 0.2,  comfort ≥ 0.4,  stagecount ≤ 1\n")
print("Aggregation along a path:")
print("  • duration, cost: add each edge’s values")
print("  • belief, comfort: multiply each edge’s factors")
print("  • stagecount: count consecutive distinct map_ids (all edges here are ‘map-BE’, so it stays 1)\n")

print("Per-route feasibility:")
print("  Route A: Gent → Brugge → Oostende")
print("    dur = 1500 + 900  = 2400   (≤ 5000)")
print("    cost= 0.006+0.004 = 0.010  (≤ 5.0)")
print("    bel = 0.96×0.98   = 0.941  (≥ 0.2)")
print("    comf= 0.99×1.00   = 0.990  (≥ 0.4)")
print("    stages = 1\n")
print("  Route B: Gent → Kortrijk → Brugge → Oostende")
print("    dur = 1600 + 1600 + 900   = 4100   (≤ 5000)")
print("    cost= 0.007+ 0.007+ 0.004 = 0.018  (≤ 5.0)")
print("    bel = 0.96×0.96×0.98      ≈ 0.903  (≥ 0.2)")
print("    comf= 0.99×0.99×1.00      ≈ 0.980  (≥ 0.4)")
print("    stages = 1\n")

# -----------------------------
# 7) CHECK — harness (with independent proof traces)
# -----------------------------
def approx_eq(a, b, tol=1e-12):
    d = a - b
    if d < 0: d = -d
    return d <= tol

def harness():
    # Expected two routes (by action names)
    expected = {
        ("drive_gent_brugge", "drive_brugge_oostende"),
        ("drive_gent_kortrijk", "drive_kortrijk_brugge", "drive_brugge_oostende"),
    }
    found = set(tuple(acts) for (acts, *_rest) in solutions)
    assert found == expected, f"Unexpected solution set: {found}"

    # All solutions satisfy constraints & have stagecount=1
    for acts, dur, cost, bel, comf, maps in solutions:
        assert within_limits(dur, cost, bel, comf, maps), f"Constraints violated by {acts}"
        assert stagecount(maps) == 1, f"Stagecount not 1 for {acts}"

    # Recompute totals from edges & cross-check
    index = {(e.src, e.action): e for e in EDGES}
    for acts, dur, cost, bel, comf, _maps in solutions:
        city = START
        D=C=0.0; Bf=Cf=1.0
        maps2 = []
        for act in acts:
            e = index[(city, act)]
            city = e.dst
            D += e.dur; C += e.cost; Bf *= e.belief; Cf *= e.comfort
            maps2.append(e.map_id)
        assert city == GOAL
        assert approx_eq(D, dur) and approx_eq(C, cost) and approx_eq(Bf, bel) and approx_eq(Cf, comf)
        assert stagecount(maps2) == 1

    # Minimal duration route should be the 2-step via Brugge
    best = solutions[0]
    assert best[0] == ["drive_gent_brugge", "drive_brugge_oostende"], "Shortest route not first as expected."
    return True

print("Check (harness)")
print("===============")
if not solutions:
    print("No routes to check.\n")
else:
    # Independent proof traces
    for acts, *_rest in solutions:
        print_trace_for_actions(START, GOAL, acts)
    # Assertions
    harness()
    print("Harness: solutions set, constraints, totals, and optimality verified. ✓")

