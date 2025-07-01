#!/usr/bin/env python3
"""
Show independent proof traces for the query

    ?- path(Gent, Oostende).

Constraints are exactly the same as in Jos De Roo’s GPS demo:
    duration ≤ 5000
    cost     ≤ 5.0
    belief   ≥ 0.2
    comfort  ≥ 0.4
    stagecount ≤ 1   (all edges from same map)
"""

from dataclasses import dataclass
from itertools import count
from typing import List, Dict, Tuple

# ────────────────────────────────────────────────────────────
# 1. Edge data (same as forward GPS example)
# ────────────────────────────────────────────────────────────
@dataclass(frozen=True)
class Edge:
    map_id:  str
    src:     str
    dst:     str
    action:  str
    dur:     float
    cost:    float
    belief:  float
    comfort: float

edges: List[Edge] = [
    Edge("map-BE","Gent",     "Brugge",   "drive_gent_brugge",     1500, 0.006, 0.96, 0.99),
    Edge("map-BE","Gent",     "Kortrijk", "drive_gent_kortrijk",   1600, 0.007, 0.96, 0.99),
    Edge("map-BE","Kortrijk", "Brugge",   "drive_kortrijk_brugge", 1600, 0.007, 0.96, 0.99),
    Edge("map-BE","Brugge",   "Oostende", "drive_brugge_oostende",  900, 0.004, 0.98, 1.00),
]

OUT: Dict[str, List[Edge]] = {}
for e in edges:
    OUT.setdefault(e.src, []).append(e)

# ────────────────────────────────────────────────────────────
# 2. Constraint helpers
# ────────────────────────────────────────────────────────────
LIMITS = dict(max_dur=5000.0, max_cost=5.0,
              min_bel=0.2,   min_comf=0.4,
              max_stage=1)

def stagecount(maps: List[str]) -> int:
    if not maps: return 0
    cnt = 1
    for a,b in zip(maps,maps[1:]):
        if a!=b: cnt+=1
    return cnt

# ────────────────────────────────────────────────────────────
# 3. Backward-chaining engine
# ────────────────────────────────────────────────────────────
def prove(start: str, goal: str) -> List[Tuple[List[str],float,float,float,float]]:
    """Return *all* admissible proofs (action list + aggregates)."""
    step = count(1)                    # fresh counter for this top call
    solutions = []

    def bc(state, dur, cost, bel, comf, maps, acts, depth):
        ind="  "*depth
        print(f"{ind}Step {next(step):02}: at {state}"
              f" (dur={dur}, cost={cost:.3f}, bel={bel:.3f}, comf={comf:.3f})")

        # goal reached
        if state==goal:
            if (dur<=LIMITS["max_dur"] and cost<=LIMITS["max_cost"]
                and bel>=LIMITS["min_bel"] and comf>=LIMITS["min_comf"]
                and stagecount(maps)<=LIMITS["max_stage"]):
                print(f"{ind}✓ goal & constraints OK")
                solutions.append((acts,dur,cost,bel,comf))
            else:
                print(f"{ind}✗ constraints fail")
            return

        # explore outgoing edges (fixed order for deterministic proofs)
        for e in sorted(OUT.get(state, []), key=lambda e:e.action):
            ndur  = dur  + e.dur
            ncost = cost + e.cost
            nbel  = bel  * e.belief
            ncomf = comf * e.comfort
            nmaps = maps+[e.map_id]
            if (ndur>LIMITS["max_dur"] or ncost>LIMITS["max_cost"]
                or nbel<LIMITS["min_bel"] or ncomf<LIMITS["min_comf"]
                or stagecount(nmaps)>LIMITS["max_stage"]):
                continue
            print(f"{ind}→ {e.action}")
            bc(e.dst, ndur, ncost, nbel, ncomf,
               nmaps, acts+[e.action], depth+1)

    bc(start,0,0,1.0,1.0,[],[],0)
    return solutions

# ────────────────────────────────────────────────────────────
# 4. Run and show BOTH proofs from Gent to Oostende
# ────────────────────────────────────────────────────────────
if __name__=="__main__":
    all_proofs = prove("Gent","Oostende")

    if not all_proofs:
        print("\nNo admissible paths.")
    else:
        print("\nSolutions (Gent → Oostende):\n")
        for idx,(acts,dur,cost,bel,comf) in enumerate(all_proofs,1):
            chain=" → ".join(acts)
            print(f"{idx}. {chain}")
            print(f"   dur={dur}, cost={cost:.3f}, bel={bel:.3f}, comf={comf:.3f}\n")

