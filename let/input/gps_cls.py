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

from dataclasses import dataclass, field, asdict
from typing import List, Dict, Optional, Any
from datetime import datetime

# ───────────────────────────────────────────────────────────────
# 1) Domain model: directed edges with properties
# ───────────────────────────────────────────────────────────────

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
    Edge("map-BE", "Gent",     "Brugge",   "drive_gent_brugge",     1500, 0.006, 0.96, 0.99),
    Edge("map-BE", "Gent",     "Kortrijk", "drive_gent_kortrijk",   1600, 0.007, 0.96, 0.99),
    Edge("map-BE", "Kortrijk", "Brugge",   "drive_kortrijk_brugge", 1600, 0.007, 0.96, 0.99),
    Edge("map-BE", "Brugge",   "Oostende", "drive_brugge_oostende",  900, 0.004, 0.98, 1.00),
]

OUT: Dict[str, List[Edge]] = {}
for e in edges:
    OUT.setdefault(e.src, []).append(e)

# ───────────────────────────────────────────────────────────────
# 2) Constraints and helpers
# ───────────────────────────────────────────────────────────────

LIMITS = dict(
    max_dur=5000.0,
    max_cost=5.0,
    min_bel=0.2,
    min_comf=0.4,
    max_stage=1  # consecutive unique map_ids
)

def stagecount(maps: List[str]) -> int:
    if not maps:
        return 0
    cnt = 1
    for p, c in zip(maps, maps[1:]):
        if c != p:
            cnt += 1
    return cnt

# ───────────────────────────────────────────────────────────────
# 3) Structured proof traces
# ───────────────────────────────────────────────────────────────

@dataclass(frozen=True)
class TraceEvent:
    step: int
    kind: str                 # "enter" | "edge" | "goal_ok" | "goal_fail"
    state: str
    action: Optional[str] = None
    dur: float = 0.0
    cost: float = 0.0
    belief: float = 1.0
    comfort: float = 1.0
    depth: int = 0

@dataclass(frozen=True)
class Proof:
    actions: List[str]
    maps: List[str]
    duration: float
    cost: float
    belief: float
    comfort: float
    trace: List[TraceEvent] = field(default_factory=list)

# ───────────────────────────────────────────────────────────────
# 4) Backward-chaining search that returns structured proofs
# ───────────────────────────────────────────────────────────────

def prove(start: str, goal: str) -> List[Proof]:
    proofs: List[Proof] = []

    def bc(state: str, dur: float, cost: float, bel: float, comf: float,
           maps: List[str], acts: List[str], depth: int,
           visited: set, trace: List[TraceEvent]) -> None:

        # Enter event (per-path step numbering)
        step_no = len(trace) + 1
        trace = trace + [TraceEvent(step=step_no, kind="enter", state=state,
                                    dur=dur, cost=cost, belief=bel, comfort=comf, depth=depth)]

        # Goal check
        if state == goal:
            ok = (dur <= LIMITS["max_dur"] and cost <= LIMITS["max_cost"]
                  and bel >= LIMITS["min_bel"] and comf >= LIMITS["min_comf"]
                  and stagecount(maps) <= LIMITS["max_stage"])
            step_no = len(trace) + 1
            trace_goal = trace + [TraceEvent(step=step_no, kind="goal_ok" if ok else "goal_fail",
                                             state=state, dur=dur, cost=cost, belief=bel, comfort=comf, depth=depth)]
            if ok:
                proofs.append(Proof(actions=acts, maps=maps, duration=dur, cost=cost,
                                    belief=bel, comfort=comf, trace=trace_goal))
            return

        # Cycle prevention
        if state in visited:
            return

        # Explore deterministically
        for e in sorted(OUT.get(state, []), key=lambda x: x.action):
            ndur, ncost = dur + e.dur, cost + e.cost
            nbel, ncomf = bel * e.belief, comf * e.comfort
            nmaps, nacts = maps + [e.map_id], acts + [e.action]

            # Early pruning
            if (ndur > LIMITS["max_dur"] or ncost > LIMITS["max_cost"]
                or nbel < LIMITS["min_bel"] or ncomf < LIMITS["min_comf"]
                or stagecount(nmaps) > LIMITS["max_stage"]):
                continue

            # Edge taken (record then go deeper)
            step_no = len(trace) + 1
            trace_next = trace + [TraceEvent(step=step_no, kind="edge", state=e.dst, action=e.action,
                                             dur=ndur, cost=ncost, belief=nbel, comfort=ncomf, depth=depth+1)]
            bc(e.dst, ndur, ncost, nbel, ncomf, nmaps, nacts, depth + 1, visited | {state}, trace_next)

    bc(start, 0.0, 0.0, 1.0, 1.0, [], [], 0, set(), [])
    return proofs

# ───────────────────────────────────────────────────────────────
# 5) Pretty-print emitter that outputs **valid Python code**
# ───────────────────────────────────────────────────────────────

def to_python(obj: Any, indent: int = 0) -> str:
    IND  = " " * indent
    IND2 = " " * (indent + 4)

    if isinstance(obj, Proof):
        lines = [
            f"{IND}Proof(",
            f"{IND2}actions={to_python(obj.actions, indent+4)},",
            f"{IND2}maps={to_python(obj.maps, indent+4)},",
            f"{IND2}duration={to_python(obj.duration, indent+4)},",
            f"{IND2}cost={to_python(obj.cost, indent+4)},",
            f"{IND2}belief={to_python(obj.belief, indent+4)},",
            f"{IND2}comfort={to_python(obj.comfort, indent+4)},",
            f"{IND2}trace={to_python(obj.trace, indent+4)}",
            f"{IND})"
        ]
        return "\n".join(lines)

    if isinstance(obj, TraceEvent):
        lines = [
            f"{IND}TraceEvent(",
            f"{IND2}step={obj.step},",
            f"{IND2}kind={to_python(obj.kind, indent+4)},",
            f"{IND2}state={to_python(obj.state, indent+4)},",
            f"{IND2}action={to_python(obj.action, indent+4)},",
            f"{IND2}dur={to_python(obj.dur, indent+4)},",
            f"{IND2}cost={to_python(obj.cost, indent+4)},",
            f"{IND2}belief={to_python(obj.belief, indent+4)},",
            f"{IND2}comfort={to_python(obj.comfort, indent+4)},",
            f"{IND2}depth={obj.depth}",
            f"{IND})"
        ]
        return "\n".join(lines)

    if isinstance(obj, dict):
        if not obj: return "{}"
        lines = [f"{IND}{{"]
        for k, v in obj.items():
            lines.append(f"{IND2}{to_python(k)}: {to_python(v, indent+4)},")
        lines.append(f"{IND}}}")
        return "\n".join(lines)

    if isinstance(obj, list):
        if not obj: return "[]"
        lines = [f"{IND}["]
        for v in obj:
            lines.append(f"{IND2}{to_python(v, indent+4)},")
        lines.append(f"{IND}]")
        return "\n".join(lines)

    if isinstance(obj, tuple):
        if not obj: return "()"
        lines = [f"{IND}("]
        for v in obj:
            lines.append(f"{IND2}{to_python(v, indent+4)},")
        lines.append(f"{IND})")
        return "\n".join(lines)

    if isinstance(obj, float):
        # keep a compact but precise representation
        return repr(round(obj, 6))  # adjust precision if desired

    return repr(obj)

def emit_py(name: str, obj: Any) -> None:
    print(f"{name} = {to_python(obj)}\n")

# ───────────────────────────────────────────────────────────────
# 6) Run and emit the result as Python code
# ───────────────────────────────────────────────────────────────

if __name__ == "__main__":
    proofs = prove("Gent", "Oostende")
    emit_py("proofs", proofs)

