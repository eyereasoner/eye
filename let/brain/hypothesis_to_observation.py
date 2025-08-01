#!/usr/bin/env python3
"""
Hypothesis → Observation (list membership + tensor unpacking), with full member-by-member proof
-----------------------------------------------------------------------------------------------

Data:
  t:* t:position-tensor (lat lon).

Hypothesis:
  h:traveler1 h:inOneOf (t:Antwerp-Belgium t:Boston-USA t:Cambridge-USA t:Ghent-Belgium t:Ostend-Belgium).

Rule:
  { ?T h:inOneOf ?LIST. ?C list:in ?LIST. ?C t:position-tensor (?LA ?LO). }
  =>
  { ?T h:location (?LA ?LO). }

Observation/Query:
  { h:traveler1 h:location (51.0535 3.7304). } => { h:traveler1 h:location (51.0535 3.7304). }.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, List, Optional, Set, Tuple, Dict

# -----------------------------------------------------------------------------
# Data
# -----------------------------------------------------------------------------

POS_TENSOR: Dict[str, Tuple[float, float]] = {
    "t:Antwerp-Belgium":  (51.2192,   4.4029),
    "t:Boston-USA":       (42.3584, -71.0598),
    "t:Bruges-Belgium":   (51.2108,   3.2249),
    "t:Cambridge-USA":    (42.3736, -71.1097),
    "t:Chicago-USA":      (41.8781, -87.6298),
    "t:Ghent-Belgium":    (51.0535,   3.7304),
    "t:Ostend-Belgium":   (51.2154,   2.9286),
}

H_IN_ONE_OF: List[str] = [
    "t:Antwerp-Belgium",
    "t:Boston-USA",
    "t:Cambridge-USA",
    "t:Ghent-Belgium",
    "t:Ostend-Belgium",
]

TRAVELER = "h:traveler1"
OBS_LOC = "(51.0535 3.7304)"  # from the query

def fmt_pair(lat: float, lon: float) -> str:
    return f"({lat:.4f} {lon:.4f})"

# -----------------------------------------------------------------------------
# Tiny triple store (only what we need)
# -----------------------------------------------------------------------------

Triple = Tuple[str, str, str]

class KB:
    def __init__(self):
        self.triples: Set[Triple] = set()
    def add(self, s: str, p: str, o: str):
        self.triples.add((s, p, o))
    def ask(self, p: Optional[str] = None) -> List[Triple]:
        return [t for t in self.triples if p is None or t[1] == p]

# -----------------------------------------------------------------------------
# Pretty-proof kernel
# -----------------------------------------------------------------------------

@dataclass
class Conclusion:
    kind: str
    payload: Any
    def pretty(self) -> str:
        return self.payload if isinstance(self.payload, str) else str(self.payload)

@dataclass
class Step:
    id: int
    rule: str
    premises: List[int]
    conclusion: Conclusion
    notes: Optional[str] = None

@dataclass
class Proof:
    steps: List[Step] = field(default_factory=list)
    def add(self, rule: str, premises: List[int], conclusion: Conclusion, notes: Optional[str] = None) -> int:
        sid = len(self.steps) + 1
        self.steps.append(Step(sid, rule, premises, conclusion, notes))
        return sid
    def pretty(self) -> str:
        lines = []
        for s in self.steps:
            prem = f" [{', '.join(map(str, s.premises))}]" if s.premises else ""
            note = f" // {s.notes}" if s.notes else ""
            lines.append(f"[{s.id}] {s.rule}{prem}: {s.conclusion.pretty()}{note}")
        return "\n".join(lines)

# -----------------------------------------------------------------------------
# Derivation: enumerate ALL member cases explicitly in the proof
# -----------------------------------------------------------------------------

def derive_locations_full(kb: KB, proof: Proof) -> None:
    # Facts/hypothesis
    proof.add("Facts", [], Conclusion("text",
        f"{TRAVELER} h:inOneOf ({' '.join(H_IN_ONE_OF)}) ;  t:* t:position-tensor (lat lon)"))

    # Rule sketch
    s_rule = proof.add(
        "Premise-Rule",
        [],
        Conclusion("rule",
            "{ ?T h:inOneOf ?LIST. ?C list:in ?LIST. ?C t:position-tensor (?LA ?LO). }"
            " => { ?T h:location (?LA ?LO). }"
        ),
        notes="We will enumerate all ?C ∈ ?LIST"
    )

    # Enumerate members explicitly
    s_list = proof.add("List-Enumerate", [s_rule],
                       Conclusion("text", "Members: " + ", ".join(H_IN_ONE_OF)))

    for C in H_IN_ONE_OF:
        s_in = proof.add("List-In", [s_list], Conclusion("text", f"{C} ∈ LIST"))
        if C in POS_TENSOR:
            la, lo = POS_TENSOR[C]
            pair = fmt_pair(la, lo)
            s_tensor = proof.add("Use-Tensor", [s_in], Conclusion("text", f"{C} t:position-tensor {pair}"))
            kb.add(TRAVELER, "h:location", pair)
            proof.add("Head-Intro", [s_tensor],
                      Conclusion("formula", f"{TRAVELER} h:location {pair}"),
                      notes="Instantiate rule head with (?LA ?LO)")
        else:
            proof.add("No-Tensor", [s_in], Conclusion("text", f"{C} has no tensor — no emission"))

# -----------------------------------------------------------------------------
# Driver (query answering + proof)
# -----------------------------------------------------------------------------

def main():
    kb = KB()
    proof = Proof()

    # Derive all locations from the hypothesis list
    derive_locations_full(kb, proof)

    # Check observation
    entailed = (TRAVELER, "h:location", OBS_LOC) in kb.triples

    # Print derived locations
    print("# Derived locations for h:traveler1")
    for s, p, o in sorted(kb.ask(p="h:location")):
        print(f"{s} {p} {o}")

    print("\n# Observation")
    print(f"{TRAVELER} h:location {OBS_LOC}  --  {'ENTAILED' if entailed else 'NOT entailed'}")

    # Goal & short closing
    proof.add("Goal", [], Conclusion("goal", f"{TRAVELER} h:location {OBS_LOC}"))
    proof.add("Observation-Check", [], Conclusion("text", "Entailed by the Ghent member case"))

    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

if __name__ == "__main__":
    main()

