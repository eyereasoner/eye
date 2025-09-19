#!/usr/bin/env python3
"""
Hypothesis → Observation (list membership + tensor unpacking), ARC-ified
========================================================================

Data:
  t:* t:position-tensor (lat lon).

Hypothesis:
  h:traveler1 h:inOneOf
    (t:Antwerp-Belgium t:Boston-USA t:Cambridge-USA t:Ghent-Belgium t:Ostend-Belgium).

Rule:
  { ?T h:inOneOf ?LIST. ?C list:in ?LIST. ?C t:position-tensor (?LA ?LO). }
  =>
  { ?T h:location (?LA ?LO). }

Query/Observation:
  { h:traveler1 h:location (51.0535 3.7304). } => { h:traveler1 h:location (51.0535 3.7304). }.

This script:
  1) Injects tensor facts and the hypothesis list.
  2) Enumerates every LIST member (?C), unpacks its tensor, and derives one h:location for the traveler.
  3) Prints ARC sections:
       • Answer — all derived h:location facts
       • Reason why — short explanation and the member that proves the observation
       • Check (harness) — determinism, exact set equality, and observation entailed
  4) Shows a compact pretty-proof trace with per-member steps.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, List, Optional, Set, Tuple, Dict

# ─────────────────────────────────────────────────────────────
# Data
# ─────────────────────────────────────────────────────────────

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

# ─────────────────────────────────────────────────────────────
# Tiny triple store
# ─────────────────────────────────────────────────────────────

Triple = Tuple[str, str, str]

class KB:
    def __init__(self):
        self.triples: Set[Triple] = set()
    def add(self, s: str, p: str, o: str):
        self.triples.add((s, p, o))
    def ask(self, p: Optional[str] = None) -> List[Triple]:
        return [t for t in self.triples if p is None or t[1] == p]

# ─────────────────────────────────────────────────────────────
# Pretty-proof kernel
# ─────────────────────────────────────────────────────────────

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

# ─────────────────────────────────────────────────────────────
# Derivation (enumerate ALL member cases explicitly)
# ─────────────────────────────────────────────────────────────

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
        notes="We enumerate each ?C ∈ ?LIST"
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

# ─────────────────────────────────────────────────────────────
# ARC sections
# ─────────────────────────────────────────────────────────────

def print_answer(kb: KB) -> List[Triple]:
    print("Answer")
    print("------")
    locs = sorted(kb.ask(p="h:location"))
    for s, p, o in locs:
        print(f"{s} {p} {o}")
    return locs

def print_reason(kb: KB) -> None:
    print("\nReason why")
    print("----------")
    print("• The rule matches each member ?C of the hypothesis list and unpacks")
    print("  its tensor (?LA ?LO), emitting one h:location for the traveler.")
    # Point out which member proves the observation
    ghent_pair = fmt_pair(*POS_TENSOR["t:Ghent-Belgium"])
    entailed = (TRAVELER, "h:location", ghent_pair) in kb.triples
    print(f"• Observation pair {ghent_pair} arises from member t:Ghent-Belgium ({'entailed' if entailed else 'not entailed'}).")

def arc_check(locs: List[Triple], kb: KB) -> None:
    print("\nCheck (harness)")
    print("---------------")

    # 1) Determinism — recompute in a fresh KB+proof
    kb2 = KB(); proof2 = Proof()
    derive_locations_full(kb2, proof2)
    locs2 = sorted(kb2.ask(p="h:location"))
    assert locs == locs2, "Non-deterministic result: recomputation differs."

    # 2) Exact set — should be the list members that have tensors, mapped to pairs
    expected = sorted(
        (TRAVELER, "h:location", fmt_pair(*POS_TENSOR[c]))
        for c in H_IN_ONE_OF
        if c in POS_TENSOR
    )
    assert locs == expected, "Derived locations mismatch the hypothesis∩tensor set."

    # 3) Observation entailed
    assert (TRAVELER, "h:location", OBS_LOC) in kb.triples, "Observation not entailed."

    # 4) Uniqueness: no duplicate locations
    assert len(locs) == len(set(locs)), "Duplicate derived locations found."

    print("All checks passed.")
    print(f"  members in list          = {len(H_IN_ONE_OF)}")
    print(f"  members with tensors     = {len(expected)}")
    print(f"  derived h:location facts = {len(locs)}")

# ─────────────────────────────────────────────────────────────
# Driver (query answering + proof)
# ─────────────────────────────────────────────────────────────

def main():
    kb = KB()
    proof = Proof()

    # Derive all locations from the hypothesis list
    derive_locations_full(kb, proof)

    # ARC: Answer
    locs = print_answer(kb)

    # ARC: Reason why
    print_reason(kb)

    # Pretty proof
    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

    # ARC: Check
    arc_check(locs, kb)

if __name__ == "__main__":
    main()

