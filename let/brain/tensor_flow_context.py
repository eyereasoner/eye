#!/usr/bin/env python3
"""
tfcontext → geo:lat/geo:long + e:findall-neighborhood (< 1.0 degrees)
=====================================================================

Given facts (position tensors):
  t:Antwerp-Belgium  (51.2192,   4.4029)
  t:Boston-USA       (42.3584, -71.0598)
  t:Bruges-Belgium   (51.2108,   3.2249)
  t:Cambridge-USA    (42.3736, -71.1097)
  t:Chicago-USA      (41.8781, -87.6298)
  t:Ghent-Belgium    (51.0535,   3.7304)
  t:Ostend-Belgium   (51.2154,   2.9286)

Rule 1 (unpack tensor):
  { ?C t:position-tensor (?LA ?LO) }
  =>
  { ?C geo:lat ?LA . ?C geo:long ?LO . }.

Rule 2 (query with e:findall):
  Given C has geo:lat LAC and geo:long LOC,
  collect all T (T ≠ C) such that
    sqrt((LA - LAC)^2 + (LO - LOC)^2) < 1.0
  into list L, and conclude
    { ?C t:answer ?L }.

This script:
  1) Injects the tensor facts and derives geo:lat / geo:long (Rule 1).
  2) For each city C, computes neighbors T within 1.0 degrees (Rule 2).
  3) Prints an ARC-style report:
       • Answer        – all derived  C t:answer (…)
       • Reason why    – summary + first few inner “solutions”
       • Check         – harness assertions (symmetry, no self, determinism)
  4) Shows a compact pretty-proof trace (rule intros + applications).

Notes
-----
• Distance is deliberately Euclidean over degrees (as in your N3 fragment),
  not geodesic; this exactly mirrors the intended behavior.
• Output order is deterministic (sorted by city id and then witness id).
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Set, Tuple
import math
import time
import copy

# ─────────────────────────────────────────────────────────────
# Data (position tensors)
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

THRESHOLD = 1.0  # degrees

# ─────────────────────────────────────────────────────────────
# Tiny RDF-ish store
# ─────────────────────────────────────────────────────────────

Triple = Tuple[str, str, str]  # (s, p, o)

class KB:
    def __init__(self):
        self.triples: Set[Triple] = set()

    def add(self, s: str, p: str, o: str):
        self.triples.add((s, p, o))

    def ask(self, s: Optional[str]=None, p: Optional[str]=None, o: Optional[str]=None):
        for (S, P, O) in self.triples:
            if (s is None or s == S) and (p is None or p == P) and (o is None or o == O):
                yield (S, P, O)

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
    def add(self, rule: str, premises: List[int], conclusion: Conclusion, notes: Optional[str]=None) -> int:
        sid = len(self.steps) + 1
        self.steps.append(Step(sid, rule, premises, conclusion, notes))
        return sid
    def pretty(self) -> str:
        out = []
        for s in self.steps:
            prem = f" [{', '.join(map(str, s.premises))}]" if s.premises else ""
            note = f" // {s.notes}" if s.notes else ""
            out.append(f"[{s.id}] {s.rule}{prem}: {s.conclusion.pretty()}{note}")
        return "\n".join(out)

# ─────────────────────────────────────────────────────────────
# Rules
# ─────────────────────────────────────────────────────────────

def apply_tensor_to_geo(kb: KB, proof: Proof) -> None:
    """
    Rule 1:
      { ?C t:position-tensor (?LA ?LO) }
      =>
      { ?C geo:lat ?LA . ?C geo:long ?LO . }.
    """
    s_intro = proof.add("Premise-Rule-1", [], Conclusion("rule",
        "{ ?C t:position-tensor (?LA ?LO). } => { ?C geo:lat ?LA. ?C geo:long ?LO. }."))

    for C, (LA, LO) in sorted(POS_TENSOR.items()):
        kb.add(C, "t:position-tensor", f"({LA} {LO})")
        kb.add(C, "geo:lat",  f"{LA}")
        kb.add(C, "geo:long", f"{LO}")
        proof.add("Apply-Rule-1", [s_intro],
                  Conclusion("text", f"{C} geo:lat {LA} ; geo:long {LO}"),
                  notes="unpacked from position-tensor")

def e_findall_neighbors(kb: KB, proof: Proof, threshold: float = THRESHOLD) -> List[Tuple[str, List[str]]]:
    """
    Rule 2 (query rule):
      If C has geo:lat LAC and geo:long LOC,
      collect all T (T ≠ C) where distance((LA,LO),(LAC,LOC)) < threshold
      into a list L and conclude { C t:answer L }.

    Returns list of (C, [neighbors...]).
    """
    s_intro = proof.add("Premise-Rule-2", [], Conclusion("rule",
        "{ ?C geo:lat ?LAC. ?C geo:long ?LOC. e:findall(?T {...} ?L). } => { ?C t:answer ?L. }"),
        notes="query rule with e:findall")

    # Lookup tables for lat/long as floats
    lat: Dict[str, float] = {}
    lon: Dict[str, float] = {}
    for (S, _, O) in kb.ask(p="geo:lat"):  lat[S] = float(O)
    for (S, _, O) in kb.ask(p="geo:long"): lon[S] = float(O)

    results: List[Tuple[str, List[str]]] = []
    for C in sorted(lat.keys()):
        LAC, LOC = lat[C], lon[C]
        witnesses: List[str] = []
        for T in sorted(lat.keys()):
            if T == C:
                continue
            LA, LO = lat[T], lon[T]
            D = math.sqrt((LA - LAC)**2 + (LO - LOC)**2)  # Euclidean-on-degrees
            if D < threshold:
                witnesses.append(T)
                proof.add("Inner-Solution", [s_intro],
                          Conclusion("text", f"C={C}, T={T}, D={D:.4f} < {threshold} ✓"))
        L_str = "(" + " ".join(witnesses) + ")"
        proof.add("Head-Intro", [s_intro],
                  Conclusion("formula", f"{C} t:answer {L_str}"),
                  notes="e:findall collected the T-solutions")
        results.append((C, witnesses))
    return results

# ─────────────────────────────────────────────────────────────
# ARC sections
# ─────────────────────────────────────────────────────────────

def print_answer(answers: List[Tuple[str, List[str]]]) -> None:
    print("Answer")
    print("------")
    for C, L in answers:
        print(f"{C} t:answer ({' '.join(L)})")

def print_reason(proof: Proof, first_n_inner: int = 10) -> None:
    print("\nReason why")
    print("----------")
    print("• Rule 1 unpacks each tfcontext tensor into geo:lat/geo:long.")
    print("• Rule 2 uses e:findall to collect, for each city C, all T ≠ C with")
    print("  Euclidean-on-degrees distance < 1.0. The head concludes C t:answer (… neighbors …).")
    # Show a few inner hits to make it tangible
    inner = [s for s in proof.steps if s.rule == "Inner-Solution"]
    if inner:
        print(f"\nFirst {min(first_n_inner, len(inner))} inner solutions:")
        for s in inner[:first_n_inner]:
            print("  -", s.conclusion.pretty())

def arc_check(answers: List[Tuple[str, List[str]]], kb: KB, threshold: float = THRESHOLD) -> None:
    print("\nCheck (harness)")
    print("---------------")
    t0 = time.time()

    # 1) Determinism: recompute in a fresh KB+proof and compare lists.
    kb2 = KB(); proof2 = Proof()
    apply_tensor_to_geo(kb2, proof2)
    answers2 = e_findall_neighbors(kb2, proof2, threshold)
    assert answers == answers2, "Non-deterministic answers (mismatch on recompute)."

    # 2) No self in any list, lists are sorted unique.
    for C, L in answers:
        assert C not in L, f"Self neighbor present for {C}"
        assert L == sorted(set(L)), f"Witness list not sorted unique for {C}"

    # 3) Symmetry: T in neighbors(C) ⇒ C in neighbors(T)   (with this metric it should hold)
    neigh = {C: set(L) for C, L in answers}
    for C, L in answers:
        for T in L:
            assert C in neigh.get(T, set()), f"Symmetry violation: {T} lists not include {C}"

    # 4) Distance re-check from stored geo:lat/geo:long facts
    lat: Dict[str, float] = {S: float(O) for (S, P, O) in kb.ask(p="geo:lat")}
    lon: Dict[str, float] = {S: float(O) for (S, P, O) in kb.ask(p="geo:long")}
    for C, L in answers:
        LAC, LOC = lat[C], lon[C]
        for T in L:
            d = math.sqrt((lat[T] - LAC)**2 + (lon[T] - LOC)**2)
            assert d < threshold, f"Distance check failed: {C}–{T} = {d} ≥ {threshold}"

    # 5) Spot-check expected pairs given the data (sanity)
    exp_pairs = [
        ("t:Boston-USA", "t:Cambridge-USA"),
        ("t:Antwerp-Belgium", "t:Ghent-Belgium"),
        ("t:Bruges-Belgium", "t:Ghent-Belgium"),
        ("t:Bruges-Belgium", "t:Ostend-Belgium"),
        ("t:Ghent-Belgium", "t:Ostend-Belgium"),
    ]
    for a, b in exp_pairs:
        assert b in neigh.get(a, set()) and a in neigh.get(b, set()), f"Expected neighbor pair missing: {a}–{b}"

    # 6) Chicago should have no neighbors within 1.0°
    assert neigh.get("t:Chicago-USA", set()) == set(), "Chicago should have zero neighbors within 1.0°"

    dt = time.time() - t0
    print("All checks passed.")
    print(f"  cities = {len(answers)}")
    print(f"  total neighbor links = {sum(len(L) for _, L in answers)}")
    print(f"  time = {dt:.3f}s")

# ─────────────────────────────────────────────────────────────
# Driver
# ─────────────────────────────────────────────────────────────

def main():
    kb = KB()
    proof = Proof()

    # Facts intro
    proof.add("Facts", [], Conclusion("text",
        "t:* t:position-tensor (lat lon) for Antwerp, Boston, Bruges, Cambridge, Chicago, Ghent, Ostend"))

    # Derive geo:lat / geo:long
    apply_tensor_to_geo(kb, proof)

    # Compute neighbors (answers)
    answers = e_findall_neighbors(kb, proof, threshold=THRESHOLD)

    # ARC output
    print_answer(answers)
    print_reason(proof, first_n_inner=10)

    # Pretty proof
    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

    # Harness
    arc_check(answers, kb, threshold=THRESHOLD)

if __name__ == "__main__":
    main()

