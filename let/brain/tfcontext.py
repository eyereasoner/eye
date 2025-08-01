#!/usr/bin/env python3
"""
tfcontext → geo:lat/geo:long + e:findall-neighborhood (< 1.0 degrees)
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Set, Tuple
import math

# -----------------------------------------------------------------------------
# Data (from your N3)
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

# -----------------------------------------------------------------------------
# Tiny RDF-ish store
# -----------------------------------------------------------------------------

Triple = Tuple[str, str, str]  # (s, p, o)

class KB:
    def __init__(self):
        self.triples: Set[Triple] = set()

    def add(self, s: str, p: str, o: str):
        self.triples.add((s, p, o))

    def ask(self, s: Optional[str]=None, p: Optional[str]=None, o: Optional[str]=None):
        for (S,P,O) in self.triples:
            if (s is None or s == S) and (p is None or p == P) and (o is None or o == O):
                yield (S,P,O)

# -----------------------------------------------------------------------------
# Pretty-proof kernel (like earlier examples)
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

# -----------------------------------------------------------------------------
# Rules
# -----------------------------------------------------------------------------

def apply_tensor_to_geo(kb: KB, proof: Proof) -> None:
    """
    { ?C t:position-tensor (?LA ?LO) } => { ?C geo:lat ?LA . ?C geo:long ?LO . }
    """
    s_intro = proof.add("Premise-Rule", [], Conclusion("rule",
        "{ ?C t:position-tensor (?LA ?LO). } => { ?C geo:lat ?LA. ?C geo:long ?LO. }."))

    for C, (LA, LO) in POS_TENSOR.items():
        kb.add(C, "t:position-tensor", f"({LA} {LO})")
        kb.add(C, "geo:lat",  f"{LA}")
        kb.add(C, "geo:long", f"{LO}")
        proof.add("Apply-Rule", [s_intro],
                  Conclusion("text", f"{C} geo:lat {LA} ; geo:long {LO}"),
                  notes="unpacked from position-tensor")

def e_findall_neighbors(kb: KB, proof: Proof, threshold: float = 1.0) -> List[Tuple[str, List[str]]]:
    """
    Implements the query rule:

    If C has geo:lat LAC and geo:long LOC, then
      e:findall( ?T { T has lat/long; T != C; sqrt((LA-LAC)^2 + (LO-LOC)^2) < 1.0 } ?L )
    and conclude  C t:answer L.

    Returns list of (C, L) for printing.
    """
    s_intro = proof.add("Premise-Rule", [], Conclusion("rule",
        "{ ?C geo:lat ?LAC. ?C geo:long ?LOC. e:findall(?T {...} ?L). } => { ?C t:answer ?L. }"),
        notes="query rule with e:findall")

    # Build a quick lookup for lat/long as floats
    lat = {}
    lon = {}
    for (S,P,O) in kb.ask(p="geo:lat"):
        lat[S] = float(O)
    for (S,P,O) in kb.ask(p="geo:long"):
        lon[S] = float(O)

    results = []
    for C in sorted(lat.keys()):
        LAC, LOC = lat[C], lon[C]
        witnesses: List[str] = []
        # Compute all T satisfying the inner pattern
        for T in sorted(lat.keys()):
            if T == C:
                continue  # log:notEqualTo
            LA, LO = lat[T], lon[T]
            D = math.sqrt((LA - LAC)**2 + (LO - LOC)**2)
            if D < threshold:
                witnesses.append(T)
                proof.add("Inner-Solution", [s_intro],
                          Conclusion("text", f"C={C}, T={T}, D={D:.4f} < {threshold} ✓"))
        # Conclude the head with L = list of T
        L_str = "(" + " ".join(witnesses) + ")"
        proof.add("Head-Intro", [s_intro],
                  Conclusion("formula", f"{C} t:answer {L_str}"),
                  notes="e:findall collected the T-solutions")
        results.append((C, witnesses))
    return results

# -----------------------------------------------------------------------------
# Driver
# -----------------------------------------------------------------------------

def main():
    kb = KB()
    proof = Proof()

    # Facts (tensor tuples)
    proof.add("Facts", [], Conclusion("text",
        "t:* t:position-tensor (lat lon) for Antwerp, Boston, Bruges, Cambridge, Chicago, Ghent, Ostend"))

    # Derive geo:lat / geo:long
    apply_tensor_to_geo(kb, proof)

    # Apply the e:findall rule per city
    answers = e_findall_neighbors(kb, proof, threshold=1.0)

    # Print results in the requested style
    print("# Answers (C t:answer L)")
    for C, L in answers:
        print(f"{C} t:answer ({' '.join(L)})")

    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

if __name__ == "__main__":
    main()

