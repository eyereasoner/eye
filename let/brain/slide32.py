#!/usr/bin/env python3
"""
Slide 32 (Pat Hayes talk) — tiny N3 to Python
---------------------------------------------

Facts:
  :Ghent a :City .

Rule:
  { ?x a :City } => { ?x a :HumanCommunity } .

Query:
  { ?S a ?C } => { ?S a ?C } .
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, List, Optional, Set, Tuple

# -----------------------------------------------------------------------------
# Simple RDF-ish store
# -----------------------------------------------------------------------------

Triple = Tuple[str, str, str]  # (s, p, o), with 'a' as the rdf:type predicate

FACTS: Set[Triple] = {
    (":Ghent", "a", ":City"),
}

# -----------------------------------------------------------------------------
# Pretty-proof kernel (same style as earlier examples)
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
        out = []
        for s in self.steps:
            prem = f" [{', '.join(map(str, s.premises))}]" if s.premises else ""
            note = f" // {s.notes}" if s.notes else ""
            out.append(f"[{s.id}] {s.rule}{prem}: {s.conclusion.pretty()}{note}")
        return "\n".join(out)

# -----------------------------------------------------------------------------
# Rule application
# -----------------------------------------------------------------------------

def apply_city_to_hc_rule(facts: Set[Triple], proof: Proof) -> None:
    """
    { ?x a :City } => { ?x a :HumanCommunity } .
    Forward-chain once (suffices here).
    """
    s_intro = proof.add(
        "Premise-Rule",
        [],
        Conclusion("rule", "{ ?x a :City } => { ?x a :HumanCommunity } ."),
        notes="Universal over ?x"
    )

    new = False
    # For every ?x such that ?x a :City, assert ?x a :HumanCommunity
    for (s, p, o) in list(facts):
        if p == "a" and o == ":City":
            concl = (s, "a", ":HumanCommunity")
            if concl not in facts:
                facts.add(concl)
                new = True
                proof.add(
                    "Apply-Rule",
                    [s_intro],
                    Conclusion("text", f"{s} a :HumanCommunity"),
                    notes=f"Since {s} a :City"
                )
    if not new:
        proof.add("Apply-Rule", [s_intro], Conclusion("text", "no new facts"), notes="Already saturated")

# -----------------------------------------------------------------------------
# Driver (query answering + proof)
# -----------------------------------------------------------------------------

def main():
    proof = Proof()

    # [1] Facts
    proof.add("Facts", [], Conclusion("text", ":Ghent a :City"))

    # [2] Goal
    proof.add("Goal", [], Conclusion("text", "{ ?S a ?C }"))

    # [3] Apply the rule
    apply_city_to_hc_rule(FACTS, proof)

    # Answers to the query { ?S a ?C }
    answers = sorted([f"{s} a {o}" for (s, p, o) in FACTS if p == "a"])

    # [4] Answer presentation
    proof.add("Answer", [], Conclusion("text", " ; ".join(answers)), notes="All entailed type assertions")

    # Print results
    print("# Derived type assertions (?S a ?C)")
    for line in answers:
        print(line)

    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

if __name__ == "__main__":
    main()

