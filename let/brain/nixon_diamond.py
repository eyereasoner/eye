#!/usr/bin/env python3
"""
Nixon Diamond (EYE-style N3) — forward-chaining with a SNAF-guarded default
---------------------------------------------------------------------------

Mirrors:
- Facts: ex:RichardNixon a ex:Quaker, ex:Republican .
- Rule 1 (lower priority):  Quakers are Pacifist  (only if NOT already NonPacifist)
    { ?x a ex:Quaker . ?SCOPE log:notIncludes { ?x a ex:NonPacifist } } => { ?x a ex:Pacifist } .
- Rule 2 (higher priority): Republicans are NonPacifist
    { ?x a ex:Republican } => { ?x a ex:NonPacifist } .
- Query: { ?x a ?y } log:impliesAnswer { ?x a ?y }.

This script:
  1) Loads the facts.
  2) Applies Rule 2 first (higher priority), then Rule 1 with a SNAF guard.
  3) Prints all derived type assertions (?x a ?y).
  4) Prints a goal-oriented, pretty proof trace.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Dict, Iterable, List, Optional, Set, Tuple

# ---------------------------------------------------------------------------
# Data model
# ---------------------------------------------------------------------------

Triple = Tuple[str, str, str]  # (subject, predicate, object) using 'a' as predicate

EX = "ex:"
RICHARD = EX + "RichardNixon"
QUAKER = EX + "Quaker"
REPUBLICAN = EX + "Republican"
PACIFIST = EX + "Pacifist"
NONPACIFIST = EX + "NonPacifist"

# Initial fact base (from the N3 file)
FACTS: Set[Triple] = {
    (RICHARD, "a", QUAKER),
    (RICHARD, "a", REPUBLICAN),
}

# ---------------------------------------------------------------------------
# Pretty-proof kernel (in the spirit of odrl_tc1.py dumps)
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class Atom:
    pred: str
    args: Tuple[Any, ...]
    def pretty(self) -> str:
        def fmt(x: Any) -> str:
            return x if isinstance(x, str) else str(x)
        return f"{self.pred}(" + ", ".join(fmt(a) for a in self.args) + ")"

@dataclass
class Conclusion:
    kind: str
    payload: Any
    def pretty(self) -> str:
        if self.kind in ("formula", "goal", "text", "rule"):
            return self.payload if isinstance(self.payload, str) else str(self.payload)
        if hasattr(self.payload, "pretty"):
            return self.payload.pretty()
        return str(self.payload)

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

# ---------------------------------------------------------------------------
# Rule application
# ---------------------------------------------------------------------------

def known_type(facts: Set[Triple], x: str, t: str) -> bool:
    return (x, "a", t) in facts

def apply_rule2_higher_priority(facts: Set[Triple], proof: Proof) -> None:
    """
    Rule 2 (higher priority): { ?x a ex:Republican } => { ?x a ex:NonPacifist } .
    Apply to saturation (here it's just one individual).
    """
    s_intro = proof.add(
        "Premise-Rule",
        [],
        Conclusion("rule", "{ ?x a ex:Republican } => { ?x a ex:NonPacifist } ."),
        notes="Higher priority"
    )
    fired = False
    # Find all Republicans
    xs = {s for (s, p, o) in facts if p == "a" and o == REPUBLICAN}
    for x in xs:
        if not known_type(facts, x, NONPACIFIST):
            facts.add((x, "a", NONPACIFIST))
            fired = True
            proof.add(
                "Apply-Rule2",
                [s_intro],
                Conclusion("formula", f"{x} a {NONPACIFIST}"),
                notes=f"Since {x} a {REPUBLICAN}"
            )
    if not fired:
        proof.add(
            "Apply-Rule2",
            [s_intro],
            Conclusion("text", "no new facts"),
            notes="Already saturated"
        )

def apply_rule1_lower_priority_with_snaf(facts: Set[Triple], proof: Proof) -> None:
    """
    Rule 1 (lower priority): { ?x a ex:Quaker . ?SCOPE log:notIncludes { ?x a ex:NonPacifist } } => { ?x a ex:Pacifist } .
    We implement the SNAF guard by checking the *current* knowledge base for the absence of NonPacifist.
    """
    s_intro = proof.add(
        "Premise-Rule",
        [],
        Conclusion("rule", "{ ?x a ex:Quaker . notIncludes { ?x a ex:NonPacifist } } => { ?x a ex:Pacifist } ."),
        notes="Lower priority (default with SNAF guard)"
    )

    # Consider all Quakers
    xs = {s for (s, p, o) in facts if p == "a" and o == QUAKER}
    for x in xs:
        guard_holds = not known_type(facts, x, NONPACIFIST)
        if guard_holds and not known_type(facts, x, PACIFIST):
            facts.add((x, "a", PACIFIST))
            proof.add(
                "Apply-Rule1",
                [s_intro],
                Conclusion("formula", f"{x} a {PACIFIST}"),
                notes=f"SNAF guard passed (no {x} a {NONPACIFIST} in KB)"
            )
        else:
            why = "blocked: NonPacifist present" if not guard_holds else "already derived"
            proof.add(
                "Apply-Rule1",
                [s_intro],
                Conclusion("text", f"no new facts for {x} ({why})"),
                notes="SNAF guard check"
            )

# ---------------------------------------------------------------------------
# Query and driver
# ---------------------------------------------------------------------------

def main():
    # Prepare proof and initial facts
    proof = Proof()

    # [1] Show facts
    s_facts = proof.add(
        "Facts",
        [],
        Conclusion("text", f"{RICHARD} a {QUAKER}, {REPUBLICAN}")
    )

    # [2] Goal
    s_goal = proof.add("Goal", [], Conclusion("goal", "{ ?x a ?y }"))

    # [3] Apply higher-priority rule first
    apply_rule2_higher_priority(FACTS, proof)

    # [4] Then attempt lower-priority default (with SNAF guard)
    apply_rule1_lower_priority_with_snaf(FACTS, proof)

    # Answers to the query (?x a ?y): enumerate all type triples
    answers = sorted({f"{s} a {o}" for (s, p, o) in FACTS if p == "a"})

    # [5] Answer presentation
    proof.add(
        "Answer",
        [s_goal],
        Conclusion("text", " ; ".join(answers)),
        notes="All entailed type assertions"
    )

    # Print results
    print("\n# Derived type assertions (?x a ?y)")
    for line in answers:
        print(line)

    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

if __name__ == "__main__":
    main()

