#!/usr/bin/env python3
"""
Qiana (EYE-style N3) — quoting + meta-rules with a goal-oriented proof
---------------------------------------------------------------------

Mirrors (from qiana.n3):
  1) { :Einstein :says ?phi } => { ?x :believes ?phi }        # (∀x,φ)
  2) { :Einstein :says ?phi } => ?phi                         # (authoritative says)
  3) :Einstein :says { { ?x a :glitter } => { ?x :notNecessarilyA :gold } } .
  4) :northStar a :glitter .

Query (from qiana-query.n3):
  - { :Fabian :believes ?what } => { :Fabian :believes ?what } .
  - { ?x :notNecessarilyA ?what } => { ?x :notNecessarilyA ?what } .

This script:
  • Encodes the facts and rules exactly as above (with quoting for the said formula).
  • Applies the meta-rules:
      - From “Einstein says φ” derive everyone-believes φ (we’ll show Fabian explicitly).
      - From “Einstein says φ” assert φ (if φ is a rule: add rule; if φ is a fact: add fact).
  • Uses the asserted glitter→notNecessarilyA rule on the example fact.
  • Answers the query and prints a pretty, goal-oriented proof trace.

Output highlights:
  - :Fabian :believes { { ?x a :glitter } => { ?x :notNecessarilyA :gold } }
  - :northStar :notNecessarilyA :gold
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Set, Tuple

# ---------------------------------------------------------------------------
# Simple data model (triples, quoted formulas, rules)
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class Triple:
    s: str
    p: str
    o: str  # when quoting a formula as object, we represent it as a string label

@dataclass(frozen=True)
class Rule:
    """A simple Horn rule with one triple pattern in body and one triple in head."""
    body: Triple
    head: Triple

# Pretty-proof kernel (modeled after prior answers)
@dataclass(frozen=True)
class Atom:
    pred: str
    args: Tuple[Any, ...]
    def pretty(self) -> str:
        fmt = lambda x: x if isinstance(x, str) else str(x)
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
# KB initialization from qiana.n3
# ---------------------------------------------------------------------------

# Names
Einstein = ":Einstein"
Fabian = ":Fabian"

# The quoted rule φ_E:  { ?x a :glitter } => { ?x :notNecessarilyA :gold }
phi_label = "{ { ?x a :glitter } => { ?x :notNecessarilyA :gold } }"
phi_rule = Rule(
    body=Triple("?x", "a", ":glitter"),
    head=Triple("?x", ":notNecessarilyA", ":gold"),
)

# Facts: Einstein says φ_E ; example glitter fact
FACTS: Set[Triple] = {
    Triple(Einstein, ":says", phi_label),
    Triple(":northStar", "a", ":glitter"),
}

# Dynamic rule set (φ asserted via the meta-rule)
RULES: List[Rule] = []

# ---------------------------------------------------------------------------
# Inference: apply the two meta-rules and then the asserted φ_E
# ---------------------------------------------------------------------------

def apply_meta_rules_and_derive(proof: Proof) -> None:
    # Present meta-rules (sketch)
    r1 = proof.add(
        "Premise-Rule",
        [],
        Conclusion("rule", "{ :Einstein :says ?phi } => { ?x :believes ?phi } ."),
        notes="says→believes (universal :x)"
    )
    r2 = proof.add(
        "Premise-Rule",
        [],
        Conclusion("rule", "{ :Einstein :says ?phi } => ?phi ."),
        notes="says→assert (admit φ into KB)"
    )

    # Gather all φ with :Einstein :says φ
    said_phis = [t.o for t in FACTS if t.s == Einstein and t.p == ":says"]

    # Apply says→believes: show specifically that Fabian believes each φ
    for phi in said_phis:
        FACTS.add(Triple(Fabian, ":believes", phi))
        proof.add(
            "Apply says→believes",
            [r1],
            Conclusion("formula", f"{Fabian} :believes {phi}"),
            notes="Instantiate ?x = :Fabian"
        )

    # Apply says→assert: if φ is φ_E (our labeled rule), admit rule; (if φ were a ground triple, we’d add it to FACTS)
    for phi in said_phis:
        if phi == phi_label:
            RULES.append(phi_rule)
            proof.add(
                "Apply says→assert",
                [r2],
                Conclusion("text", "Admitted rule: { ?x a :glitter } => { ?x :notNecessarilyA :gold }"),
                notes="Quoted formula added as a rule"
            )

    # Now use the admitted rule on existing facts
    # For every Triple(s, p, o) that matches body (?x a :glitter), add head (?x :notNecessarilyA :gold)
    fired = False
    for t in list(FACTS):
        if t.p == "a" and t.o == ":glitter":
            x = t.s
            concl = Triple(x, ":notNecessarilyA", ":gold")
            if concl not in FACTS:
                FACTS.add(concl)
                fired = True
                proof.add(
                    "Apply φ_E",
                    [],
                    Conclusion("formula", f"{x} :notNecessarilyA :gold"),
                    notes="From admitted rule and glitter fact"
                )
    if not fired:
        proof.add("Apply φ_E", [], Conclusion("text", "no new facts"), notes="Already saturated")

# ---------------------------------------------------------------------------
# Query answering (as in qiana-query.n3)
# ---------------------------------------------------------------------------

def main():
    proof = Proof()

    # Facts intro (brief)
    proof.add(
        "Facts",
        [],
        Conclusion("text", f"{Einstein} :says {phi_label} ; :northStar a :glitter")
    )

    # Apply meta-rules and then φ_E
    apply_meta_rules_and_derive(proof)

    # Query 1: { :Fabian :believes ?what } ⇒ report :Fabian :believes ?what
    belief_answers = sorted([t for t in FACTS if t.s == Fabian and t.p == ":believes"])
    # Query 2: { ?x :notNecessarilyA ?what } ⇒ report those directly
    nnec_answers = sorted([t for t in FACTS if t.p == ":notNecessarilyA"])

    # Present answers
    if belief_answers or nnec_answers:
        proof.add("Answer", [], Conclusion(
            "text",
            "; ".join([f"{t.s} {t.p} {t.o}" for t in belief_answers + nnec_answers])
        ), notes="All entailed matches for the two query patterns")

    # Print results in a friendly way
    print("# Answers")
    for t in belief_answers:
        print(f"{t.s} {t.p} {t.o}")
    for t in nnec_answers:
        print(f"{t.s} {t.p} {t.o}")

    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

if __name__ == "__main__":
    main()

