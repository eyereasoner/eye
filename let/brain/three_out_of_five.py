#!/usr/bin/env python3
"""
N3 fragment with log:callWithOptional + math:sum + math:notLessThan
-------------------------------------------------------------------

Given facts:
  :s :p1 true.
  #:s :p2 true.        (absent)
  :s :p3 true.
  :s :p4 true.
  #:s :p5 true.        (absent)
  :s :p6 true.         (unused here)
  :s :p7 true.         (unused here)

Rule:
  For i in {1..5}:
    true log:callWithOptional { :s :pi true. ?Ci = 1 } { ?Ci = 0 }.
  (?C1 ?C2 ?C3 ?C4 ?C5) math:sum ?C.
  ?C math:notLessThan 3.
  ---------------------------------
  =>  :s a :3outof5.

Query asks whether we can conclude: :s a :3outof5.

This program:
  1) Encodes the above facts.
  2) Implements the exact optional-binding behavior.
  3) Computes the sum and threshold test.
  4) Prints the derived triple.
  5) Prints a goal-oriented, pretty proof trace.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, List, Optional, Tuple

# -----------------------------------------------------------------------------
# Tiny fact store and helpers
# -----------------------------------------------------------------------------

Triple = Tuple[str, str, bool]

FACTS: set[Triple] = {
    (":s", ":p1", True),
    # (":s", ":p2", True),   # intentionally absent
    (":s", ":p3", True),
    (":s", ":p4", True),
    # (":s", ":p5", True),   # intentionally absent
    (":s", ":p6", True),     # unused by the rule
    (":s", ":p7", True),     # unused by the rule
}

def has_fact(s: str, p: str, o: bool) -> bool:
    return (s, p, o) in FACTS

# -----------------------------------------------------------------------------
# Pretty-proof kernel (style similar to odrl_tc1.py dumps)
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
# Rule mechanics for this specific N3
# -----------------------------------------------------------------------------

def call_with_optional(pname: str) -> Tuple[int, str]:
    """
    true log:callWithOptional { :s pname true. ?C = 1 } { ?C = 0 }
    If the fact exists, choose left branch (?C=1); otherwise right branch (?C=0).
    Returns (C, explanation).
    """
    if has_fact(":s", pname, True):
        return 1, f"left-branch (found :s {pname} true) ⇒ 1"
    else:
        return 0, f"right-branch (no :s {pname} true) ⇒ 0"

def apply_rule_and_prove() -> Tuple[bool, Proof, dict]:
    proof = Proof()

    # [1] Present the rule sketch
    rule_text = (
        "R (N3): For i∈{1..5}, true log:callWithOptional { :s :pi true. ?Ci=1 } { ?Ci=0 } ; "
        "then sum(?C1..?C5, ?C) and ?C ≥ 3 ⇒ (:s a :3outof5)."
    )
    s1 = proof.add("Premise-Rule", [], Conclusion("rule", rule_text),
                   notes="Single rule concluding the type :3outof5")

    # [2] Goal
    s2 = proof.add("Goal", [], Conclusion("goal", ":s a :3outof5"),
                   notes="Original query")

    # [3] Backchain on the rule
    s3 = proof.add("Backchain", [s1, s2],
                   Conclusion("text", "Reduce to establishing optional counts C1..C5, sum C, and C ≥ 3"),
                   notes="Goal-directed use of rule head")

    # [4..8] Evaluate the five optionals
    ci = {}
    expl = {}

    for i, pname in enumerate([":p1", ":p2", ":p3", ":p4", ":p5"], start=1):
        val, why = call_with_optional(pname)
        ci[f"C{i}"] = val
        expl[f"C{i}"] = why
        proof.add(f"Optional-C{i}", [s3],
                  Conclusion("text", f"{pname}: {why}  ⇒  C{i} = {val}"))

    # [9] Sum the vector
    C = sum(ci[f"C{i}"] for i in range(1, 6))
    vec = "(" + " ".join(str(ci[f"C{i}"]) for i in range(1, 6)) + ")"
    s9 = proof.add("math:sum", [],
                   Conclusion("text", f"{vec} ⇒ sum C = {C}"),
                   notes="math:sum over (C1 C2 C3 C4 C5)")

    # [10] Check threshold
    ok = C >= 3
    s10 = proof.add("math:notLessThan", [s9],
                    Conclusion("text", f"{C} ≥ 3  ⇒  {ok}"),
                    notes="Threshold holds")

    # [11] Conclude the head
    if ok:
        s11 = proof.add("Head-Intro", [s3, s10],
                        Conclusion("formula", ":s a :3outof5"),
                        notes="Rule applied since antecedent holds")
        derived = True
    else:
        s11 = proof.add("Head-Blocked", [s3, s10],
                        Conclusion("text", "Cannot conclude :s a :3outof5"),
                        notes="Antecedent failed")
        derived = False

    return derived, proof, {"C": C, **ci}

# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------

def main():
    derived, proof, vals = apply_rule_and_prove()

    # Print the grounded conclusion (if any)
    if derived:
        print(":s a :3outof5")
    else:
        print("# Not derivable: :s a :3outof5")

    # Also show the chosen C1..C5 and sum, for transparency
    print(f"C1..C5 = {vals['C1']}, {vals['C2']}, {vals['C3']}, {vals['C4']}, {vals['C5']}  (sum={vals['C']})")

    # Pretty proof
    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

if __name__ == "__main__":
    main()

