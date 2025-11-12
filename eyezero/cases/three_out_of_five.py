#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
three_out_of_five.py
──────────────────────────────────────────────────────────────
N3 fragment with log:callWithOptional + math:sum + math:notLessThan

Facts (dataset under test)
--------------------------
  :s :p1 true.
  # :s :p2 true.      (absent)
  :s :p3 true.
  :s :p4 true.
  # :s :p5 true.      (absent)
  :s :p6 true.        (unused here)
  :s :p7 true.        (unused here)

Rule (schematic)
----------------
For i ∈ {1..5}:
  true log:callWithOptional { :s :pi true.  ?Ci = 1 } { ?Ci = 0 } .
(?C1 ?C2 ?C3 ?C4 ?C5) math:sum ?C .
?C math:notLessThan 3 .
---------------------------------
⇒ :s a :3outof5 .

Query
-----
Can we conclude  :s a :3outof5  from the facts above?

ARC output
----------
• Answer      — the derived triple (yes/no) and the computed C1..C5 and sum
• Reason why  — which optional branches were taken, the sum, and threshold test
• Check       — harness that:
    - recomputes truth by direct counting;
    - exhaustively validates all 2^5 presence/absence combinations;
    - shows unused facts (:p6, :p7) do not affect the result;
    - ensures determinism of the derivation.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Dict, Iterable, List, Optional, Tuple, Set

# ─────────────────────────────────────────────────────────────
# Tiny fact store type
# ─────────────────────────────────────────────────────────────
Triple = Tuple[str, str, bool]

DEFAULT_FACTS: Set[Triple] = {
    (":s", ":p1", True),
    # (":s", ":p2", True),   # absent
    (":s", ":p3", True),
    (":s", ":p4", True),
    # (":s", ":p5", True),   # absent
    (":s", ":p6", True),     # unused
    (":s", ":p7", True),     # unused
}

# ─────────────────────────────────────────────────────────────
# Pretty-proof kernel (compact)
# ─────────────────────────────────────────────────────────────
@dataclass
class Step:
    id: int
    rule: str
    text: str
    refs: List[int] = field(default_factory=list)

@dataclass
class Proof:
    steps: List[Step] = field(default_factory=list)
    def add(self, rule: str, text: str, refs: Iterable[int] = ()) -> int:
        sid = len(self.steps) + 1
        self.steps.append(Step(sid, rule, text, list(refs)))
        return sid
    def pretty(self) -> str:
        lines: List[str] = []
        for s in self.steps:
            refs = f" [{', '.join(map(str, s.refs))}]" if s.refs else ""
            lines.append(f"[{s.id}] {s.rule}{refs}: {s.text}")
        return "\n".join(lines)

# ─────────────────────────────────────────────────────────────
# Rule mechanics for this specific N3
# ─────────────────────────────────────────────────────────────
def has_fact(facts: Set[Triple], s: str, p: str, o: bool) -> bool:
    return (s, p, o) in facts

def call_with_optional(facts: Set[Triple], pname: str) -> Tuple[int, str]:
    """
    true log:callWithOptional { :s pname true. ?C=1 } { ?C=0 }
    If the fact exists, choose left branch (?C=1); otherwise right (?C=0).
    Returns (C, human_explanation).
    """
    if has_fact(facts, ":s", pname, True):
        return 1, f"left-branch (found :s {pname} true) ⇒ 1"
    else:
        return 0, f"right-branch (no :s {pname} true) ⇒ 0"

def apply_rule_and_prove(facts: Set[Triple]) -> Tuple[bool, Proof, Dict[str, int]]:
    proof = Proof()

    # Present the rule sketch & goal
    r = proof.add("Rule", "For i∈{1..5} do log:callWithOptional …; sum C1..C5 → C; require C≥3 ⇒ :s a :3outof5")
    g = proof.add("Goal", ":s a :3outof5")

    # Optionals
    ci: Dict[str, int] = {}
    branches: Dict[str, str] = {}
    for i, pname in enumerate([":p1", ":p2", ":p3", ":p4", ":p5"], start=1):
        val, why = call_with_optional(facts, pname)
        ci[f"C{i}"] = val
        branches[f"C{i}"] = why
        proof.add(f"Optional C{i}", f"{pname}: {why}  ⇒  C{i}={val}", refs=[r, g])

    # Sum and threshold
    C = sum(ci[f"C{i}"] for i in range(1, 6))
    v = "(" + " ".join(str(ci[f"C{i}"]) for i in range(1, 6)) + ")"
    s = proof.add("math:sum", f"{v} ⇒ C={C}")
    ok = C >= 3
    proof.add("math:notLessThan", f"{C} ≥ 3  ⇒  {ok}", refs=[s])

    # Head conclusion
    if ok:
        proof.add("Conclude", ":s a :3outof5", refs=[g])
    else:
        proof.add("Blocked", "Cannot conclude :s a :3outof5", refs=[g])

    return ok, proof, {"C": C, **ci}

# ─────────────────────────────────────────────────────────────
# ARC — Answer / Reason why / Check
# ─────────────────────────────────────────────────────────────
def arc_answer(facts: Set[Triple]) -> Tuple[bool, Proof, Dict[str, int]]:
    ok, proof, vals = apply_rule_and_prove(facts)
    C1, C2, C3, C4, C5, C = vals["C1"], vals["C2"], vals["C3"], vals["C4"], vals["C5"], vals["C"]
    print("Answer")
    print("------")
    print(":s a :3outof5" if ok else "# Not derivable: :s a :3outof5")
    print(f"C1..C5 = {C1}, {C2}, {C3}, {C4}, {C5}  (sum={C})\n")
    return ok, proof, vals

def arc_reason(proof: Proof) -> None:
    print("Reason why")
    print("----------")
    print("• Each optional chooses 1 if the corresponding fact :s :pi true is present; else 0.")
    print("• The five choices are summed via math:sum to C, then we require C ≥ 3.")
    print("• If true, the rule’s head :s a :3outof5 is concluded.\n")
    print("Pretty proof\n------------")
    print(proof.pretty())
    print()

def arc_check(facts: Set[Triple], ok: bool, vals: Dict[str, int]) -> None:
    print("Check (harness)")
    print("---------------")
    # 1) Direct counting equals computed sum
    present = [p for p in [":p1", ":p2", ":p3", ":p4", ":p5"] if has_fact(facts, ":s", p, True)]
    expected_sum = len(present)
    assert vals["C"] == expected_sum, f"Sum mismatch: got {vals['C']}, expected {expected_sum}"

    # 2) Threshold equivalence
    assert ok == (vals["C"] >= 3), "Derived truth does not match threshold C≥3"

    # 3) Unused facts do not influence the result
    #    Toggle :p6 / :p7 presence; outcome must be identical.
    def with_toggle(pname: str, present: bool) -> Set[Triple]:
        new = set(facts)
        triple = (":s", pname, True)
        if present:
            new.add(triple)
        else:
            new.discard(triple)
        return new

    for p in (":p6", ":p7"):
        ok_on  = apply_rule_and_prove(with_toggle(p, True))[0]
        ok_off = apply_rule_and_prove(with_toggle(p, False))[0]
        assert ok_on == ok_off == ok, f"Unused {p} changed the outcome"

    # 4) Exhaustive validation over all 2^5 patterns (p1..p5)
    base = {(s, p, o) for (s, p, o) in facts if p not in {":p1", ":p2", ":p3", ":p4", ":p5"}}
    pins = [":p1", ":p2", ":p3", ":p4", ":p5"]
    for mask in range(32):
        test_facts = set(base)
        count = 0
        for i, p in enumerate(pins):
            if (mask >> i) & 1:
                test_facts.add((":s", p, True))
                count += 1
        ok_test, _, vals_test = apply_rule_and_prove(test_facts)
        assert vals_test["C"] == count, "math:sum disagreement in exhaustive check"
        assert ok_test == (count >= 3), "Threshold disagreement in exhaustive check"

    # 5) Determinism: run twice on the same facts
    ok2, _, vals2 = apply_rule_and_prove(facts)
    assert ok2 == ok and vals2 == vals, "Non-deterministic result on re-run"

    print("OK: sums correct, threshold respected, unused facts ignored, exhaustive cases consistent, deterministic.\n")

# ─────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    ok, proof, vals = arc_answer(DEFAULT_FACTS)
    arc_reason(proof)
    arc_check(DEFAULT_FACTS, ok, vals)

