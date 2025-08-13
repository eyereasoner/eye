#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Backward-chaining proof for the “ageAbove” rule (ARC-ified)

N3 rule (tutorial style)
------------------------
{ ?S :ageAbove ?A } <= {
    ?S :birthDay ?B .
    "" time:localTime ?D .
    (?D ?B) math:difference ?F .
    ?F math:greaterThan ?A .
} .

Query
-----
  ?S :ageAbove "P80Y"^^xsd:duration .

Engine notes
------------
• Built-ins supported: time:localTime, math:difference, math:greaterThan
• Dates are ISO (YYYY-MM-DD); durations are ISO-8601 with *years only* ("PnY")
• “Today” is fixed to 2025-06-30 (as in your snippet) so runs are reproducible
• Prints ARC sections: Answer / Reason why / Check (harness)
"""

from __future__ import annotations
from datetime import date, datetime
from itertools import count
from typing import Dict, Iterable, Iterator, List, Optional, Set, Tuple
import re

# ──────────────────────────────────────────────────────────────
# Ground facts
# ──────────────────────────────────────────────────────────────
facts: Set[Tuple[str, str, object]] = {
    (":patH", ":birthDay", "1944-08-21"),   # ISO date string
}

# ──────────────────────────────────────────────────────────────
# Rule
# ──────────────────────────────────────────────────────────────
rule_id   = "R-ageAbove"
rule_head = ("?S", ":ageAbove", "?A")
rule_body = [
    ("?S", ":birthDay", "?B"),
    ("",   "time:localTime", "?D"),
    (("?D","?B"), "math:difference", "?F"),          # subject is a tuple (D,B)
    ("?F", "math:greaterThan", "?A"),
]

# ──────────────────────────────────────────────────────────────
# Utility: parse dates & durations
# ──────────────────────────────────────────────────────────────
def parse_iso_date(s: str) -> date:
    return datetime.strptime(s, "%Y-%m-%d").date()

_duration_pat = re.compile(r"^P(\d+)Y$")

def duration_to_years(dur: str) -> float:
    """Example: 'P80Y' -> 80.0"""
    m = _duration_pat.fullmatch(dur)
    if not m:
        raise ValueError(f"Unsupported duration literal {dur!r} (use 'PnY')")
    return float(m.group(1))

# ──────────────────────────────────────────────────────────────
# Unification helpers
# ──────────────────────────────────────────────────────────────
def is_var(t: object) -> bool:
    return isinstance(t, str) and t.startswith("?")

def unify(pattern, datum, theta: Optional[Dict[str, object]] = None) -> Optional[Dict[str, object]]:
    """Tuplewise unification; variables allowed on the pattern side."""
    θ = dict(theta or {})
    if isinstance(pattern, tuple) != isinstance(datum, tuple):
        return None
    if not isinstance(pattern, tuple):
        if is_var(pattern):
            if pattern in θ and θ[pattern] != datum:
                return None
            θ[pattern] = datum
            return θ
        return θ if pattern == datum else None
    if len(pattern) != len(datum):
        return None
    for p, d in zip(pattern, datum):
        θ = unify(p, d, θ)
        if θ is None:
            return None
    return θ

def subst(term, θ: Dict[str, object]):
    if isinstance(term, tuple):
        return tuple(subst(x, θ) for x in term)
    return θ.get(term, term)

def subst_triple(trp, θ: Dict[str, object]):
    return tuple(subst(t, θ) for t in trp)

# ──────────────────────────────────────────────────────────────
# Built-ins
# ──────────────────────────────────────────────────────────────
def builtin_time_localTime(s, o, θ: Dict[str, object]) -> Optional[Dict[str, object]]:
    """'' time:localTime ?D  — binds ?D to the fixed 'today'."""
    if s != "":
        return None
    today = date(2025, 6, 30)  # fixed “today”
    val = today.isoformat()
    θ2 = dict(θ)
    if is_var(o):
        θ2[o] = val
        return θ2
    return θ2 if o == val else None

def builtin_math_difference(pair, o, θ: Dict[str, object]) -> Optional[Dict[str, object]]:
    """(?D ?B) math:difference ?F  — years between D and B (tropical years)."""
    if not isinstance(pair, tuple) or len(pair) != 2:
        return None
    d_str, b_str = pair
    d_val = parse_iso_date(d_str)
    b_val = parse_iso_date(b_str)
    years = (d_val - b_val).days / 365.2425
    θ2 = dict(θ)
    if is_var(o):
        θ2[o] = years
        return θ2
    return θ2 if o == years else None

def builtin_math_greaterThan(s, o, θ: Dict[str, object]) -> Optional[Dict[str, object]]:
    """?F math:greaterThan ?A   where ?A is a duration 'PnY' or a number."""
    if isinstance(o, str):
        o_val = duration_to_years(o)
    else:
        o_val = o
    return θ if isinstance(s, (int, float)) and s > o_val else None

# ──────────────────────────────────────────────────────────────
# Backward-chaining prover (trace-collecting)
# ──────────────────────────────────────────────────────────────
def bc_prove(goal, θ: Dict[str, object], depth: int, seen, stepctr, trace: List[str]):
    g = subst_triple(goal, θ)
    trace.append("  " * depth + f"Step {next(stepctr):02}: prove {g}")

    subj, pred, obj = g

    # Built-ins (by predicate)
    if pred == "time:localTime":
        θ2 = builtin_time_localTime(subj, obj, θ)
        if θ2 is not None:
            trace.append("  " * depth + f"✓ built-in time:localTime → {θ2.get(obj, obj)}")
            yield θ2
        else:
            trace.append("  " * depth + "✗ built-in time:localTime failed")
        return

    if pred == "math:difference":
        θ2 = builtin_math_difference(subj, obj, θ)
        if θ2 is not None:
            val = θ2.get(obj, obj)
            val_s = f"{val:.2f}y" if isinstance(val, (int, float)) else str(val)
            trace.append("  " * depth + f"✓ built-in math:difference → {val_s}")
            yield θ2
        else:
            trace.append("  " * depth + "✗ built-in math:difference failed")
        return

    if pred == "math:greaterThan":
        θ2 = builtin_math_greaterThan(subj, obj, θ)
        if θ2 is not None:
            rhs = f"{duration_to_years(obj):.0f}y" if isinstance(obj, str) else obj
            trace.append("  " * depth + f"✓ built-in greaterThan: {subj} > {rhs}")
            yield θ2
        else:
            trace.append("  " * depth + f"✗ built-in greaterThan failed: {subj} ≤ {obj}")
        return

    # Ground facts
    for fact in sorted(facts):
        θ2 = unify(g, fact, {})
        if θ2 is not None:
            trace.append("  " * depth + f"✓ fact {fact}")
            yield {**θ, **θ2}
            return

    # Apply the rule if the head matches
    θ_head = unify(rule_head, g, {})
    if θ_head is None:
        return
    head_inst = subst_triple(rule_head, θ_head)
    if head_inst in seen:  # simple loop guard
        return
    trace.append("  " * depth + f"→ via {rule_id}")
    θ_curr = {**θ, **θ_head}

    def prove_body(i: int, θ_now: Dict[str, object]):
        if i == len(rule_body):
            yield θ_now
        else:
            for θ_next in bc_prove(rule_body[i], θ_now, depth + 1, seen | {head_inst}, stepctr, trace):
                yield from prove_body(i + 1, θ_next)

    yield from prove_body(0, θ_curr)

def solve(query) -> Tuple[bool, Dict[str, object], List[str]]:
    trace: List[str] = []
    θ = next(bc_prove(query, {}, 0, frozenset(), count(1), trace), None)
    return (θ is not None), (θ or {}), trace

# ──────────────────────────────────────────────────────────────
# ARC: Answer / Reason / Check
# ──────────────────────────────────────────────────────────────
def arc_answer():
    # Run the canonical query
    query = ("?S", ":ageAbove", "P80Y")
    success, θ, _ = solve(query)

    print("Answer")
    print("------")
    if success:
        s = θ["?S"]
        # Reconstruct the numeric age from bindings if present (for display)
        # We can get ?F by re-running the inner part, but it’s simpler to recompute:
        birth = next(o for (_, p, o) in facts if p == ":birthDay" and isinstance(o, str))
        years = (date(2025, 6, 30) - parse_iso_date(birth)).days / 365.2425
        print(f"Derived: {s} :ageAbove 'P80Y'^^xsd:duration")
        print(f"Numeric age (as of 2025-06-30): ~{years:.2f} years")
    else:
        print("No derivation for :ageAbove 'P80Y'.")
    print()

def arc_reason(max_lines: int = 40):
    query = ("?S", ":ageAbove", "P80Y")
    success, θ, trace = solve(query)

    print("Reason why")
    print("----------")
    print("We use the rule:")
    print("{ ?S :ageAbove ?A }  <=  { ?S :birthDay ?B . '' time:localTime ?D . (?D ?B) math:difference ?F . ?F math:greaterThan ?A . }")
    print("Instantiations here:")
    print("  • ?S = :patH")
    print("  • '' time:localTime ?D  ⇒  ?D = '2025-06-30'")
    print("  • (?D ?B) math:difference ?F  ⇒  years from 1944-08-21 to 2025-06-30 ≈ 80.86")
    print("  • 80.86 > 'P80Y'  ⇒  rule head fires")
    print()
    print("Proof trace:")
    for line in trace[:max_lines]:
        print(line)
    if len(trace) > max_lines:
        print(f"… {len(trace) - max_lines} more step(s) …")
    print()

def arc_check():
    """
    Harness:
      1) With birthday 1944-08-21 and today=2025-06-30, :ageAbove P80Y holds.
      2) :ageAbove P100Y must NOT hold.
      3) Built-in identities behave as intended.
    """
    print("Check (harness)")
    print("---------------")
    # 1) P80Y succeeds
    ok80, θ80, _ = solve(("?S", ":ageAbove", "P80Y"))
    assert ok80 and θ80.get("?S") == ":patH", "Expected :patH to satisfy :ageAbove P80Y."
    # 2) P100Y fails
    ok100, _, _ = solve(("?S", ":ageAbove", "P100Y"))
    assert not ok100, "Unexpected success for :ageAbove P100Y."
    # 3) Built-ins sanity
    #   time:localTime
    θ = {}
    θ2 = builtin_time_localTime("", "?D", θ)
    assert θ2 and θ2["?D"] == "2025-06-30"
    #   math:difference (numeric)
    θ3 = builtin_math_difference(("2025-06-30", "1944-08-21"), "?F", {})
    assert θ3 and isinstance(θ3["?F"], float) and θ3["?F"] > 80.0
    #   math:greaterThan with durations
    assert builtin_math_greaterThan(80.8586, "P80Y", {}) is not None
    assert builtin_math_greaterThan(80.0, "P80Y", {}) is None

    print("OK: rule fires for P80Y, fails for P100Y, and built-ins behave correctly.")

# ──────────────────────────────────────────────────────────────
# Main
# ──────────────────────────────────────────────────────────────
def main():
    arc_answer()
    arc_reason()
    arc_check()

if __name__ == "__main__":
    main()

