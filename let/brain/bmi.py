#!/usr/bin/env python3
# -*- coding: utf-8 -*-
r"""
bmi.py — Goal-directed derivation of BMI and weight status (ARC-style)

This is a lightly reworked, comment-rich version of the original program:
- Same data (john, mary; weights in kg; heights in metres).
- Same rules:
    R-bmi:    { ?P weight ?W . ?P height ?H . }       ⇒ { ?P bmi ?B }     where  B = W / H²
    R-status: { ?P bmi ?B . ?B ≥ 25 }                 ⇒ { status(?P,true) }   (overweight threshold)

- Same query:  ?- status(P,true).

What’s new:
- Clear ARC output sections: Answer / Reason why / Check.
- The back-chaining engine records a proof trace (with rule firings and built-ins).
- A harness recomputes everything numerically and checks consistency.

Notation:
- Triples are (subject, predicate, object) with variables like '?P', '?W', '?H'.
- Built-ins run only after a rule body is fully proven (just like in the original).
"""

from __future__ import annotations
from dataclasses import dataclass
from itertools import count
from typing import Dict, Iterable, Iterator, List, Optional, Tuple, Union
import math

# ────────────────────────────────────────────────────────────
# 0) Ground facts (identical data; ONLY THREE items per triple)
# ────────────────────────────────────────────────────────────

facts: set[tuple] = {
    ("john", "weight", 92.0),
    ("john", "height", 1.83),
    ("mary", "weight", 65.0),
    ("mary", "height", 1.71),
}

# ────────────────────────────────────────────────────────────
# 1) Rules (same heads/bodies; built-ins trigger after body)
# ────────────────────────────────────────────────────────────

Rule = Dict[str, Union[str, tuple, list]]

rules: List[Rule] = [
    dict(
        id="R-bmi",
        head=("?P", "bmi", "?B"),
        body=[("?P", "weight", "?W"), ("?P", "height", "?H")],
        builtin="compute_bmi",   # run after body is proven
    ),
    dict(
        id="R-status",
        head=("status", "?P", "true"),
        body=[("?P", "bmi", "?B")],
        builtin="gte25",         # run after body is proven
    ),
]

# ────────────────────────────────────────────────────────────
# 2) Unification helpers (variables start with '?')
# ────────────────────────────────────────────────────────────

Term = Union[str, float, tuple]
Subst = Dict[str, Term]

def is_var(t: Term) -> bool:
    return isinstance(t, str) and t.startswith("?")

def unify(pat: Term, fact: Term, theta: Optional[Subst] = None) -> Optional[Subst]:
    """
    Classic structural unification:
      - tuples unify pointwise
      - variables unify by binding (with light 'self-binding' support)
      - atoms/numbers must match
    """
    θ: Subst = dict(theta or {})

    # normalize (tuple vs non-tuple)
    if isinstance(pat, tuple) != isinstance(fact, tuple):
        # allow variable on either side to bind whole tuple/atom
        if is_var(pat):
            if pat in θ and θ[pat] != fact and θ[pat] != pat:  # 'self-binding' guard
                return None
            θ[pat] = fact
            return θ
        if is_var(fact):
            if fact in θ and θ[fact] != pat and θ[fact] != fact:
                return None
            θ[fact] = pat
            return θ
        return None

    # atoms/numbers
    if not isinstance(pat, tuple):
        if is_var(pat):
            if pat in θ and θ[pat] not in (pat, fact):
                return None
            θ[pat] = fact
            return θ
        return θ if pat == fact else None

    # tuples
    if len(pat) != len(fact):
        return None
    for p, f in zip(pat, fact):
        θ = unify(p, f, θ)
        if θ is None:
            return None
    return θ

def subst(t: Term, θ: Subst) -> Term:
    """Apply a substitution to a term (tuples recursively)."""
    if isinstance(t, tuple):
        return tuple(subst(x, θ) for x in t)
    if is_var(t):
        return θ.get(t, t)
    return t

# ────────────────────────────────────────────────────────────
# 3) Built-ins (post-body computations)
# ────────────────────────────────────────────────────────────

all_bmis: List[Tuple[str, float]] = []   # (person, bmi) in encounter order

def compute_bmi(θ: Subst, log: List[str]) -> Optional[Subst]:
    """
    After proving weight & height:
      θ("?B") := round( ?W / (?H^2), 2 )
    """
    if {"?P", "?W", "?H"} <= θ.keys():
        b = round(float(θ["?W"]) / (float(θ["?H"]) ** 2), 2)
        θ2 = dict(θ); θ2["?B"] = b
        all_bmis.append((str(θ2["?P"]), b))
        log.append(f" ↪ bmi({θ2['?P']}) = {b} kg/m²  (B = W/H²)")
        return θ2
    return None  # not all variables bound yet

def gte25(θ: Subst, log: List[str]) -> Optional[Subst]:
    """
    Overweight threshold: accept only if ?B ≥ 25.
    """
    b = θ.get("?B")
    ok = b is not None and float(b) >= 25.0
    log.append(f" ↪ check {b} ≥ 25 {'✓' if ok else '✗'}")
    return θ if ok else None

builtin_dispatch = {
    "compute_bmi": compute_bmi,
    "gte25": gte25,
}

# ────────────────────────────────────────────────────────────
# 4) Back-chaining engine (depth-first, goal-directed)
# ────────────────────────────────────────────────────────────

@dataclass
class ProofResult:
    goal: tuple
    solutions: List[Subst]
    trace: List[str]

def bc(goal: tuple, θ0: Optional[Subst] = None, depth: int = 0,
       stepcounter: Optional[Iterator[int]] = None, trace: Optional[List[str]] = None) -> Iterator[Subst]:
    """
    Prove a single goal against facts and rules, yielding substitutions.
    We:
      (a) try ground facts first,
      (b) then try rules. When a rule head unifies, we prove its body left-to-right.
          After the body succeeds, we run the rule’s built-in (if any).
    """
    θ0 = dict(θ0 or {})
    step = stepcounter or count(1)
    log = trace if trace is not None else []

    g = subst(goal, θ0)
    log.append(" " * depth + f"Step {next(step):02}: prove {g!r}")

    # (a) Ground facts
    for f in sorted(facts, key=repr):
        θf = unify(g, f, θ0)
        if θf is not None:
            log.append(" " * depth + f"✓ fact {f!r}")
            yield θf

    # (b) Rules
    for r in sorted(rules, key=lambda r: r["id"]):  # deterministic order
        θh = unify(r["head"], g, θ0)
        if θh is None:
            continue
        log.append(" " * depth + f"→ via {r['id']}")
        # prove body left-to-right
        def prove_body(i: int, θcur: Subst) -> Iterator[Subst]:
            if i == len(r["body"]):
                # run built-in AFTER body
                bname = r.get("builtin")
                if bname:
                    θdone = builtin_dispatch[bname](θcur, log)
                    if θdone is not None:
                        yield θdone
                else:
                    yield θcur
                return
            atom = r["body"][i]
            for θn in bc(atom, θcur, depth + 1, step, log):
                yield from prove_body(i + 1, θn)

        yield from prove_body(0, {**θ0, **θh})

def prove(goal: tuple) -> ProofResult:
    step = count(1)
    trace: List[str] = []
    sols = list(bc(goal, {}, 0, step, trace))
    return ProofResult(goal=goal, solutions=sols, trace=trace)

# ────────────────────────────────────────────────────────────
# 5) ARC sections
# ────────────────────────────────────────────────────────────

def arc_answer(proof: ProofResult) -> None:
    print("Answer")
    print("------")
    print("Computed BMI values:")
    if all_bmis:
        for person, bmi in all_bmis:
            print(f"  {person}: {bmi:.2f} kg/m²")
    else:
        print("  (none)")
    print()
    print("Overweight individuals (BMI ≥ 25):")
    if proof.solutions:
        for θ in proof.solutions:
            print(f"  {θ['?P']} (BMI {θ['?B']:.2f} kg/m²)")
    else:
        print("  none")
    print()

def arc_reason(proof: ProofResult, max_lines: int = 40) -> None:
    print("Reason why")
    print("----------")
    print("Rules used:")
    print("  R-bmi    : from weight & height derive bmi B = W/H²")
    print("  R-status : if bmi B ≥ 25 then status(P,true)")
    print()
    print("Proof trace (first lines):")
    head = proof.trace[:max_lines]
    for line in head:
        print(line)
    if len(proof.trace) > max_lines:
        print(f"… {len(proof.trace) - max_lines} more steps …")
    print()

# ────────────────────────────────────────────────────────────
# 6) Check (harness)
# ────────────────────────────────────────────────────────────

def check_harness(proof: ProofResult) -> None:
    """
    Re-verify:
      1) BMI numbers match the numeric definition (rounded to 2 decimals).
      2) Overweight classification uses ≥ 25.
      3) The solver’s substitution for ?P/?B equals the numeric recomputation.
    """
    # 1) direct numeric BMI
    def bmi(w: float, h: float) -> float:
        return round(w / (h * h), 2)

    expected = {
        "john": bmi(92.0, 1.83),  # 27.49
        "mary": bmi(65.0, 1.71),  # 22.24
    }
    for who, val in expected.items():
        assert (who, val) in all_bmis, f"Missing or wrong BMI for {who}: {val}"

    # 2) overweight classification (≥ 25)
    overweight = {p for p, b in expected.items() if b >= 25.0}
    assert overweight == {"john"}, f"Expected only john overweight; got {overweight}"

    # 3) substitution cross-check
    sols = {(str(θ["?P"]), float(θ["?B"])) for θ in proof.solutions}
    assert sols == {("john", expected["john"])}, f"Solution set mismatch: {sols}"

# ────────────────────────────────────────────────────────────
# 7) Main — run the original query
# ────────────────────────────────────────────────────────────

def main():
    goal = ("status", "?P", "true")
    # reset global side-channel
    all_bmis.clear()
    proof = prove(goal)

    # ----- ARC output -----
    arc_answer(proof)
    arc_reason(proof, max_lines=60)

    print("Check (harness)")
    print("---------------")
    try:
        check_harness(proof)
        print("OK: BMI derivations, status classification, and solver substitutions all match.")
    except AssertionError as e:
        print("FAILED:", e)
        raise

if __name__ == "__main__":
    main()

