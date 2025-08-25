#!/usr/bin/env python3
"""
mmln.py
──────────────────────────────────────────────────────────────
Generic backward-proof engine for the Greek-vase MMLN.
See https://github.com/eyereasoner/eye/tree/master/reasoning/mmln

• A *shape* has N weighted rules (boolean predicates against the fact base).
• Fired weight = sum of weights of satisfied rules.
• Inductivity score P(shape|facts) = Fired / Total_for_shape.
• We evaluate every shape against the same specimen facts (for 'g1').
• Clear ARC sections: Answer / Reason why / Check (harness).
• Per-shape traces (✓/✗) collected, then printed in a tidy way.
• A small harness that re-checks the math and shows how P changes if you add facts.

Three shapes are implemented as examples (Chalice, Neck-amphora,
Aryballos).  To add the rest, copy each rule list from the N3 into
`ShapeRules` exactly like the examples.

To add more shapes
------------------
Copy each rule list from the N3 into `make_shape_rules(...)` exactly like the examples.
"""

from __future__ import annotations
from dataclasses import dataclass
from typing import Callable, Dict, List, Tuple, Set, Optional

# ─────────────────────────────────────────────────────────────
# 0) Fact base for ONE specimen (g1)
# ─────────────────────────────────────────────────────────────
Facts = Set[Tuple[str, str, str]]

BASE_FACTS: Facts = {
    ("g1", "Body",   "small"),
    ("g1", "Body",   "deep"),
    ("g1", "Handle", "vertical"),
    ("g1", "Handle", "horizontal"),
    ("g1", "Height", "100"),   # mm (stringified integer, like your code)
}

# ─────────────────────────────────────────────────────────────
# 1) Rules model
# ─────────────────────────────────────────────────────────────
@dataclass
class Rule:
    label: str
    weight: float
    cond: Callable[[], bool]

@dataclass
class ShapeEval:
    name: str
    total_weight: float
    fired_weight: float
    prob: float
    checks: List[Tuple[str, float, bool]]  # (label, weight, ok?)

# ─────────────────────────────────────────────────────────────
# 2) Utilities bound to a specific fact base
# ─────────────────────────────────────────────────────────────
def make_predicates(facts: Facts):
    def has(s: str, p: str, o: str) -> bool:
        return (s, p, o) in facts

    def height_values() -> List[int]:
        vals: List[int] = []
        for s, p, o in facts:
            if s == "g1" and p == "Height":
                try:
                    vals.append(int(o))
                except Exception:
                    pass
        return vals

    # Existential semantics: true if ANY recorded height satisfies the condition
    def height_between(lo: int, hi: int) -> bool:
        return any(lo <= h <= hi for h in height_values())

    def height_gt(th: int) -> bool:
        return any(h > th for h in height_values())

    return has, height_between, height_gt

# ─────────────────────────────────────────────────────────────
# 3) Shape rule table (same labels/weights/logic as before)
# ─────────────────────────────────────────────────────────────
def make_shape_rules(facts: Facts) -> Dict[str, List[Rule]]:
    has, height_between, height_gt = make_predicates(facts)

    return {
        "Chalice": [
            Rule("body bowl",          100, lambda: has("g1", "Body", "bowl")),
            Rule("body deep",          100, lambda: has("g1", "Body", "deep")),
            Rule("handle horizontal",  100, lambda: has("g1", "Handle", "horizontal")),
            Rule("handle up",          100, lambda: has("g1", "Handle", "extended_upward")),
            Rule("foot flaring",       100, lambda: has("g1", "Foot", "flaring")),
            Rule("height 150–200",     100, lambda: height_between(150, 200)),
        ],

        "Neck_amphora": [
            Rule("body oval",          100, lambda: has("g1", "Body", "oval")),
            Rule("mouth thick",        100, lambda: has("g1", "Mouth", "thick")),
            Rule("handle vertical",    100, lambda: has("g1", "Handle", "vertical")),
            Rule("neck offset",        100, lambda: has("g1", "Neck", "offset")),
            Rule("foot heavy",         100, lambda: has("g1", "Foot", "heavy")),
            Rule("height >200",        100, lambda: height_gt(200)),
        ],

        "Aryballos": [
            Rule("body round",         200, lambda: has("g1", "Body", "round")),
            Rule("mouth disk",         100, lambda: has("g1", "Mouth", "disk")),
            Rule("handle present",      50, lambda: any(has("g1","Handle",x)
                                                        for x in ("vertical","horizontal","extended_upward"))),
            Rule("height 50–100",      100, lambda: height_between(50, 100)),
        ],
    }

# ─────────────────────────────────────────────────────────────
# 4) Evaluator
# ─────────────────────────────────────────────────────────────
def evaluate_shape(name: str, rules: List[Rule]) -> ShapeEval:
    checks: List[Tuple[str, float, bool]] = []
    fired = 0.0
    total = sum(r.weight for r in rules)
    for r in rules:
        ok = r.cond()
        if ok:
            fired += r.weight
        checks.append((r.label, r.weight, ok))
    prob = (fired / total) if total > 0 else 0.0
    return ShapeEval(name=name, total_weight=total, fired_weight=fired, prob=prob, checks=checks)

def evaluate_all(facts: Facts) -> List[ShapeEval]:
    table = make_shape_rules(facts)
    return [evaluate_shape(name, rules) for name, rules in table.items()]

# ─────────────────────────────────────────────────────────────
# 5) ARC — Answer
# ─────────────────────────────────────────────────────────────
def arc_answer(facts: Facts, evals: List[ShapeEval]) -> None:
    print("Answer")
    print("------")
    print("Facts for specimen g1:")
    for s, p, o in sorted(facts):
        print(f"  ({s}, {p}, {o})")
    print()

    for ev in sorted(evals, key=lambda e: -e.prob):
        print(f"Shape: {ev.name}")
        for label, wt, ok in ev.checks:
            mark = "✓" if ok else "✗"
            print(f"  {mark} {label:<30} w={wt}")
        print(f"  → fired {ev.fired_weight:.0f} / {ev.total_weight:.0f} = P={ev.prob:.3f}")
        print()

    print("Posterior probabilities (sorted):")
    for ev in sorted(evals, key=lambda e: -e.prob):
        print(f"  {ev.name:<12} : {ev.prob:.3f}")
    print()

# ─────────────────────────────────────────────────────────────
# 6) ARC — Reason why
# ─────────────────────────────────────────────────────────────
def arc_reason() -> None:
    print("Reason why")
    print("----------")
    print("Each shape has a set of weighted rules Rᵢ with weights wᵢ.")
    print("We evaluate Rᵢ against the specimen facts; the fired weight is Σ wᵢ over satisfied rules.")
    print("The inductivity score is P = (fired weight) / (total weight for the shape).")
    print("Height predicates are existential over all Height facts to ensure monotonicity under added evidence.")
    print()

# ─────────────────────────────────────────────────────────────
# 7) ARC — Check (harness)
# ─────────────────────────────────────────────────────────────
def arc_check():
    """
    Verifications:
      1) Numbers: P = fired / total for each shape; totals are positive.
      2) Expected base values under BASE_FACTS:
            Chalice:      fired=200 / 600 → 0.333
            Neck_amphora: fired=100 / 600 → 0.167
            Aryballos:    fired=150 / 450 → 0.333
      3) Adding supportive facts for Chalice cannot *decrease* any fired weight,
         and strictly increases P for Chalice.
    """
    eps = 1e-9

    # (1) & (2) Base run
    base_evals = evaluate_all(BASE_FACTS)
    byname = {e.name: e for e in base_evals}

    for e in base_evals:
        assert e.total_weight > 0, f"{e.name}: total weight must be > 0"
        assert abs(e.prob - (e.fired_weight / e.total_weight)) < eps, f"{e.name}: P mismatch"

    assert abs(byname["Chalice"].prob      - (200/600)) < 1e-6
    assert abs(byname["Neck_amphora"].prob - (100/600)) < 1e-6
    assert abs(byname["Aryballos"].prob    - (150/450)) < 1e-6

    # (3) Add supportive facts for Chalice
    richer: Facts = set(BASE_FACTS)
    richer.update({
        ("g1", "Body", "bowl"),
        ("g1", "Handle", "extended_upward"),
        ("g1", "Foot", "flaring"),
        ("g1", "Height", "180"),   # extra measurement; existential checks keep earlier matches valid
    })
    richer_evals = evaluate_all(richer)
    r_byname = {e.name: e for e in richer_evals}

    # Fired weight should not drop for any shape when only *adding* facts
    for name in byname:
        assert r_byname[name].fired_weight >= byname[name].fired_weight, f"{name}: fired decreased unexpectedly"

    # Chalice should get a strictly higher P with the supportive facts
    assert r_byname["Chalice"].prob > byname["Chalice"].prob, "Chalice did not increase with supportive facts"

    print("OK: arithmetic correct; expected base P’s verified; supportive facts increase P as expected.")

# ─────────────────────────────────────────────────────────────
# 8) Main
# ─────────────────────────────────────────────────────────────
def main():
    evals = evaluate_all(BASE_FACTS)

    # ----- ARC output -----
    arc_answer(BASE_FACTS, evals)
    arc_reason()

    print("Check (harness)")
    print("---------------")
    try:
        arc_check()
        print("All checks passed.")
    except AssertionError as e:
        print("FAILED:", e)
        raise

if __name__ == "__main__":
    main()

