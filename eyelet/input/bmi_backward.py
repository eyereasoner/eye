#!/usr/bin/env python3
"""
bmi_backward.py  –  backward-chaining proof with BMI evidence
"""

from itertools import count
from typing import Dict, Tuple, List

# ────────────────────────────────────────────────────────────
# 0. Ground facts  (only THREE items per triple!)
# ────────────────────────────────────────────────────────────
facts = {
    ("john", "weight", 92.0),
    ("john", "height", 1.83),
    ("mary", "weight", 65.0),
    ("mary", "height", 1.71),
}

# ────────────────────────────────────────────────────────────
# 1. Rules
# ────────────────────────────────────────────────────────────
rules = [
    dict(id="R-bmi",
         head=("?P", "bmi", "?B"),
         body=[("?P", "weight", "?W"),
               ("?P", "height", "?H")],
         builtin="compute_bmi"),           # run after body done

    dict(id="R-status",
         head=("status", "?P", "true"),
         body=[("?P", "bmi", "?B")],
         builtin="gte25"),                 # run after body done
]

# ────────────────────────────────────────────────────────────
# 2. Unification helpers
# ────────────────────────────────────────────────────────────
is_var = lambda t: isinstance(t, str) and t.startswith("?")

def unify(pat, fact, θ=None):
    θ = dict(θ or {})
    if isinstance(pat, tuple) != isinstance(fact, tuple):
        return None
    if not isinstance(pat, tuple):             # atom
        if is_var(pat):
            # treat self-binding (?P -> '?P') as unbound
            if pat in θ and θ[pat] not in (pat, fact):
                return None
            θ[pat] = fact
            return θ
        return θ if pat == fact else None
    if len(pat) != len(fact):
        return None
    for p, f in zip(pat, fact):
        θ = unify(p, f, θ)
        if θ is None:
            return None
    return θ

subst = lambda t, θ: (tuple(subst(x, θ) for x in t)
                      if isinstance(t, tuple) else θ.get(t, t))

# ────────────────────────────────────────────────────────────
# 3. Built-ins
# ────────────────────────────────────────────────────────────
all_bmis: List[Tuple[str, float]] = []

def compute_bmi(θ: Dict) -> Dict | None:
    if {"?P", "?W", "?H"} <= θ.keys():
        b = round(θ["?W"] / (θ["?H"] ** 2), 2)
        θ = dict(θ); θ["?B"] = b
        all_bmis.append((θ["?P"], b))
        print(f"      ↪ bmi({θ['?P']}) = {b} kg/m²")
        return θ
    return None          # vars not yet bound → fail

def gte25(θ: Dict) -> Dict | None:
    b = θ.get("?B")
    ok = b is not None and b >= 25
    print(f"      ↪ check {b} ≥ 25  {'✓' if ok else '✗'}")
    return θ if ok else None

# ────────────────────────────────────────────────────────────
# 4. Back-chaining engine
# ────────────────────────────────────────────────────────────
step = count(1)

def bc(goal: Tuple, θ: Dict, depth: int):
    g = subst(goal, θ)
    print("  "*depth + f"Step {next(step):02}: prove {g}")

    # (a) ground facts
    for f in sorted(facts, key=repr):
        θ2 = unify(g, f, θ)
        if θ2:
            print("  "*depth + f"✓ fact {f}")
            yield θ2

    # (b) rules
    for r in sorted(rules, key=lambda r: r["id"]):
        θh = unify(r["head"], g, θ)
        if θh is None:
            continue
        print("  "*depth + f"→ via {r['id']}")

        def prove_body(idx: int, θcur: Dict):
            if idx == len(r["body"]):
                # run built-in AFTER body
                if r["builtin"] == "compute_bmi":
                    θcur = compute_bmi(θcur)
                elif r["builtin"] == "gte25":
                    θcur = gte25(θcur)
                if θcur:
                    yield θcur
            else:
                atom = r["body"][idx]
                for θn in bc(atom, θcur, depth+1):
                    yield from prove_body(idx+1, θn)

        yield from prove_body(0, {**θ, **θh})

# ────────────────────────────────────────────────────────────
# 5. Run query  ?- status(P,true)
# ────────────────────────────────────────────────────────────
goal = ("status", "?P", "true")
print(f"\n=== Proving {goal} ===\n")
step = count(1)                           # reset step numbers
solutions = list(bc(goal, {}, 0))

# ────────────────────────────────────────────────────────────
# 6. Display results
# ────────────────────────────────────────────────────────────
print("\nComputed BMI values:")
for person, bmi in all_bmis:
    print(f"  {person}: {bmi} kg/m²")

print("\nOverweight individuals (BMI ≥25):")
if solutions:
    for θ in solutions:
        print(f"  {θ['?P']}  (BMI {θ['?B']} kg/m²)")
else:
    print("  none")

