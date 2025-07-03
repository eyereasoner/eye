#!/usr/bin/env python3
"""
fallacy_backward_full.py
Demonstrates a complete backward-chaining proof for
fallacy(ad_hominem, Argument).
"""

from itertools import count
from typing import Dict, Tuple, List

# ── facts (extracted signals) ─────────────────────────────────
facts = {
    ("Arg1", "attack", True),
    ("Arg1", "evidence", False),
    ("Arg2", "attack", False),
    ("Arg2", "evidence", True),
    ("Arg3", "attack", True),
    ("Arg3", "evidence", False),
    ("Arg4", "attack", False),
    ("Arg4", "evidence", False),
}

# ── rule set ──────────────────────────────────────────────────
rules = [
    dict(id="R-ad-hominem",
         head=("fallacy", "ad_hominem", "?A"),
         body=[
             ("?A", "attack",   True),
             ("?A", "evidence", False)
         ]),
]

# ── unification helpers ──────────────────────────────────────
is_var = lambda t: isinstance(t, str) and t.startswith("?")

def unify(pat, fact, θ=None):
    θ = dict(θ or {})
    if isinstance(pat, tuple) != isinstance(fact, tuple):
        return None
    if not isinstance(pat, tuple):
        if is_var(pat):
            if pat in θ and θ[pat] not in (pat, fact):
                return None
            θ[pat] = fact
            return θ
        return θ if pat == fact else None
    if len(pat) != len(fact):
        return None
    for a, b in zip(pat, fact):
        θ = unify(a, b, θ)
        if θ is None:
            return None
    return θ

subst = lambda term, θ: (
    tuple(subst(x, θ) for x in term)
    if isinstance(term, tuple)
    else θ.get(term, term)
)

# ── backward prover with full trace ──────────────────────────
def bc(goal: Tuple, θ: Dict, depth: int, step=count(1)):
    g = subst(goal, θ)
    indent = "  " * depth
    print(f"{indent}Step {next(step):02}: prove {g}")

    # (a) ground facts
    success = False
    for f in facts:
        θ2 = unify(g, f, θ)
        if θ2:
            print(f"{indent}✓ fact {f}")
            success = True
            yield θ2
    if success:
        return                                         # fact satisfied

    # (b) rules
    for r in rules:
        θh = unify(r["head"], g, θ)
        if θh is None:
            continue
        print(f"{indent}→ via {r['id']}")

        def prove_seq(body, idx, θcur):
            if idx == len(body):
                yield θcur
            else:
                atom = subst(body[idx], θcur)
                found = False
                for θnext in bc(atom, θcur, depth + 1, step):
                    found = True
                    yield from prove_seq(body, idx + 1, θnext)
                if not found:
                    print(f"{indent}✗ sub-goal fails: {atom}")

        yield from prove_seq(r["body"], 0, θh)

# ── run demo on four arguments ───────────────────────────────
sentences = {
    "Arg1": "Don’t listen to Smith; he’s a criminal.",
    "Arg2": "Smith’s argument is flawed because the data show X.",
    "Arg3": "You’re just too young to understand economics.",
    "Arg4": "Climate is changing primarily because of CO₂.",
}
results = {}
for aid, text in sentences.items():
    print(f"\n=== {aid}: {text}")
    goal = ("fallacy", "ad_hominem", aid)
    proved = any(bc(goal, {}, 0))
    results[aid] = proved
    print("Result:", "ad hominem\n" if proved else "no ad hominem\n")

print("Summary:")
for a in sentences:
    print(f"  {a}: {'ad hominem' if results[a] else 'ok'}")

