#!/usr/bin/env python3
"""
non_sequitur.py
Identify the *non-sequitur* fallacy via backward chaining.

Rule
    fallacy(non_sequitur,A) :-
        irrelevant(A),
        not valid_inference(A).
"""

from itertools import count
from typing import Dict, Tuple, List

# ────────────────────────────────────────────────────────────
# 1.  Toy “signal” facts
# ────────────────────────────────────────────────────────────
facts = {
    # irrelevant, valid_inference
    ("NS1", "irrelevant",      True),
    ("NS1", "valid_inference", False),

    ("NS2", "irrelevant",      False),
    ("NS2", "valid_inference", True),

    ("NS3", "irrelevant",      True),
    ("NS3", "valid_inference", False),

    ("NS4", "irrelevant",      True),
    ("NS4", "valid_inference", False),
}

# ────────────────────────────────────────────────────────────
# 2.  Rule base
# ────────────────────────────────────────────────────────────
rules = [
    dict(id="R-ns",
         head=("fallacy", "non_sequitur", "?A"),
         body=[("?A", "irrelevant", True),
               ("?A", "valid_inference", False)]),
]

# ────────────────────────────────────────────────────────────
# 3.  Unification helpers
# ────────────────────────────────────────────────────────────
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
    for p, f in zip(pat, fact):
        θ = unify(p, f, θ)
        if θ is None:
            return None
    return θ

subst = lambda t, θ: (
    tuple(subst(x, θ) for x in t) if isinstance(t, tuple) else θ.get(t, t)
)

# ────────────────────────────────────────────────────────────
# 4.  Back-chaining prover (full trace)
# ────────────────────────────────────────────────────────────
def bc(goal: Tuple, θ: Dict, depth: int, step=count(1)):
    g = subst(goal, θ)
    indent = "  " * depth
    print(f"{indent}Step {next(step):02}: prove {g}")

    # a) ground facts
    for f in facts:
        θ2 = unify(g, f, θ)
        if θ2:
            print(indent + f"✓ fact {f}")
            yield θ2
            return                        # fact satisfied

    # b) rules
    for r in rules:
        θh = unify(r["head"], g, θ)
        if θh is None:
            continue
        print(indent + f"→ via {r['id']}")

        def prove_seq(i: int, θcur: Dict):
            if i == len(r["body"]):
                yield θcur
            else:
                sub = subst(r["body"][i], θcur)
                found = False
                for θn in bc(sub, θcur, depth + 1, step):
                    found = True
                    yield from prove_seq(i+1, θn)
                if not found:
                    print(indent + f"✗ sub-goal fails: {sub}")

        yield from prove_seq(0, θh)

# ────────────────────────────────────────────────────────────
# 5.  Demo arguments
# ────────────────────────────────────────────────────────────
examples = {
    "NS1": "If you love me, you’ll buy me this car.",
    "NS2": "All dogs are mammals; Fido is a dog; therefore Fido is a mammal.",
    "NS3": "It’s raining, so the stock market will go up.",
    "NS4": "Prices rose after the election, proving the policy worked.",
}

results = {}
for aid, text in examples.items():
    print(f"\n=== {aid}: {text}")
    goal = ("fallacy", "non_sequitur", aid)
    proved = any(bc(goal, {}, 0))
    results[aid] = proved
    print("Result:", "non sequitur\n" if proved else "no fallacy\n")

print("Summary:")
for aid in examples:
    print(f"  {aid}: {'non sequitur' if results[aid] else 'ok'}")

