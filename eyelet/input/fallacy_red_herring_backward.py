#!/usr/bin/env python3
"""
red_herring_backward.py
Detects the *red-herring* fallacy via backward chaining.

Rule
    fallacy(red_herring,A) :-
        off_topic(A),
        not addresses_issue(A).
"""

from itertools import count
from typing import Dict, Tuple, List

# ────────────────────────────────────────────────────────────
# 1.  Signal facts (toy annotations)
# ────────────────────────────────────────────────────────────
facts = {
    ("RH1", "off_topic", True),
    ("RH1", "addresses_issue", False),

    ("RH2", "off_topic", False),
    ("RH2", "addresses_issue", True),

    ("RH3", "off_topic", True),
    ("RH3", "addresses_issue", False),

    ("RH4", "off_topic", True),
    ("RH4", "addresses_issue", True),    # tries to address issue
}

# ────────────────────────────────────────────────────────────
# 2.  Rule base
# ────────────────────────────────────────────────────────────
rules = [
    dict(id="R-red-herring",
         head=("fallacy", "red_herring", "?A"),
         body=[("?A", "off_topic", True),
               ("?A", "addresses_issue", False)]),
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
# 4.  Backward prover (full trace)
# ────────────────────────────────────────────────────────────
def bc(goal: Tuple, θ: Dict, depth: int, step=count(1)):
    g = subst(goal, θ)
    indent = "  " * depth
    print(f"{indent}Step {next(step):02}: prove {g}")

    # facts
    for f in facts:
        θ2 = unify(g, f, θ)
        if θ2:
            print(indent + f"✓ fact {f}")
            yield θ2
            return                # fact satisfied – no alternative facts

    # rules
    for r in rules:
        θh = unify(r["head"], g, θ)
        if θh is None:
            continue
        print(indent + f"→ via {r['id']}")

        def prove_seq(idx: int, θcur: Dict):
            if idx == len(r["body"]):
                yield θcur
            else:
                atom = subst(r["body"][idx], θcur)
                found = False
                for θn in bc(atom, θcur, depth + 1, step):
                    found = True
                    yield from prove_seq(idx + 1, θn)
                if not found:
                    print(indent + f"✗ sub-goal fails: {atom}")

        yield from prove_seq(0, θh)

# ────────────────────────────────────────────────────────────
# 5.  Demo arguments
# ────────────────────────────────────────────────────────────
examples = {
    "RH1": "Why worry about climate change when aliens might invade?",
    "RH2": "Your proposal to lower taxes ignores the deficit problem.",
    "RH3": "We should not ban plastic bags; look at unemployment instead.",
    "RH4": "The report is off, but let me explain exactly where it errs.",
}

results = {}
for aid, text in examples.items():
    print(f"\n=== {aid}: {text}")
    goal = ("fallacy", "red_herring", aid)
    proved = any(bc(goal, {}, 0))
    results[aid] = proved
    print("Result:", "red herring\n" if proved else "no fallacy\n")

print("Summary:")
for aid in examples:
    print(f"  {aid}: {'red herring' if results[aid] else 'ok'}")

