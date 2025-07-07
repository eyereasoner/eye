#!/usr/bin/env python3
"""
false_dilemma.py
Detects the *false dilemma* fallacy with backward-chaining proof.

Rule
    fallacy(false_dilemma,A) :-
        either_or(A),
        not alts(A).

An argument that frames “either X or Y” **and** fails to mention other
options commits the fallacy.
"""

from itertools import count
from typing import Dict, Tuple, List

# ────────────────────────────────────────────────────────────
# 1.  Facts (hand-labelled signals)
# ────────────────────────────────────────────────────────────
facts = {
    ("FD1", "either_or", True),
    ("FD1", "alts",      False),

    ("FD2", "either_or", True),
    ("FD2", "alts",      True),     # offers “mix” alternative

    ("FD3", "either_or", True),
    ("FD3", "alts",      False),

    ("FD4", "either_or", False),
    ("FD4", "alts",      True),
}

# ────────────────────────────────────────────────────────────
# 2.  Rule base
# ────────────────────────────────────────────────────────────
rules = [
    dict(id="R-fd",
         head=("fallacy", "false_dilemma", "?A"),
         body=[("?A", "either_or", True),
               ("?A", "alts",      False)]),
]

# ────────────────────────────────────────────────────────────
# 3.  Unification helpers
# ────────────────────────────────────────────────────────────
is_var = lambda t: isinstance(t,str) and t.startswith("?")

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
    for p_elem, f_elem in zip(pat, fact):
        θ = unify(p_elem, f_elem, θ)
        if θ is None:
            return None
    return θ

subst = lambda t,θ: (tuple(subst(x,θ) for x in t)
                     if isinstance(t,tuple) else θ.get(t,t))

# ────────────────────────────────────────────────────────────
# 4.  Backward-chaining engine (full trace)
# ────────────────────────────────────────────────────────────
def bc(goal:Tuple, θ:Dict, depth:int, step=count(1)) -> List[Dict]:
    g = subst(goal, θ)
    indent = "  "*depth
    print(f"{indent}Step {next(step):02}: prove {g}")

    # (a) ground fact check
    for f in facts:
        θ2 = unify(g, f, θ)
        if θ2:
            print(indent + f"✓ fact {f}")
            yield θ2
            return                        # fact satisfied

    # (b) rule application
    for r in rules:
        θh = unify(r["head"], g, θ)
        if θh is None:
            continue
        print(indent + f"→ via {r['id']}")

        def prove_body(i, θcur):
            if i == len(r["body"]):
                yield θcur
            else:
                atom = subst(r["body"][i], θcur)
                found=False
                for θn in bc(atom, θcur, depth+1, step):
                    found=True
                    yield from prove_body(i+1, θn)
                if not found:
                    print(indent + f"✗ sub-goal fails: {atom}")

        yield from prove_body(0, θh)

# ────────────────────────────────────────────────────────────
# 5.  Demo on four arguments
# ────────────────────────────────────────────────────────────
examples = {
    "FD1": "Either we raise taxes or the nation will collapse.",
    "FD2": "Either we raise taxes or cut spending — or some mix of both.",
    "FD3": "You are either with us or against us.",
    "FD4": "We could raise taxes, cut spending, or issue bonds.",
}

results={}
for aid,text in examples.items():
    print(f"\n=== {aid}: {text}")
    goal=("fallacy","false_dilemma",aid)
    proved = any(bc(goal, {}, 0))
    results[aid]=proved
    print("Result:", "false dilemma\n" if proved else "no fallacy\n")

print("Summary:")
for aid in examples:
    print(f"  {aid}: {'false dilemma' if results[aid] else 'ok'}")

