#!/usr/bin/env python3
"""
strawman.py
Backward-chaining proof for the *straw-man* fallacy.

Rule
    fallacy(strawman,A) :-
        misrepresents(A),
        refutes_misrep(A),
        not addresses_original(A).
"""

from itertools import count
from typing import Dict, Tuple, List

# ────────────────────────────────────────────────────────────
# 1.  Ground signal facts  (toy annotations)
# ────────────────────────────────────────────────────────────
facts = {
    # misrepresents, refutes, addresses_original
    ("SM1", "misrepresents",     True),
    ("SM1", "refutes_misrep",    True),
    ("SM1", "addresses_original",False),

    ("SM2", "misrepresents",     False),
    ("SM2", "refutes_misrep",    False),
    ("SM2", "addresses_original",True),

    ("SM3", "misrepresents",     True),
    ("SM3", "refutes_misrep",    True),
    ("SM3", "addresses_original",False),

    ("SM4", "misrepresents",     False),
    ("SM4", "refutes_misrep",    True),
    ("SM4", "addresses_original",True),
}

# ────────────────────────────────────────────────────────────
# 2.  Rule base
# ────────────────────────────────────────────────────────────
rules = [
    dict(id="R-strawman",
         head=("fallacy", "strawman", "?A"),
         body=[("?A", "misrepresents", True),
               ("?A", "refutes_misrep", True),
               ("?A", "addresses_original", False)]),
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
    for p, f in zip(pat, fact):
        θ = unify(p, f, θ)
        if θ is None:
            return None
    return θ

subst = lambda t,θ: (tuple(subst(x,θ) for x in t)
                     if isinstance(t,tuple) else θ.get(t,t))

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
            return                     # fact satisfied

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
                found=False
                for θn in bc(atom, θcur, depth+1, step):
                    found=True
                    yield from prove_seq(idx+1, θn)
                if not found:
                    print(indent + f"✗ sub-goal fails: {atom}")

        yield from prove_seq(0, θh)

# ────────────────────────────────────────────────────────────
# 5.  Demo corpus
# ────────────────────────────────────────────────────────────
examples = {
    "SM1": "Either we ban guns or keep mass shootings (distorts).",
    "SM2": "Your proposal includes phased tax changes (addresses).",
    "SM3": "Opponent wants to abolish the army (distorted).",
    "SM4": "Let me quote and rebut the original claim (addresses).",
}

results={}
for aid, text in examples.items():
    print(f"\n=== {aid}: {text}")
    goal=("fallacy","strawman",aid)
    proved = any(bc(goal, {}, 0))
    results[aid]=proved
    print("Result:", "straw-man\n" if proved else "no fallacy\n")

print("Summary:")
for aid in examples:
    print(f"  {aid}: {'straw-man' if results[aid] else 'ok'}")

