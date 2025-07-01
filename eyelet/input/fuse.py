"""
fuse_backward.py  –  backward-chaining demo for
https://github.com/eyereasoner/eye/blob/master/reasoning/fuse/fuse.n3

Data
----
:stone :color :black .
:stone :color :white .

Rule
----
{ ?X :color :black .
  ?X :color :white . } => false .

Goal: false
"""
from itertools import count

# ── data ─────────────────────────────────────────────────────────
facts = {
    (":stone", ":color", ":black"),
    (":stone", ":color", ":white"),
}

rule = {
    "id": "R-fuse",
    "head": ("false",),                     # 1-tuple head
    "body": [
        ("?X", ":color", ":black"),
        ("?X", ":color", ":white"),
    ],
}

# ── helpers ─────────────────────────────────────────────────────
is_var = lambda t: isinstance(t, str) and t.startswith("?")

def unify(pat, fact, θ=None):
    θ = dict(θ or {})
    for p, f in zip(pat, fact):
        if is_var(p):
            if p in θ and θ[p] != f:
                return None
            θ[p] = f
        elif p != f:
            return None
    return θ

def subst(term, θ):
    if isinstance(term, tuple):
        return tuple(subst(x, θ) for x in term)
    return θ.get(term, term)

# ── backward prover with trace ──────────────────────────────────
step = count(1)

def bc(goal, θ, depth):
    g = subst(goal, θ)
    print("  " * depth + f"Step {next(step):02}: prove {g}")

    # 1. ground fact
    for f in sorted(facts):
        θ2 = unify(g, f, θ)
        if θ2 is not None:
            print("  " * depth + f"✓ fact {f}")
            yield θ2                         # keep searching for alt proofs

    # 2. rule
    θh = unify(rule["head"], g, θ)
    if θh is not None:
        print("  " * depth + f"→ via {rule['id']}")
        def prove_body(i, θcur):
            if i == len(rule["body"]):
                yield θcur
            else:
                for θnext in bc(rule["body"][i], θcur, depth + 1):
                    yield from prove_body(i + 1, θnext)
        yield from prove_body(0, θh)

# ── run query “false” ───────────────────────────────────────────
print("\n=== Proving false ===")
gen = bc(("false",), {}, 0)
proved = next(gen, None) is not None
print("\n✔ PROVED false" if proved else "\n✗ NOT PROVED")

