#!/usr/bin/env python3
"""
snaf.py  –  scoped negation-as-failure (SNAF) toy proof

Data set
========
:g1 { :a :knows :b . }      # named graph g1
:g2 { }                     # empty graph g2

Rule
====
{ :g2 log:notIncludes { ?S :knows :b } . }
    => { :b :lonely true } .

Goal
====
:b :lonely true
"""

from itertools import count

# ──────────────────────────────
# 1. Named-graph store
# ──────────────────────────────
store = {
    ":g1": { (":a", ":knows", ":b") },
    ":g2": set(),                     # empty graph
}
# default graph (rule head lives here)
default = set()

# ──────────────────────────────
# 2. Rule
# ──────────────────────────────
rule = {
    "id": "R-lonely",
    "head": (":b", ":lonely", "true"),
    "graph": ":default",
    "body": (":g2", "log:notIncludes", ("?S", ":knows", ":b")),
}

# ──────────────────────────────
# 3. Unification helpers
# ──────────────────────────────
is_var = lambda t: isinstance(t, str) and t.startswith("?")

def unify(pat, fact, θ=None):
    """Unify pat (may contain vars) with fact; works for tuples & atoms."""
    θ = dict(θ or {})
    if isinstance(pat, tuple) != isinstance(fact, tuple):
        return None
    if not isinstance(pat, tuple):        # atoms
        if is_var(pat):
            if pat in θ and θ[pat] != fact:
                return None
            θ[pat] = fact
            return θ
        return θ if pat == fact else None
    # tuples
    if len(pat) != len(fact): return None
    for pe, fe in zip(pat, fact):
        θ = unify(pe, fe, θ)
        if θ is None: return None
    return θ

def subst(term, θ):
    """Apply substitution θ to atom or nested tuple."""
    if isinstance(term, tuple):
        return tuple(subst(t, θ) for t in term)
    return θ.get(term, term)

# ──────────────────────────────
# 4. Built-in  log:notIncludes
# ──────────────────────────────
def not_includes(graph_uri, pattern, θ):
    """
    Evaluate log:notIncludes.
    • Substitute graph_uri with θ, look that graph up in store.
    • Substitute *bound* vars inside pattern; unbound vars are left as vars
      and treated as wildcards.
    • Return True if NO triple in the graph unifies with the pattern.
    """
    g = subst(graph_uri, θ)
    if g not in store:                         # unknown graph
        return False
    pat = subst(pattern, θ)

    for triple in store[g]:
        if unify(pat, triple, {} ) is not None:
            return False                       # pattern present
    return True                                # pattern absent ⇒ satisfied

# ──────────────────────────────
# 5. Backward-chaining prover
# ──────────────────────────────
step = count(1)

def bc(goal, θ, depth):
    g = subst(goal, θ)
    print("  "*depth + f"Step {next(step):02}: prove {g}")

    # 1. Check default graph facts (none here, but code ready)
    if g in default:
        print("  "*depth + "✓ fact")
        yield θ

    # 2. Apply rule
    θh = unify(rule["head"], g, θ)
    if θh is None:
        return
    print("  "*depth + f"→ via {rule['id']}")

    graph_uri, pred, pattern = rule["body"]
    if pred == "log:notIncludes" and not_includes(graph_uri, pattern, θh):
        print("  "*(depth+1) + "✓ built-in notIncludes true")
        yield θh
    else:
        print("  "*(depth+1) + "✗ built-in notIncludes false")

# ──────────────────────────────
# 6. Run the query
# ──────────────────────────────
goal = (":b", ":lonely", "true")
print(f"\n=== Proving {goal} ===\n")
success = next(bc(goal, {}, 0), None) is not None
print("\n✔ PROVED" if success else "\n✗ NOT PROVED")

