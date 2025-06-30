#!/usr/bin/env python3
"""
usage_control_backward.py
Goal-directed version of usage_control_forward.py that terminates.

Key design choices
------------------
1.  PRE-COMPUTE the symmetric & transitive closures for the three
    meta-properties (`communicates_with`, `before`, `inherits_from`) and
    store the results as *facts*.  This lets us delete the generic
    R-sym / R-trans rules and removes the self-recursion that caused
    infinite traces.

2.  Classic depth-first backward chaining with a memo-table for failed
    *ground* goals, so we never revisit the same dead end twice.

3.  `bc()` always YIELDS a **dict** (possibly empty) on success.  When
    the “driver” only needs to know *whether* the query succeeded, we
    call `next(generator, None)` instead of `any(...)` so the truthiness
    of the yielded object no longer matters.
"""
from itertools import count, product

# ────────────────────────────────────────────────────────────────
# 0. Seed facts  (same as forward engine, without closures)
# ────────────────────────────────────────────────────────────────
facts = {
    # Schema
    ("inherits_from","a","TRANSITIVE_RELATION"),
    ("before","a","TRANSITIVE_RELATION"),
    ("communicates_with","a","SYMMETRIC_RELATION"),
    ("DATE_TODAY","a","DATE"),
    ("DATE_2025-12-31","a","DATE"),
    ("DATE_2030-01-01","a","DATE"),
    ("DATE_TODAY","before","DATE_2025-12-31"),
    ("DATE_2025-12-31","before","DATE_2030-01-01"),

    # Role hierarchy
    ("ADMIN","inherits_from","MANAGER"),
    ("MANAGER","inherits_from","EMPLOYEE"),

    # Permission objects
    ("PERM_PUB_ANY","permits_purpose","ANY"),
    ("PERM_PUB_ANY","valid_until","DATE_2030-01-01"),
    ("PERM_PRIV_ACC","permits_purpose","ACCOUNTING"),
    ("PERM_PRIV_ACC","valid_until","DATE_2025-12-31"),
    ("PERM_CONF_ANY","permits_purpose","ANY"),
    ("PERM_CONF_ANY","valid_until","DATE_2030-01-01"),

    # Role → permission object
    ("EMPLOYEE","has_permission_obj","PERM_PUB_ANY"),
    ("MANAGER","has_permission_obj","PERM_PRIV_ACC"),
    ("ADMIN","has_permission_obj","PERM_CONF_ANY"),

    # Users
    ("Alice","has_role","MANAGER"),
    ("Bob","has_role","EMPLOYEE"),
    ("Charlie","has_role","ADMIN"),

    # Resources
    ("public_doc","requires_permission","PERM_PUB_ANY"),
    ("public_doc","usage_purpose","ANY"),
    ("accounting_doc","requires_permission","PERM_PRIV_ACC"),
    ("accounting_doc","usage_purpose","ACCOUNTING"),
    ("secret_doc","requires_permission","PERM_CONF_ANY"),
    ("secret_doc","usage_purpose","ANY"),

    # Social edge
    ("Alice","communicates_with","Charlie"),
}

# ────────────────────────────────────────────────────────────────
# 1.  Pre-compute symmetric / transitive closures
# ────────────────────────────────────────────────────────────────
def closure_transitive(prop):
    added = True
    while added:
        added = False
        new = set()
        for (x,p,y) in facts:
            if p != prop: continue
            for (y2,p2,z) in facts:
                if p2 == prop and y2 == y:
                    tup = (x, prop, z)
                    if tup not in facts:
                        new.add(tup)
        if new:
            facts.update(new)
            added = True

def closure_symmetric(prop):
    for (a,p,b) in list(facts):
        if p == prop:
            facts.add((b,prop,a))

closure_transitive("before")
closure_transitive("inherits_from")
closure_symmetric("communicates_with")

# ────────────────────────────────────────────────────────────────
# 2.  Domain-specific rules  (generic schema rules removed)
# ────────────────────────────────────────────────────────────────
def T(s): return tuple(s.split())

_raw_rules = [
    ("R-role-inherit",     "?u has_role ?r2",
                           ["?u has_role ?r1", "?r1 inherits_from ?r2"]),
    ("R-perm-assign",      "?u has_permission_obj ?perm",
                           ["?u has_role ?r", "?r has_permission_obj ?perm"]),
    ("R-perm-current",     "?perm is_current true",
                           ["?perm valid_until ?exp", "DATE_TODAY before ?exp"]),
    ("R-user-current-perm","?u has_current_permission ?perm",
                           ["?u has_permission_obj ?perm", "?perm is_current true"]),
    ("R-access-purpose",   "?u can_access ?res",
                           ["?u has_current_permission ?perm",
                            "?perm permits_purpose ?purpose",
                            "?res requires_permission ?perm",
                            "?res usage_purpose ?purpose"]),
    ("R-access-any",       "?u can_access ?res",
                           ["?u has_current_permission ?perm",
                            "?perm permits_purpose ANY",
                            "?res requires_permission ?perm"]),
]
rules = [dict(id=i, head=T(h), body=[T(b) for b in body]) for i,h,body in _raw_rules]

# ────────────────────────────────────────────────────────────────
# 3.  Unification helpers
# ────────────────────────────────────────────────────────────────
is_var = lambda t: isinstance(t,str) and t.startswith("?")
def unify(p, f, θ=None):
    θ = dict(θ or {})
    for a,b in zip(p,f):
        if is_var(a):
            if a in θ and θ[a]!=b: return None
            θ[a]=b
        elif a != b: return None
    return θ
def subst(triple, θ): return tuple(θ.get(t,t) for t in triple)

# ────────────────────────────────────────────────────────────────
# 4.  Backward chaining engine
# ────────────────────────────────────────────────────────────────
_counter = count(1)
failed = set()          # memoised failed goals

def bc(goal, θ, depth, seen):
    g = subst(goal, θ)
    if g in failed: return          # prune known dead-end

    num = next(_counter)
    print("  "*depth + f"Step {num:03}: prove {g}")

    # 1. Ground fact
    for f in facts:
        θ2 = unify(g, f, {})
        if θ2 is not None:
            print("  "*depth + "✓ fact")
            yield {**θ, **θ2}       # must be dict
            # keep searching for completeness

    # 2. Apply domain rules
    for r in rules:
        θh = unify(r["head"], g, {})
        if θh is None: continue
        key = (r["id"], subst(r["head"], θh))
        if key in seen: continue    # avoid loops

        print("  "*depth + f"→ via {r['id']}")
        θnew = {**θ, **θh}

        def prove_body(i, θcurr):
            if i == len(r["body"]):
                yield θcurr
            else:
                sg = subst(r["body"][i], θcurr)
                for θn in bc(sg, θcurr, depth+1, seen|{key}):
                    yield from prove_body(i+1, θn)

        yielded = False
        for θfinal in prove_body(0, θnew):
            yielded = True
            yield θfinal
        # continue with other rules
    # 3. Memoise definite failure (no yields)
    if g not in failed:
        failed.add(g)

# ────────────────────────────────────────────────────────────────
# 5.  Queries & driver
# ────────────────────────────────────────────────────────────────
queries = [
    ("Alice","can_access","accounting_doc"),
    ("Bob","can_access","accounting_doc"),
    ("Bob","can_access","public_doc"),
    ("Charlie","can_access","accounting_doc"),
    ("Charlie","communicates_with","Alice"),
]

for q in queries:
    print(f"\n=== Query {q} ===")
    gen = bc(q, {}, 0, frozenset())
    success = next(gen, None) is not None
    print("✔ PROVED\n" if success else "✗ NOT PROVED\n")

