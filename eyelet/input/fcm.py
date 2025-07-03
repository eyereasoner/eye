#!/usr/bin/env python3
"""
fcm.py  –  toy Fuzzy Cognitive Map proven by backward chaining
────────────────────────────────────────────────────────────────────────

Concepts
--------
I   = Industry      (initial 1.0)
A   = Awareness     (initial 0.2)
P   = Pollution     (initial 0.0)
E   = EnvQual       (initial 0.0)

Weighted causal arcs  (like the original N3 “description” rules)
    I  →  P     +0.9
    P  →  E     −0.8
    A  →  P     −0.6
    I  →  A     +0.4

Update rule  (one synchronous step, bounded linear activation):
    new(C) = clip( Σ_j ( old(j) * w(j→C) ),  -1, +1 )

We encode that as one built-in per concept.  The goal is:

    ?- sign(EnvQual, negative).

The prover shows how each concept’s new value is computed, then
demonstrates that  new(E) < 0, thereby proving the goal.
"""

from itertools import count
from typing import Dict, Tuple, List

# ─────────────────────────────────────────────────────────────
# 0.  Initial concept values  (time t = 0)
# ─────────────────────────────────────────────────────────────
init_val = {
    "I": 1.0,    # Industry
    "A": 0.2,    # Awareness
    "P": 0.0,    # Pollution
    "E": 0.0,    # EnvQual
}

# ─────────────────────────────────────────────────────────────
# 1.  Weighted causal edges  src → dst  (weight w)
# ─────────────────────────────────────────────────────────────
edges = [
    ("I", "P", +0.9),
    ("P", "E", -0.8),
    ("A", "P", -0.6),
    ("I", "A", +0.4),
]

# in_edges[dst] = [(src, w), …]
in_edges: Dict[str, List[Tuple[str, float]]] = {}
for s, d, w in edges:
    in_edges.setdefault(d, []).append((s, w))

# ─────────────────────────────────────────────────────────────
# 2.  Triples for ground facts (initial values)
# ─────────────────────────────────────────────────────────────
facts = {(c, "value0", v) for c, v in init_val.items()}

# ─────────────────────────────────────────────────────────────
# 3.  Rule set
#     R-new      compute newval/2
#     R-sign-*   classify sign/3
# ─────────────────────────────────────────────────────────────
rules = [
    dict(id="R-new",
         head=("?C", "newval", "?V"),
         body=[],                       # body done entirely by built-in
         builtin="compute_new"),

    dict(id="R-sign-pos",
         head=("sign", "?C", "positive"),
         body=[("?C", "newval", "?V")],
         builtin="gt0"),

    dict(id="R-sign-neg",
         head=("sign", "?C", "negative"),
         body=[("?C", "newval", "?V")],
         builtin="lt0"),

    dict(id="R-sign-zero",
         head=("sign", "?C", "zero"),
         body=[("?C", "newval", "?V")],
         builtin="eq0"),
]

# ─────────────────────────────────────────────────────────────
# 4.  Unification with “self-binding is unbound” fix
# ─────────────────────────────────────────────────────────────
is_var = lambda t: isinstance(t, str) and t.startswith("?")

def unify(p, f, θ=None):
    θ = dict(θ or {})
    if isinstance(p, tuple) != isinstance(f, tuple):
        return None
    if not isinstance(p, tuple):
        if is_var(p):
            if p in θ and θ[p] not in (p, f):
                return None
            θ[p] = f
            return θ
        return θ if p == f else None
    if len(p) != len(f):
        return None
    for pe, fe in zip(p, f):
        θ = unify(pe, fe, θ)
        if θ is None:
            return None
    return θ

subst = lambda t, θ: tuple(subst(x, θ) for x in t) if isinstance(t, tuple) else θ.get(t, t)

# ─────────────────────────────────────────────────────────────
# 5.  Built-ins
# ─────────────────────────────────────────────────────────────
computed: Dict[str, float] = {}          # new values cache

def clip(x, a=-1.0, b=1.0): return max(a, min(b, x))

def ensure_new(c: str) -> float:
    """Recursively compute new value for concept c."""
    if c in computed:
        return computed[c]
    if c not in in_edges:                # no parents ⇒ stays the same
        computed[c] = init_val[c]
        return computed[c]
    s = 0.0
    for src, w in in_edges[c]:
        s += ensure_new(src) * w
    computed[c] = round(clip(s), 3)
    return computed[c]

def compute_new(θ):
    c = θ.get("?C")
    if c is None:
        return None
    val = ensure_new(c)
    θ = dict(θ); θ["?V"] = val
    print(f"      ↪ newval({c}) = {val}")
    return θ

def gt0(θ): return θ if θ.get("?V", 0) > 0  else None
def lt0(θ): return θ if θ.get("?V", 0) < 0  else None
def eq0(θ): return θ if abs(θ.get("?V", 0)) < 1e-9 else None

# ─────────────────────────────────────────────────────────────
# 6.  Backward-chaining engine with trace
# ─────────────────────────────────────────────────────────────
step = count(1)

def bc(goal: Tuple, θ: Dict, depth: int):
    g = subst(goal, θ)
    print("  " * depth + f"Step {next(step):02}: prove {g}")

    # (a) facts (only initial values, not useful here but kept)
    for f in sorted(facts, key=repr):
        θ2 = unify(g, f, θ)
        if θ2 is not None:
            print("  " * depth + f"✓ fact {f}")
            yield θ2

    # (b) rules
    for r in rules:
        θh = unify(r["head"], g, θ)
        if θh is None:
            continue
        print("  " * depth + f"→ via {r['id']}")

        if r["builtin"] == "compute_new":
            θb = compute_new(θh)
            if θb:
                yield θb
            continue

        # other rules have exactly one body atom
        for θmid in bc(r["body"][0], θh, depth + 1):
            if r["builtin"] == "gt0":  θfin = gt0(θmid)
            elif r["builtin"] == "lt0":θfin = lt0(θmid)
            else:                      θfin = eq0(θmid)
            if θfin:
                yield θfin

# ─────────────────────────────────────────────────────────────
# 7.  Run the goal
# ─────────────────────────────────────────────────────────────
goal = ("sign", "E", "negative")
print(f"\n=== Proving {goal} ===\n")
solutions = list(bc(goal, {}, 0))

# ─────────────────────────────────────────────────────────────
# 8.  Evidence and verdict
# ─────────────────────────────────────────────────────────────
print("\nEvidence – new concept values after one step:")
for c, v in computed.items():
    print(f"  {c}: {v}")

print("\nVerdict:")
if solutions:
    print("  EnvQual is NEGATIVE – proof succeeded.")
else:
    print("  Goal NOT provable.")

