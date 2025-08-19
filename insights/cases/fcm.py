#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
fcm.py  –  toy Fuzzy Cognitive Map proven by backward chaining (ARC-ified)
──────────────────────────────────────────────────────────────────────────────

Concepts
--------
I = Industry      (initial 1.0)
A = Awareness     (initial 0.2)
P = Pollution     (initial 0.0)
E = EnvQual       (initial 0.0)

Weighted causal arcs  (like the original N3 “description” rules)
    I  →  P     +0.9
    P  →  E     −0.8
    A  →  P     −0.6
    I  →  A     +0.4

Update rule  (one synchronous step, bounded linear activation):
    new(C) = clip( Σ_j ( old(j) * w(j→C) ),  -1, +1 )

We encode that as one built-in per concept. The goal is:
    ?- sign(EnvQual, negative).

This script prints three ARC sections:
  • Answer — the computed new concept values and the goal truth
  • Reason why — a backward-style proof trace with built-ins fired
  • Check (harness) — numeric cross-checks and proof sanity
"""

from itertools import count
from typing import Dict, Tuple, List, Iterable

# ─────────────────────────────────────────────────────────────
# 0) Initial concept values (time t = 0)
# ─────────────────────────────────────────────────────────────
init_val: Dict[str, float] = {
    "I": 1.0,    # Industry
    "A": 0.2,    # Awareness
    "P": 0.0,    # Pollution
    "E": 0.0,    # EnvQual
}

# ─────────────────────────────────────────────────────────────
# 1) Weighted causal edges  src → dst  (weight w)
# ─────────────────────────────────────────────────────────────
edges: List[Tuple[str, str, float]] = [
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
# 2) Ground facts (initial values, included for completeness)
# ─────────────────────────────────────────────────────────────
facts = {(c, "value0", v) for c, v in init_val.items()}

# ─────────────────────────────────────────────────────────────
# 3) Rules
#     R-new      compute newval/2 (via built-in)
#     R-sign-*   classify sign/3   (via built-ins gt0/lt0/eq0)
# ─────────────────────────────────────────────────────────────
rules = [
    dict(id="R-new",
         head=("?C", "newval", "?V"),
         body=[],                       # body handled by built-in
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
# 4) Unification helpers (“self-binding is unbound” safe)
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
# 5) Built-ins
# ─────────────────────────────────────────────────────────────
computed: Dict[str, float] = {}          # new values cache

def reset_computed() -> None:
    computed.clear()

def clip(x, a=-1.0, b=1.0) -> float:
    return max(a, min(b, x))

def ensure_new(c: str) -> float:
    """Recursively compute new value for concept c (one synchronous step)."""
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

def gt0(θ): return θ if θ.get("?V", 0) > 0          else None
def lt0(θ): return θ if θ.get("?V", 0) < 0          else None
def eq0(θ): return θ if abs(θ.get("?V", 0)) < 1e-9   else None

# ─────────────────────────────────────────────────────────────
# 6) Backward-chaining engine with trace
# ─────────────────────────────────────────────────────────────
step = count(1)

def bc(goal: Tuple, θ: Dict, depth: int) -> Iterable[Dict]:
    g = subst(goal, θ)
    print("  " * depth + f"Step {next(step):02}: prove {g}")

    # (a) facts
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

        # R-sign-* have exactly one body atom
        for θmid in bc(r["body"][0], θh, depth + 1):
            if r["builtin"] == "gt0":   θfin = gt0(θmid)
            elif r["builtin"] == "lt0": θfin = lt0(θmid)
            else:                       θfin = eq0(θmid)
            if θfin:
                yield θfin

# ─────────────────────────────────────────────────────────────
# 7) Pretty helpers for Answer section
# ─────────────────────────────────────────────────────────────
def explain_concept(c: str) -> str:
    """
    Return a one-line explanation:
      new(C) = clip( Σ src * w ) = value
    """
    if c not in in_edges:
        return f"new({c}) = {init_val[c]}  (no parents)"
    parts = []
    total = 0.0
    for src, w in in_edges[c]:
        val = ensure_new(src)
        parts.append(f"{val}×{w:+.1f}")
        total += val * w
    return f"new({c}) = clip({' + '.join(parts)}) = {round(clip(total), 3)}"

# ─────────────────────────────────────────────────────────────
# 8) ARC sections
# ─────────────────────────────────────────────────────────────
def arc_answer() -> None:
    print("Answer")
    print("------")
    reset_computed()
    # force all concepts to be computed
    for c in ("I","A","P","E"):
        ensure_new(c)

    print("New values after one step:")
    for c in ("I","A","P","E"):
        print(f"  {c}: {computed[c]}")

    print("\nWhy (one-line summaries):")
    for c in ("A","P","E"):   # I has no parents
        print("  " + explain_concept(c))

    # Goal verdict
    sign = "negative" if computed["E"] < 0 else ("positive" if computed["E"] > 0 else "zero")
    print(f"\nGoal  sign(E) = negative  →  {sign == 'negative'}\n")

def arc_reason() -> None:
    print("Reason why")
    print("----------")
    reset_computed()
    # Backward proof for the goal
    goal = ("sign", "E", "negative")
    _ = list(bc(goal, {}, 0))
    print()

def arc_check() -> None:
    print("Check (harness)")
    print("---------------")
    reset_computed()
    # Numeric expectations (with our edges and init):
    # I = 1.0 (no parents)
    # A = clip( I*0.4 )                  = 0.4
    # P = clip( I*0.9 + A*(-0.6) )       = 0.9 - 0.24 = 0.66
    # E = clip( P*(-0.8) )               = -0.528
    ensure_new("E")
    exp = {"I": 1.0, "A": 0.4, "P": 0.66, "E": -0.528}
    for k, v in exp.items():
        assert abs(computed[k] - v) < 1e-9, f"{k} expected {v}, got {computed[k]}"

    # Proof succeeds for negative, fails for positive/zero
    reset_computed()
    ok_neg = any(True for _ in bc(("sign","E","negative"), {}, 0))
    reset_computed()
    ok_pos = any(True for _ in bc(("sign","E","positive"), {}, 0))
    reset_computed()
    ok_zero= any(True for _ in bc(("sign","E","zero"),     {}, 0))
    assert ok_neg and not ok_pos and not ok_zero, "Sign classification mismatch"

    # Clip bounds sanity (stress a large incoming weight)
    # Temporarily test a hypothetical node X with 2.5 ⇒ clipped to +1
    assert clip(2.5) == 1.0 and clip(-3.1) == -1.0, "clip() bounds failed"

    print("OK: numeric values, proof outcomes, and activation bounds verified.\n")

# ─────────────────────────────────────────────────────────────
# 9) Main
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    arc_answer()
    arc_reason()
    arc_check()

