#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
newton.py — Newton–Raphson as a backward-style proof (ARC-ified)
────────────────────────────────────────────────────────────────────

We “prove” the goal  root(f, x*)  by exhibiting x* such that |f(x*)| < ε.

Function
  f(x)  =  x³ − 2x − 5        (has a single real root ≈ 2.09455)
Derivative
  f'(x) =  3x² − 2

Method
  Newton–Raphson:  x_{k+1} = x_k − f(x_k)/f'(x_k)
  Start at x₀ = 2, stop when |f(x_k)| < ε with ε = 1e−6 (or max 20 iters).

ARC sections
  • Answer      — certified root, residual, iteration count, short trace
  • Reason why  — why Newton works here + the exact update used
  • Check       — harness verifies goal & a few safety properties
"""

from __future__ import annotations
from dataclasses import dataclass
from typing import List, Tuple

# ─────────────────────────────────────────────────────────────
# Problem definition
# ─────────────────────────────────────────────────────────────
def f(x: float) -> float:
    return x**3 - 2*x - 5

def df(x: float) -> float:
    return 3*x**2 - 2

EPS  = 1e-6      # tolerance for |f(x*)|
X0   = 2.0       # starting guess
MAXI = 20        # iteration limit
SAFE = 1e-14     # guard against zero derivative

# ─────────────────────────────────────────────────────────────
# Newton driver that records a “proof trace”
# ─────────────────────────────────────────────────────────────
@dataclass
class Step:
    k: int
    x: float
    fx: float
    dfx: float
    x_next: float

def newton_with_trace(x0: float, eps: float = EPS, max_iter: int = MAXI) -> Tuple[float, List[Step]]:
    """Run Newton and return (root, full trace). Raises on zero derivative."""
    x = x0
    trace: List[Step] = []
    for k in range(1, max_iter + 1):
        fx = f(x)
        if abs(fx) < eps:
            return x, trace
        dfx = df(x)
        if abs(dfx) < SAFE:
            raise ZeroDivisionError(f"Derivative too small at iteration {k}: f'({x})={dfx}")
        x_next = x - fx / dfx
        trace.append(Step(k, x, fx, dfx, x_next))
        x = x_next
    # final check after loop
    if abs(f(x)) < eps:
        return x, trace
    raise RuntimeError("Maximum iterations exceeded without meeting tolerance")

# ─────────────────────────────────────────────────────────────
# ARC — Answer
# ─────────────────────────────────────────────────────────────
def arc_answer(root: float, trace: List[Step]) -> None:
    print("Answer")
    print("------")
    print(f"Goal:  root(f, x*) with |f(x*)| < ε,  ε = {EPS:g}")
    print(f"Start: x₀ = {X0}")
    print(f"Done:  x* = {root:.10f}")
    print(f"Check: |f(x*)| = {abs(f(root)):.3e}")
    print(f"Iters: {len(trace)} (≤ {MAXI})\n")

    print("Short trace (first 6 steps or all):")
    show = trace[:6] if len(trace) > 6 else trace
    for s in show:
        print(f"  Step {s.k:02}: x={s.x:.10f}  f={s.fx:+.3e}  f'={s.dfx:+.3e}  →  x₊₁={s.x_next:.10f}")
    if len(trace) > 6:
        print(f"  … ({len(trace)-6} more)")
    print()

# ─────────────────────────────────────────────────────────────
# ARC — Reason why
# ─────────────────────────────────────────────────────────────
def arc_reason(root: float, trace: List[Step]) -> None:
    print("Reason why")
    print("----------")
    print("Newton–Raphson solves f(x)=0 by repeatedly applying:")
    print("  x_{k+1} = x_k − f(x_k) / f'(x_k)")
    print("Geometric intuition: follow the tangent at x_k to its x-intercept.")
    print("For f(x)=x³−2x−5, f' is continuous and nonzero near the root; the")
    print("method converges quadratically once close enough.\n")

    if trace:
        s = trace[0]
        print("First update used:")
        print(f"  x₁ = x₀ − f(x₀)/f'(x₀) = {s.x:.10f} − ({s.fx:+.3e})/({s.dfx:+.3e}) = {s.x_next:.10f}\n")
    print("Certified because the final iterate x* satisfies |f(x*)| < ε.\n")

# ─────────────────────────────────────────────────────────────
# ARC — Check (harness)
# ─────────────────────────────────────────────────────────────
def arc_check(root: float, trace: List[Step]) -> None:
    print("Check (harness)")
    print("---------------")
    # 1) goal satisfied
    assert abs(f(root)) < EPS, "Final residual not below tolerance."
    # 2) derivative never vanished along the path we used
    for s in trace:
        assert abs(s.dfx) > SAFE, f"Near-zero derivative at step {s.k}."
    # 3) iterations bounded
    assert len(trace) <= MAXI, "Exceeded iteration cap."
    # 4) root bracket sanity: f(2) < 0 < f(3), so x* ∈ (2,3)
    assert f(2.0) < 0 < f(3.0), "Pre-check failed: function should cross between 2 and 3."
    assert 2.0 < root < 3.0, "Root not in expected bracket (2,3)."
    # 5) replay determinism from the same start/tolerance
    r2, t2 = newton_with_trace(X0, EPS, MAXI)
    assert abs(r2 - root) <= 1e-12, "Re-run produced a different root."
    print("OK: goal satisfied, safe derivatives, bounded iters, bracketed, reproducible.\n")

# ─────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    print("Proving existence of a root for  f(x) = x³ − 2x − 5\n")
    root, trace = newton_with_trace(X0, EPS, MAXI)
    arc_answer(root, trace)
    arc_reason(root, trace)
    arc_check(root, trace)

