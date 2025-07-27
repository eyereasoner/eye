#!/usr/bin/env python3
"""
newton.py
──────────────────────────────────────────────────────────────
Backward-chaining “proof” of a root obtained by Newton-Raphson.

Function
    f(x)  =  x³ − 2x − 5     (has a real root near x = 2.09455…)

Derivative
    f'(x) =  3x² − 2

Goal predicate
    root(f, x*)   holds when  |f(x*)| < ε   with ε = 1e-6.

The script:
    • starts with guess  x₀ = 2 ,
    • prints each Newton step as  “→ via Newton”  in the proof,
    • stops when the goal predicate is satisfied,
    • reports the certified root.
"""

from typing import Dict
from itertools import count

# ─────────────────────────────────────────────────────────────
# 0.  Function & derivative
# ─────────────────────────────────────────────────────────────
def f(x: float) -> float:
    return x**3 - 2*x - 5

def df(x: float) -> float:
    return 3*x**2 - 2

EPS  = 1e-6          # proof tolerance
X0   = 2.0           # starting guess
MAXI = 20            # hard limit on iterations

# ─────────────────────────────────────────────────────────────
# 1.  Backward-chaining prover
# ─────────────────────────────────────────────────────────────
step_no = count(1)

def prove_root(x: float, depth: int = 0) -> float:
    """
    Recursively prove root(f, x) by Newton backward chaining.
    Returns the certified root once |f(x)| < EPS.
    """
    err = abs(f(x))
    print("  "*depth + f"Step {next(step_no):02}: prove root at x = {x:.10f} "
          f"( |f| = {err:.3e} )")

    # Base case: predicate holds within tolerance
    if err < EPS:
        print("  "*depth + "✓ |f(x)| < ε  → goal satisfied")
        return x

    if depth >= MAXI:
        raise RuntimeError("Maximum iterations exceeded")

    # Newton update  x_new = x - f/df
    deriv = df(x)
    if deriv == 0.0:
        raise ZeroDivisionError(f"Derivative zero at x = {x}")
    x_new = x - f(x)/deriv

    print("  "*depth + f"→ via Newton  "
          f"(f = {f(x):+.3e},  f' = {deriv:+.3e})  ->  xₙ₊₁ = {x_new:.10f}")

    # Sub-goal: prove root at x_new
    return prove_root(x_new, depth + 1)

# ─────────────────────────────────────────────────────────────
# 2.  Run proof from initial guess
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    print(f"Proving existence of a root for  f(x) = x³ − 2x − 5")
    certified = prove_root(X0)

    print(f"\nCertified root:  x* = {certified:.10f}")
    print(f"Final check:     f(x*) = {f(certified):+.3e}")

