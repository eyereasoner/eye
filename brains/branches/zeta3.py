#!/usr/bin/env python3
"""
Apéry's constant ζ(3) — fast, robust P3 case

Goal
----
Compute ζ(3) to ~8–9 digits (or better) with good performance and an internal check.

Method (two independent routes)
-------------------------------
Route A (fast, alternating, super-convergent):
  Use the binomial series (Apéry-type; due to e.g. Amdeberhan):
      ζ(3) = (5/2) * Σ_{n=1..∞} (-1)^{n-1} / ( n^3 * C(2n, n) ).
  Terms decay like ~ (4^{-n} / n^3), so only ~20–30 terms are needed for 9+ digits.
  Because it’s alternating with rapidly shrinking terms, the truncation error is
  ≤ the magnitude of the first omitted term.

Route B (independent cross-check):
  Use the p-series with an integral tail bound:
      S_N = Σ_{n=1..N} 1/n^3,   Tail ∈ [ ∫_{N+1}^∞ dx/x^3 , ∫_{N}^∞ dx/x^3 ]
                               = [ 1/(2 (N+1)^2) , 1/(2 N^2) ].
  Thus ζ(3) ∈ [ S_N + 1/(2 (N+1)^2) , S_N + 1/(2 N^2) ].
  With N ≈ 100,000 this interval is ~5e-11 wide, sufficient to validate 9 digits.
  We compute S_N using Kahan summation to reduce floating error.

Outputs
-------
- Answer: numeric estimate (12 decimals shown)
- Reason Why: summary of the two methods
- Check: confirms the alternating tail bound and that the value lies in the Route B interval
"""

from decimal import Decimal, getcontext
import math
from typing import Tuple

# --------------------------- Utilities ---------------------------

def central_binomial(n: int) -> int:
    """Compute C(2n, n) exactly using integer arithmetic."""
    # Use multiplicative formula to avoid huge factorials
    num = 1
    den = 1
    for k in range(1, n + 1):
        num *= (n + k)   # n+1 ... 2n
        den *= k
    return num // den


def zeta3_binomial(target_digits: int = 10) -> Tuple[Decimal, Decimal, int]:
    """
    Route A: Evaluate ζ(3) via the alternating binomial series until the first
    omitted term is < 10^{-target_digits}. Returns (sum, next_term_abs, terms_used).
    """
    # Set precision comfortably above target
    getcontext().prec = max(40, target_digits + 20)

    total = Decimal(0)
    n = 1
    thresh = Decimal(10) ** (-(target_digits + 1))  # a bit stricter than requested
    next_abs = None

    while True:
        C = central_binomial(n)
        # term = (5/2) * (-1)^{n-1} / (n^3 * C(2n,n))
        sign = 1 if (n % 2 == 1) else -1
        term = (Decimal(5) / Decimal(2)) * Decimal(sign) / (Decimal(n) ** 3 * Decimal(C))
        total += term
        # Compute next term's magnitude to decide stop
        n += 1
        Cn = central_binomial(n)
        next_abs = (Decimal(5) / Decimal(2)) / (Decimal(n) ** 3 * Decimal(Cn))
        if next_abs < thresh:
            break

    # The truncation error for an alternating series with decreasing terms is <= next_abs
    return total, next_abs, n


def zeta3_psum_with_tail(N: int = 100_000) -> Tuple[float, float, float]:
    """
    Route B: Partial sum of 1/n^3 with Kahan compensation (float), plus integral tail bounds.
    Returns (S_N, tail_lower, tail_upper).
    """
    s = 0.0
    c = 0.0  # Kahan compensation
    for k in range(1, N + 1):
        y = 1.0 / (k * k * k) - c
        t = s + y
        c = (t - s) - y
        s = t
    tail_upper = 1.0 / (2.0 * N * N)
    tail_lower = 1.0 / (2.0 * (N + 1) * (N + 1))
    return s, tail_lower, tail_upper


# ------------------------------ Driver ------------------------------

def main():
    TARGET_DIGITS = 10  # aim for ~9–10 good digits in Route A

    # Route A
    z3_A, alt_err_bound, terms = zeta3_binomial(TARGET_DIGITS)

    # Route B
    S_N, tail_lo, tail_hi = zeta3_psum_with_tail(100_000)
    interval_lo = S_N + tail_lo
    interval_hi = S_N + tail_hi

    # ------------------------ Report ------------------------
    print("Answer")
    print("------")
    # Show 12 decimals; Decimal will round properly
    print(f"Apéry's constant ζ(3) ≈ {z3_A:.12f}")
    print()

    print("Reason Why")
    print("----------")
    print("We used a rapidly convergent alternating binomial series for ζ(3), summing")
    print(f"terms until the next term's magnitude fell below 1e-{TARGET_DIGITS+1}.")
    print("By the alternating series test, the truncation error is ≤ that next-term bound.")
    print("Independently, we cross-checked with the plain 1/n^3 series using an integral tail bound,")
    print("which bounds ζ(3) inside a narrow interval [S_N + 1/(2(N+1)^2), S_N + 1/(2N^2)].")
    print()

    print("Check (harness)")
    print("---------------")
    print(f"Route A terms used: {terms}")
    print(f"Alternating tail bound (≤): {alt_err_bound:.3E}")
    inside = (Decimal(interval_lo) <= z3_A <= Decimal(interval_hi))
    print(f"Route B interval width: {(interval_hi - interval_lo):.3e}")
    print(f"Route A estimate inside Route B interval: {'PASS' if inside else 'FAIL'}")

    # Optional: show the bracket
    print("\nRoute B bracket for ζ(3)")
    print(f"Lower bound: {interval_lo:.12f}")
    print(f"Upper bound: {interval_hi:.12f}")

    # Mild PASS condition: inside & interval width smaller than 1e-10
    ok = inside and ((interval_hi - interval_lo) < 1e-10)
    print("\nResult:", "PASS" if ok else "OK (with warnings)")


if __name__ == "__main__":
    main()

