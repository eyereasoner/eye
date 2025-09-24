#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P3 — Self-contained, self-checking script (number theory)
Case: Quadratic Divisibility Trap (corrected)

Problem
-------
Find all positive integers n such that (2n - 1) divides (n^2 + 1).

Contract (P3 style)
-----------------------------
- Answer: the complete set of n.
- Reason Why: short, clean algebraic proof (no calculators needed).
- Check (harness): independent validations and brute-force confirmation.
"""

from typing import List, Tuple

# ---------- Core algebra (human proof, emitted as text)

def build_reason() -> str:
    """
    Proof:

    Assume (2n - 1) | (n^2 + 1). Then there exists an integer k ≥ 1 with
        n^2 + 1 = k(2n - 1).                                (1)

    View (1) as a quadratic in n:
        n^2 - 2kn + (k + 1) = 0.
    Its discriminant must be a perfect square:
        Δ = (2k)^2 - 4·1·(k+1) = 4(k^2 - k - 1) = (2s)^2     (s ∈ ℤ),
    i.e.
        k^2 - k - 1 = s^2.                                   (2)

    Complete the square around k:
        (2k - 1)^2 - 4s^2 = 5.                               (3)

    Factor (3) over ℤ:
        (2k - 1 - 2s)(2k - 1 + 2s) = 5.

    Both factors are odd integers whose product is 5, so the only possibilities are
        1·5, 5·1, (-1)·(-5), (-5)·(-1),
    which all give (up to signs) 2k - 1 = 3 and s = ±1. Hence k = 2 and s = 1.

    Returning to n via the quadratic roots of (1):
        n = k ± s = 2 ± 1  ⇒  n ∈ {1, 3}.

    Direct check:
        n=1:  2n-1 = 1 divides 1^2+1 = 2.
        n=3:  2n-1 = 5 divides 3^2+1 = 10.

    Therefore, the only positive integers n with (2n - 1) | (n^2 + 1) are n = 1 and n = 3.
    """
    return "\n".join([
        "Let n be a positive integer with (2n − 1) | (n^2 + 1). Then n^2 + 1 = k(2n − 1) for some k ≥ 1.",
        "Treat this as a quadratic in n: n^2 − 2kn + (k + 1) = 0.",
        "The discriminant must be a perfect square:",
        "  Δ = (2k)^2 − 4(k + 1) = 4(k^2 − k − 1) = (2s)^2 ⇒ k^2 − k − 1 = s^2.",
        "Completing the square: (2k − 1)^2 − 4s^2 = 5 ⇒ (2k − 1 − 2s)(2k − 1 + 2s) = 5.",
        "Odd factor pairs of 5 force 2k − 1 = 3 and s = ±1 ⇒ k = 2, s = 1.",
        "Thus n = k ± s = 2 ± 1 ⇒ n ∈ {1, 3}. Both values satisfy the divisibility.",
        "Conclusion: the only solutions are n = 1 and n = 3."
    ])

# ---------- Answer (deduced from the proof)

def solutions() -> List[int]:
    return [1, 3]

# ---------- Check (harness)

def brute_force(limit: int = 200000) -> List[int]:
    """Confirm all solutions up to 'limit' and ensure no extras appear."""
    good = []
    for n in range(1, limit + 1):
        m = 2 * n - 1
        if (n * n + 1) % m == 0:
            good.append(n)
    return good

def verify_discriminant_identity(n: int) -> Tuple[int, int, int]:
    """
    For a solution n, compute k = (n^2 + 1)/(2n − 1), then s = |n − k|.
    Returns (k, s, (2k − 1)^2 − 4s^2) which should equal 5.
    """
    m = 2 * n - 1
    k = (n * n + 1) // m
    s = abs(n - k)
    return k, s, (2 * k - 1) * (2 * k - 1) - 4 * s * s

def run_checks() -> List[str]:
    results: List[str] = []

    # 1) Direct brute-force guard
    bf = brute_force()
    assert bf == [1, 3], f"Unexpected solutions up to 200000: {bf}"
    results.append("✓ Brute-force check up to n=200000 finds exactly {1, 3}.")

    # 2) Discriminant / Pell-type identity for the solutions
    for n in solutions():
        k, s, val = verify_discriminant_identity(n)
        assert val == 5 and k == 2 and s == 1, f"Identity failed for n={n}: k={k}, s={s}, value={val}"
    results.append("✓ For n∈{1,3}: k=2, s=1 and (2k−1)^2 − 4s^2 = 5 holds.")

    # 3) Sanity: divisibility holds for the listed solutions
    for n in solutions():
        m = 2 * n - 1
        assert (n * n + 1) % m == 0, f"Divisibility fails for n={n}"
    results.append("✓ Divisibility n^2+1 ≡ 0 (mod 2n−1) verified for n∈{1,3}.")

    return results

# ---------- Presentation

def print_header(title: str) -> None:
    print("=" * 72)
    print(title)
    print("=" * 72)

def main() -> None:
    # --- Answer
    print_header("ANSWER")
    print(f"All positive integers n with (2n − 1) | (n^2 + 1): {solutions()}")

    # --- Reason Why
    print_header("REASON WHY")
    print(build_reason())

    # --- Check (harness)
    print_header("CHECK (HARNESS)")
    try:
        for line in run_checks():
            print(line)
        print("All checks PASSED ✅")
    except AssertionError as e:
        print(f"Check FAILED ❌: {e}")
        raise

if __name__ == "__main__":
    main()

