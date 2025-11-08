#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P3 — Self-contained, self-checking script (number theory)
Case: Square-Sum Divisibility

Problem
-------
Find all positive integers n such that
    n | 1^2 + 2^2 + ... + n^2.

Contract (P3 style)
-----------------------------
- Answer: complete description of all n.
- Reason Why: short algebraic proof using the closed form for 1^2+...+n^2.
- Check (harness): independent validations + brute-force confirmation.
"""

from typing import List

# ---------- Core algebra (human proof, emitted as text)

def build_reason() -> str:
    """
    Proof (clean algebra, no calculators):

    The well-known identity
        1^2 + 2^2 + ... + n^2 = n(n+1)(2n+1)/6
    transforms the divisibility condition n | sum into:
        n | n(n+1)(2n+1)/6  ⟺  (n+1)(2n+1)/6 ∈ ℤ.
    Thus we need 6 | (n+1)(2n+1).

    Break 6 into 2 and 3:

    • Mod 2:
      2n+1 is always odd, so (n+1)(2n+1) is even iff n+1 is even, i.e. iff n is odd.

    • Mod 3:
      Reduce n modulo 3. If n ≡ 0, then n+1 ≡ 1 and 2n+1 ≡ 1, so the product ≡ 1 (mod 3).
      If n ≡ 1, then 2n+1 ≡ 0 (mod 3). If n ≡ 2, then n+1 ≡ 0 (mod 3).
      So 3 | (n+1)(2n+1) iff n ≢ 0 (mod 3).

    Combine the two conditions:
      n must be odd and not divisible by 3  ⟺  n ≡ 1 or 5 (mod 6).

    Sufficiency is immediate:
      • If n ≡ 1 (mod 6): then n+1 ≡ 2 (even) and 2n+1 ≡ 3 (≡ 0 mod 3) ⇒ product divisible by 6.
      • If n ≡ 5 (mod 6): then n+1 ≡ 0 (mod 6) ⇒ product divisible by 6.

    Therefore,
        n | 1^2+...+n^2   ⇔   n ≡ 1 or 5 (mod 6).
    """
    return "\n".join([
        "Use 1^2+...+n^2 = n(n+1)(2n+1)/6.",
        "Dividing by n reduces the condition to (n+1)(2n+1)/6 ∈ ℤ ⇔ 6 | (n+1)(2n+1).",
        "Parity: 2n+1 is odd, so need n+1 even ⇒ n is odd.",
        "Modulo 3: (n+1)(2n+1) ≡ 0 (mod 3) iff n ≢ 0 (mod 3).",
        "Hence n must be odd and not a multiple of 3 ⇔ n ≡ 1 or 5 (mod 6).",
        "Conversely, n ≡ 1 or 5 (mod 6) → 6 | (n+1)(2n+1) → divisibility holds.",
        "Conclusion: all such n are exactly those with n ≡ 1 or 5 (mod 6)."
    ])

# ---------- Answer (deduced from the proof)

def description() -> str:
    return "All positive integers n with n ≡ 1 or 5 (mod 6)."

def first_k_solutions(k: int = 20) -> List[int]:
    ans = []
    x = 1
    while len(ans) < k:
        if x % 6 in (1, 5):
            ans.append(x)
        x += 1
    return ans

# ---------- Helpers

def sum_squares(n: int) -> int:
    """Closed form n(n+1)(2n+1)/6."""
    return n * (n + 1) * (2 * n + 1) // 6

def is_solution(n: int) -> bool:
    """Check n | sum_{i=1}^n i^2 via the closed form."""
    return sum_squares(n) % n == 0

def predicate_from_theorem(n: int) -> bool:
    """Our characterization: n ≡ 1 or 5 (mod 6)."""
    return n % 6 in (1, 5)

# ---------- Check (harness)

def run_checks() -> List[str]:
    results: List[str] = []

    # 1) Equivalence check up to a large bound
    LIMIT = 200000
    ok = all(is_solution(n) == predicate_from_theorem(n) for n in range(1, LIMIT + 1))
    assert ok, "Characterization fails within the brute-force range."
    results.append(f"✓ Equivalence holds for all n ≤ {LIMIT} (brute-force guard).")

    # 2) Necessary conditions sanity: no even n, no multiples of 3 should pass
    assert all(not is_solution(n) for n in range(2, LIMIT + 1, 2)), "An even n erroneously passed."
    results.append("✓ No even n is a solution (parity argument confirmed).")
    assert all(not is_solution(3*m) for m in range(1, LIMIT // 3 + 1)), "A multiple of 3 passed."
    results.append("✓ No multiple of 3 is a solution (mod 3 argument confirmed).")

    # 3) Spot-check a few positives and negatives
    for n in (1, 5, 7, 11, 13, 17):
        assert is_solution(n), f"Expected solution n={n} failed."
    for n in (2, 3, 4, 6, 8, 9, 12, 15, 18):
        assert not is_solution(n), f"Expected non-solution n={n} passed."
    results.append("✓ Spot-checks match the theorem (solutions and non-solutions).")

    return results

# ---------- Presentation

def print_header(title: str) -> None:
    print("=" * 72)
    print(title)
    print("=" * 72)

def main() -> None:
    # --- Answer
    print_header("ANSWER")
    print(description())
    print("First 20 solutions:", first_k_solutions(20))

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

