#!/usr/bin/env python3
# ============================================================
# Sum of first n odds equals n² — Explain & Check (ARC style)
# ============================================================
# You’ll see:
#   • Answer      — the claim in one line.
#   • Reason why  — a clean induction proof + geometric intuition.
#   • Check       — a fast integer harness that verifies the identity
#                   for many n by incremental summation (no imports).
# Deterministic output. Pure Python. No inputs.
# ============================================================

# -----------------------
# Proof harness (checker)
# -----------------------
def harness(MAX_N: int = 200_000) -> int:
    """
    Verify  ∑_{k=1..n} (2k−1) = n²  for n = 1..MAX_N.
    Linear-time, integers only, no libraries.
    """
    s = 0
    for n in range(1, MAX_N + 1):
        s += (2*n - 1)          # add next odd
        assert s == n*n, f"Failed at n={n}: partial_sum={s}, n^2={n*n}"
    return MAX_N

# ============
# ARC sections
# ============
print("Answer")
print("------")
print("For every n ≥ 1, the sum of the first n odd numbers equals n²:")
print("  1 + 3 + 5 + … + (2n−1) = n²")
print()

print("Reason why")
print("----------")
print("Proof by induction on n:")
print("• Base (n=1):  1 = 1².  True.")
print("• Step: assume 1+3+…+(2n−1) = n². Then")
print("    1+3+…+(2n−1)+(2(n+1)−1) = n² + (2n+1) = (n+1)².")
print("  Hence the statement holds for n+1. By induction, it holds for all n ≥ 1. □")
print()
print("Geometric intuition:")
print("  Build an n×n square of dots (n²). To get (n+1)², add an L-shaped border")
print("  of exactly 2n+1 dots — the next odd number. Repeating produces all squares.")
print()

print("Check (harness)")
print("---------------")
tested = harness()
print(f"Verified ∑(first n odds) = n² for n = 1..{tested}. ✓")

