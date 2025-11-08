#!/usr/bin/env python3
# ============================================================
# Sum of cubes equals a square — Explain & Check (ARC style)
# ============================================================
# You’ll see:
#   • Answer      — the identity in one line.
#   • Reason why  — a clean induction proof with explicit algebra.
#   • Check       — a fast integer harness that verifies the identity
#                   for many n (no imports).
# Deterministic output. Pure Python. No inputs.
# ============================================================

# -----------------------
# Proof harness (checker)
# -----------------------
def harness(MAX_N: int = 200_000) -> int:
    """
    Verify  1^3 + 2^3 + ... + n^3 = [ n(n+1)/2 ]^2  for n = 1..MAX_N.
    Also checks the telescoping step: S(n) − S(n−1) = n^3.
    Integer arithmetic only.
    """
    sum_cubes = 0
    prev_sum = 0
    for n in range(1, MAX_N + 1):
        # Incremental left-hand side
        sum_cubes += n*n*n

        # Right-hand side via triangular number squared
        Tn = n * (n + 1) // 2
        rhs = Tn * Tn

        # Equality check
        assert sum_cubes == rhs, f"Fail at n={n}: sum={sum_cubes}, rhs={rhs}"

        # Telescoping check (from n-1 to n)
        if n > 1:
            assert sum_cubes - prev_sum == n*n*n, \
                f"Telescoping fail at n={n}: Δ={sum_cubes - prev_sum}, expected {n*n*n}"

        prev_sum = sum_cubes
    return MAX_N

# ============
# ARC sections
# ============
print("Answer")
print("------")
print("For every n ≥ 1,  1^3 + 2^3 + ··· + n^3 = [ n(n+1)/2 ]^2.")
print()

print("Reason why")
print("----------")
print("Proof by induction on n:")
print("• Base (n=1):  1^3 = 1 and [1·2/2]^2 = 1. True.")
print("• Step: assume S(n) = 1^3+…+n^3 = [n(n+1)/2]^2. Then")
print("    S(n+1) = S(n) + (n+1)^3")
print("           = [n(n+1)/2]^2 + (n+1)^3")
print("           = (n+1)^2 · ( n^2/4 + (n+1) )")
print("           = (n+1)^2 · ( (n^2 + 4n + 4)/4 )")
print("           = (n+1)^2 · ( (n+2)^2 / 4 )")
print("           = [ (n+1)(n+2)/2 ]^2.")
print("  Hence the identity holds for n+1. By induction, it holds for all n ≥ 1. □")
print()
print("Intuition:")
print("  Let T_n = 1+2+···+n = n(n+1)/2 be the nth triangular number.")
print("  The identity says ∑_{k=1}^n k^3 = (T_n)^2 — the square of a triangle of dots.")
print()

print("Check (harness)")
print("---------------")
tested = harness()
print(f"Verified ∑_{'{'}k=1..n{'}'} k^3 = [n(n+1)/2]^2 and ΔS(n)=n^3 for n = 1..{tested}. ✓")

