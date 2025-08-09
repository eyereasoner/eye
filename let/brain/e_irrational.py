# ============================================
# Why e is irrational — the "reason why" + proof harness
# ============================================
# This script prints a clean, self-contained proof that e is irrational,
# and silently runs a proof harness that checks the crucial ingredients:
#  • factorial divisibility for the partial sum part,
#  • a sharp upper bound for the scaled tail N! * R_N,
#  • the per-term comparison that makes the bound strict.
#
# No imports. No user input. Educational comments throughout.
#
# -------------------------------------------------------
# Core idea in one line:
# -------------------------------------------------------
# If e were a rational number a/b, then multiplying e by b! would be an integer.
# But when we expand e as a series and multiply by b!, an integer part remains PLUS
# a small positive remainder strictly between 0 and 1 — which would also have to be
# an integer. Contradiction. Hence e is irrational.
#
# -------------------------------------------------------
# Ingredients we use (all elementary):
# -------------------------------------------------------
# 1) The power series for e:
#       e = sum_{n=0}^{∞} 1/n! = 1 + 1 + 1/2! + 1/3! + ...
#
# 2) Split into partial sum + tail at N:
#       S_N = sum_{n=0}^{N} 1/n!         (finite)
#       R_N = sum_{n=N+1}^{∞} 1/n!       (positive tail)
#
# 3) Multiply by N!:
#       N! * e = N! * S_N + N! * R_N
#    For 0 ≤ n ≤ N, the quantity N!/n! is an integer (n! | N!), so N!*S_N is an integer.
#
# 4) Bound the scaled tail N!*R_N:
#       N!*R_N = sum_{n=N+1}^{∞} N!/n! = sum_{n=N+1}^{∞} 1 / [(N+1)(N+2)...n]
#    For each term, the denominator product has every factor ≥ (N+1),
#    hence term-by-term:
#       1 / [(N+1)(N+2)...(N+k)]  <  1 / (N+1)^k    for k ≥ 2,
#       and equality for k = 1.
#    Therefore the entire tail is strictly less than the geometric series
#       1/(N+1) + 1/(N+1)^2 + 1/(N+1)^3 + ... = 1/N,
#    i.e.  0 < N!*R_N < 1/N  < 1  for N ≥ 2.
#
# 5) Contradiction for a hypothetical rational e = a/b:
#    Let N = b. Then
#       N!*e = b!*e = b!*(a/b) = a*(b-1)!    (an integer).
#    But also N!*e = [integer] + (N!*R_N) with 0 < N!*R_N < 1.
#    An integer plus a strictly between-0-and-1 positive number cannot be an integer.
#    Contradiction. Therefore e is irrational.  □
#
# -------------------------------------------------------
# Minimal integer/rational utilities (no imports)
# -------------------------------------------------------

def gcd(a, b):
    """Greatest common divisor (Euclidean algorithm). Works with nonnegative ints."""
    if a < 0: a = -a
    if b < 0: b = -b
    while b:
        a, b = b, a % b
    return a

def add_frac(n1, d1, n2, d2):
    """
    Add two rationals n1/d1 + n2/d2 and return a reduced fraction (num, den).
    All args are integers; result is in lowest terms with positive denominator.
    """
    num = n1 * d2 + n2 * d1
    den = d1 * d2
    g = gcd(num, den)
    num //= g
    den //= g
    if den < 0:
        num = -num
        den = -den
    return num, den

def lt_frac(n1, d1, n2, d2):
    """Return True iff n1/d1 < n2/d2, all integer args, no overflow risk in Python."""
    return n1 * d2 < n2 * d1

def factorial(n):
    """Compute n! as an integer (n ≥ 0)."""
    f = 1
    for k in range(2, n + 1):
        f *= k
    return f

def ipow(base, exp):
    """Integer power base**exp for exp ≥ 0 (fast exponentiation not needed here)."""
    p = 1
    for _ in range(exp):
        p *= base
    return p

def product_range(a, b):
    """Product of integers from a to b inclusive; assumes a ≤ b. If a>b, returns 1."""
    if a > b:
        return 1
    p = 1
    for x in range(a, b + 1):
        p *= x
    return p

# -------------------------------------------------------
# Proof harness (silent on success)
# -------------------------------------------------------
# What we check for several N (think N=b):
#   (A) Divisibility: For all 0 ≤ n ≤ N, N!/n! is an integer  ⇔  N! % n! == 0.
#   (B) Per-term bounds: For k=1..M,
#         P_k = ∏_{j=1}^k (N+j)  and  G_k = (N+1)^k
#       Then P_1 == G_1 and for k ≥ 2 we must have P_k > G_k.
#   (C) Strict upper bound for the tail:
#         Let T_M = sum_{k=1}^M 1/P_k   (exact rational),
#             RU  = 1 / ( N * (N+1)^M ) (geometric remainder bound),
#             U_M = T_M + RU.
#       Then U_M < 1/N < 1 for N ≥ 2.
#       (Because T_M is strictly less than the first M terms of the geometric series
#        as soon as M ≥ 2, the inequality to 1/N is strict.)

def check_divisibility(N):
    Nfact = factorial(N)
    # Precompute n! increasing to avoid recompute.
    nfact = 1
    for n in range(0, N + 1):
        if n >= 2:
            nfact *= n
        # Assert exact divisibility N! / n!
        assert Nfact % nfact == 0, f"Divisibility failed: {N}! not divisible by {n}!"
    return True

def check_per_term_bounds(N, M):
    # k = 1..M
    for k in range(1, M + 1):
        Pk = product_range(N + 1, N + k)  # (N+1)(N+2)...(N+k)
        Gk = ipow(N + 1, k)               # (N+1)^k
        if k == 1:
            assert Pk == Gk, f"k=1 should be equality; got P1={Pk}, G1={Gk}"
        else:
            assert Pk > Gk, f"Strict inequality failed at k={k}: Pk={Pk} !> Gk={Gk}"
    return True

def sum_first_M_tail_terms_as_fraction(N, M):
    """
    Return T_M = sum_{k=1}^M 1 / ∏_{j=1}^k (N+j) as a reduced fraction.
    (Exact rational; no floats.)
    """
    num, den = 0, 1  # running sum
    for k in range(1, M + 1):
        Pk = product_range(N + 1, N + k)
        # Add 1/Pk to running sum
        num, den = add_frac(num, den, 1, Pk)
    return num, den

def check_upper_bound_is_below_one(N, M):
    """
    Build U_M = T_M + RU and verify U_M < 1 and U_M < 1/N (strict for M ≥ 2).
    """
    # T_M exactly:
    Tnum, Tden = sum_first_M_tail_terms_as_fraction(N, M)
    # RU = 1 / ( N * (N+1)^M )
    RU_num = 1
    RU_den = N * ipow(N + 1, M)
    # U_M = T_M + RU  (reduced)
    Unum, Uden = add_frac(Tnum, Tden, RU_num, RU_den)

    # Check U_M < 1
    assert lt_frac(Unum, Uden, 1, 1), f"U_M >= 1 for N={N}, M={M}: {Unum}/{Uden}"

    # Check U_M < 1/N (should be strict when M ≥ 2)
    assert lt_frac(Unum, Uden, 1, N), f"U_M >= 1/N for N={N}, M={M}: {Unum}/{Uden} vs 1/{N}"

    return True

def proof_harness():
    # We use modest ranges so integers stay small and the harness is fast.
    # Any N ≥ 2 suffices for the contradiction (since 1/N < 1).
    Ns = list(range(2, 13))  # N = 2..12
    M  = 6                   # use at least 2 to make the inequality strict

    for N in Ns:
        # (A) Divisibility for S_N
        check_divisibility(N)

        # (B) Per-term bounds establishing strictness against the geometric series
        check_per_term_bounds(N, M)

        # (C) Concrete rational upper bound U_M for N!*R_N that is < 1 and < 1/N
        check_upper_bound_is_below_one(N, M)

# -------------------------------------------------------
# Program output: the proof, step by step
# -------------------------------------------------------

print("============================================")
print("The irrationality of e — a clean proof")
print("============================================\n")

print("1) Start from the series definition:")
print("   e = 1 + 1 + 1/2! + 1/3! + 1/4! + ... = sum_{n=0}^∞ 1/n!\n")

print("2) Assume for contradiction that e is rational: e = a/b (a, b ∈ ℕ, b ≥ 1).")
print("   Let N = b. Split e into a partial sum S_N and a positive tail R_N where")
print("     S_N = sum_{n=0}^N 1/n!")
print("     R_N = sum_{n=N+1}^∞ 1/n!\n")

print("3) Multiply by N! (here N = b):")
print("     N! · e = N! · S_N + N! · R_N")
print("   For 0 ≤ n ≤ N, each N!/n! is an integer (n! divides N!).")
print("   Hence N! · S_N is an integer.\n")

print("4) Bound the scaled tail:")
print("     N! · R_N = sum_{n=N+1}^∞ N!/n! = sum_{k=1}^∞ 1 / ∏_{j=1}^k (N+j).")
print("   Since every factor (N+j) ≥ (N+1), we have termwise")
print("     1 / [(N+1)(N+2)...(N+k)]  <  1 / (N+1)^k  for k ≥ 2 (with equality at k=1).")
print("   Therefore the whole tail is strictly less than the geometric series")
print("     1/(N+1) + 1/(N+1)^2 + ...  =  1/N,")
print("   i.e.  0 < N!·R_N < 1/N < 1  for all N ≥ 2.\n")

print("5) Contradiction for a hypothetical rational e = a/b:")
print("   With N = b, we have  N!·e = b!·e = a·(b−1)!  (an integer).")
print("   But also  N!·e = [integer] + (N!·R_N)  with  0 < N!·R_N < 1.")
print("   An integer plus a strictly between-0-and-1 amount cannot be an integer.")
print("   Contradiction. Therefore, e is irrational.  □\n")

# -------------------------------------------------------
# Run the proof harness (silent on success)
# -------------------------------------------------------

if __name__ == "__main__":
    proof_harness()
    print("All harness checks passed (N = 2..12, M = 6):")
    print("• Divisibility of N!/n! for 0 ≤ n ≤ N")
    print("• Strict per-term bounds vs geometric series")
    print("• Concrete rational upper bound U_M < 1 and < 1/N")

