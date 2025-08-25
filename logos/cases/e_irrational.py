#!/usr/bin/env python3
# ============================================================
# e is irrational — ARC-ified proof + deterministic harness
# ============================================================
# What you’ll see:
#   • Answer      — one-line claim.
#   • Reason why  — the classical proof in plain language.
#   • Check       — a silent, exact-arithmetic harness whose summary is printed.
#
# Constraints: no imports; everything is implemented with integers/fractions.
#
# Core idea (in one breath):
# If e were a/b, then b!·e would be an integer. But writing
#   e = S_N + R_N  with  N = b,  and multiplying by N! gives
#   N!·e = (integer) + N!·R_N  with  0 < N!·R_N < 1,
# so N!·e could not be an integer. Contradiction. Hence e is irrational. □
# ============================================================

# -------------------------
# Tiny integer/fraction kit
# -------------------------
def gcd(a: int, b: int) -> int:
    if a < 0: a = -a
    if b < 0: b = -b
    while b:
        a, b = b, a % b
    return a

def add_frac(n1: int, d1: int, n2: int, d2: int) -> tuple[int,int]:
    num = n1 * d2 + n2 * d1
    den = d1 * d2
    g = gcd(num, den)
    num //= g
    den //= g
    if den < 0:
        num = -num
        den = -den
    return num, den

def sub_frac(n1: int, d1: int, n2: int, d2: int) -> tuple[int,int]:
    return add_frac(n1, d1, -n2, d2)

def lt_frac(n1: int, d1: int, n2: int, d2: int) -> bool:
    return n1 * d2 < n2 * d1

def factorial(n: int) -> int:
    f = 1
    for k in range(2, n+1):
        f *= k
    return f

def ipow(base: int, exp: int) -> int:
    p = 1
    for _ in range(exp):
        p *= base
    return p

def product_range(a: int, b: int) -> int:
    if a > b:
        return 1
    p = 1
    for x in range(a, b+1):
        p *= x
    return p

# -------------------------------------
# Harness helpers (exact, no float req)
# -------------------------------------
def check_divisibility(N: int) -> None:
    """For all 0≤n≤N: N!/n! is an integer."""
    Nfact = factorial(N)
    nfact = 1
    for n in range(0, N+1):
        if n >= 2:
            nfact *= n
        assert Nfact % nfact == 0, f"N! not divisible by n! at n={n}"

def check_per_term_bounds(N: int, M: int) -> None:
    """(N+1)…(N+k) ≥ (N+1)^k with equality only at k=1 (strict for k≥2)."""
    for k in range(1, M+1):
        Pk = product_range(N+1, N+k)
        Gk = ipow(N+1, k)
        if k == 1:
            assert Pk == Gk, "k=1 must be equality"
        else:
            assert Pk > Gk, f"Expected strict > at k={k}"

def sum_first_M_tail_terms(N: int, M: int) -> tuple[int,int]:
    """
    T_M = Σ_{k=1..M} 1 / ∏_{j=1..k} (N+j)   (exact fraction).
    This is the first M terms of N!·R_N.
    """
    num, den = 0, 1
    for k in range(1, M+1):
        Pk = product_range(N+1, N+k)
        num, den = add_frac(num, den, 1, Pk)
    return num, den

def tail_upper_bound(N: int, M: int) -> tuple[int,int]:
    """
    U_M = T_M + 1 / (N·(N+1)^M)  (the M-term partial + geometric remainder bound).
    Returns exact fraction (num, den).
    """
    Tn, Td = sum_first_M_tail_terms(N, M)
    Rn, Rd = 1, N * ipow(N+1, M)
    return add_frac(Tn, Td, Rn, Rd)

# -------------------
# Proof/harness runner
# -------------------
def run_harness(N_lo: int = 2, N_hi: int = 12, M: int = 6) -> dict:
    """
    Verifies:
      • N!/n! divisibility for 0..N,
      • strict per-term bounds vs geometric series (k≥2),
      • U_M < 1 and U_M < 1/N.
    Returns summary stats including the smallest safety margin min(1/N - U_M).
    """
    tested = 0
    min_margin_num = None
    min_margin_den = None

    for N in range(N_lo, N_hi+1):
        check_divisibility(N)
        check_per_term_bounds(N, M)
        Un, Ud = tail_upper_bound(N, M)

        # U_M < 1 and < 1/N
        assert lt_frac(Un, Ud, 1, 1), f"U_M ≥ 1 for N={N}"
        assert lt_frac(Un, Ud, 1, N), f"U_M ≥ 1/N for N={N}"

        # Track margin δ_N = 1/N - U_M  (exact fraction)
        dn, dd = sub_frac(1, N, Un, Ud)
        # Keep the minimum margin (as a fraction)
        if min_margin_num is None:
            min_margin_num, min_margin_den = dn, dd
        else:
            # compare dn/dd < current?
            if lt_frac(dn, dd, min_margin_num, min_margin_den):
                min_margin_num, min_margin_den = dn, dd

        tested += 1

    # Convert min margin to a float just for a tiny readable stat
    min_margin_float = min_margin_num / min_margin_den
    return {
        "tested_N": tested,
        "N_range": (N_lo, N_hi),
        "M": M,
        "min_margin": (min_margin_num, min_margin_den),
        "min_margin_float": min_margin_float,
    }

# ============
# ARC sections
# ============
print("Answer")
print("------")
print("e is irrational.")
print()

print("Reason why")
print("----------")
print("Write e = S_N + R_N with N = b for a hypothetical e = a/b. Multiplying by N! gives")
print("  N!·e = N!·S_N + N!·R_N.  The first term is an integer because N!/n! is an integer")
print("for all 0≤n≤N. For the tail, each denominator factor is ≥(N+1), so")
print("  0 < N!·R_N < 1/(N+1) + 1/(N+1)^2 + … = 1/N < 1  (for N≥2).")
print("Thus N!·e equals an integer plus a strictly-between-0-and-1 positive number—")
print("impossible if N!·e were an integer. Contradiction, so e is irrational. □")
print()

print("Check (harness)")
print("---------------")
summary = run_harness(N_lo=2, N_hi=12, M=6)
lo, hi = summary["N_range"]
mmn, mmd = summary["min_margin"]
print(f"Divisibility, per-term strictness, and tail bounds verified for N={lo}..{hi} with M={summary['M']}.")
print(f"Smallest safety margin δ = min_N (1/N − U_M) = {mmn}/{mmd} ≈ {summary['min_margin_float']:.3e}")
print("All conditions hold, so the contradiction is airtight under the stated bounds. ✔")

