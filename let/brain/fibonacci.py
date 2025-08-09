# ============================================
# Explain-and-Check: Fibonacci via Fast Doubling
# ============================================
# Claim (what we compute exactly):
#   F_0, F_1, F_6, F_91, F_283, F_3674
#
# Reason why (fast doubling is correct):
#   Define Fibonacci numbers by F_0=0, F_1=1, F_{n+2}=F_{n+1}+F_n.
#
#   A standard addition identity (provable by induction) is:
#     (A)  F_{m+n} = F_m F_{n+1} + F_{m-1} F_n   for all m,n ≥ 1.
#
#   Specializing (A) gives the “doubling” identities from a single pair (F_k, F_{k+1}):
#     • Even index:
#         F_{2k}   = F_k (F_{k+1} + F_{k-1})
#                  = F_k ( 2F_{k+1} - F_k )                 [since F_{k-1} = F_{k+1} - F_k]
#     • Odd index:
#         F_{2k+1} = F_{k+1}^2 + F_k^2
#       (Use (A) with m=k+1, n=k:  F_{2k+1} = F_{k+1}F_{k+1} + F_kF_k.)
#
#   Therefore, if a recursive call returns the pair (F_k, F_{k+1}),
#   we can compute both (F_{2k}, F_{2k+1}) in O(1) arithmetic:
#       c = F_k * (2*F_{k+1} - F_k)            = F_{2k}
#       d = F_k*F_k + F_{k+1}*F_{k+1}          = F_{2k+1}
#   and then:
#       if n is even:  (F_n, F_{n+1}) = (c, d)
#       if n is odd:   (F_n, F_{n+1}) = (d, c + d)   [since F_{2k+2} = F_{2k} + F_{2k+1}]
#
#   With base (F_0, F_1) = (0, 1), a binary recursion on n’s bits computes F_n
#   in O(log n) steps using only integer arithmetic — exact, fast, and elegant.  □
#
# No imports. No user input.

# -----------------------------
# Core implementations
# -----------------------------

def fib_fast_doubling(n):
    """
    Return (F_n, F_{n+1}) using fast doubling.
    Runs in O(log n) time and O(log n) recursion depth.
    """
    if n == 0:
        return (0, 1)
    # Recurse on floor(n/2)
    (fk, fk1) = fib_fast_doubling(n >> 1)          # k = n//2, returns (F_k, F_{k+1})
    # Doubling step
    c = fk * (2*fk1 - fk)                          # F_{2k}
    d = fk*fk + fk1*fk1                            # F_{2k+1}
    if n & 1:                                      # n is odd -> 2k+1
        return (d, c + d)                          # (F_{2k+1}, F_{2k} + F_{2k+1}) = (F_n, F_{n+1})
    else:                                          # n is even -> 2k
        return (c, d)                              # (F_{2k}, F_{2k+1})

def fib_linear(n):
    """Simple O(n) iterator (for harness cross-checks)."""
    a, b = 0, 1
    for _ in range(n):
        a, b = b, a + b
    return a

# -----------------------------
# Program output: compute requested values
# -----------------------------

def digits_of(x):
    """Count decimal digits of nonnegative integer x (0 has 1 digit)."""
    if x == 0:
        return 1
    d = 0
    while x:
        x //= 10
        d += 1
    return d

indices = [0, 1, 6, 91, 283, 3674]

print("============================================")
print("Fibonacci via Fast Doubling — explain and check")
print("============================================\n")

print("Reason why (sketch):")
print("  Using addition identities, one pair (F_k, F_{k+1}) yields both F_{2k} and F_{2k+1}:")
print("    F_{2k}   = F_k (2F_{k+1} − F_k)")
print("    F_{2k+1} = F_k^2 + F_{k+1}^2")
print("  Recursing on n’s binary expansion gives O(log n) exact computation.\n")

print("Requested values (exact):\n")
for n in indices:
    fn, _ = fib_fast_doubling(n)
    print(f"F_{n} = {fn}")
    print(f"  (digits: {digits_of(fn)})\n")

# -----------------------------
# Proof harness (silent on success; prints summary)
# -----------------------------
# Checks performed:
#   1) Base cases: F_0=0, F_1=1.
#   2) Recurrence: F_{n+2} = F_{n+1} + F_n   for many n.
#   3) Addition identity (A): F_{m+n} = F_m F_{n+1} + F_{m-1} F_n   for many m,n.
#   4) Cross-check: fib_fast_doubling(n) == fib_linear(n)   for a broad range.

def harness():
    # 1) Base
    f0, f1 = fib_fast_doubling(0)
    assert f0 == 0 and f1 == 1, "Base pair (F_0, F_1) incorrect."

    # 2) Recurrence check up to N
    N = 1200
    prev, curr = 0, 1  # F_0, F_1
    for n in range(0, N):
        # Compare with fast doubling
        fdn, fdn1 = fib_fast_doubling(n)
        assert fdn == prev and fdn1 == curr, f"Mismatch at n={n}"
        # Next
        prev, curr = curr, prev + curr
    # Also spot-check the recurrence via fast doubling values
    for n in range(0, 1000):
        fn, fn1 = fib_fast_doubling(n)
        fn2, _  = fib_fast_doubling(n+2)
        assert fn + fn1 == fn2, f"Recurrence fails at n={n}"

    # 3) Addition identity (A) on a grid
    for m in range(1, 60):
        for n in range(0, 60):
            fm, fm1 = fib_fast_doubling(m)       # F_m, F_{m+1}
            fn, fn1 = fib_fast_doubling(n)       # F_n, F_{n+1}
            fm_minus_1, _ = fib_fast_doubling(m-1)
            lhs, _ = fib_fast_doubling(m + n)
            rhs = fm * fn1 + fm_minus_1 * fn
            assert lhs == rhs, f"Addition identity fails at m={m}, n={n}"

    # 4) Cross-check vs linear iterator for a wide range (keeps it fast)
    for n in range(0, 2000):
        assert fib_fast_doubling(n)[0] == fib_linear(n), f"Cross-check fails at n={n}"

    return True

if __name__ == "__main__":
    harness()
    print("Harness checks passed:")
    print("  • Base, recurrence, addition identity, and cross-check vs linear all OK. ✓")

