#!/usr/bin/env python3
"""
Fibonacci via Fast Doubling — ARC (Answer / Reason / Check), self-contained

We compute exact values for:
  F_0, F_1, F_6, F_91, F_283, F_3674

Layout:
  • Answer — prints requested Fibonacci numbers (exact) and their digit counts.
  • Reason why — explains the fast-doubling identities from the addition law.
  • Check (harness) — verifies base cases, recurrence, addition identity, and
    cross-checks against a linear iterator over broad ranges.

No imports. Integer-only arithmetic. O(log n) time per F_n via fast doubling.
"""

# ───────────────────────────── Core: fast doubling ───────────────────────────
def fib_fast_doubling(n):
    """Return the pair (F_n, F_{n+1}) using the fast-doubling recurrences."""
    if n == 0:
        return (0, 1)
    fk, fk1 = fib_fast_doubling(n >> 1)  # k = floor(n/2)
    c = fk * (2*fk1 - fk)                # F_{2k}
    d = fk*fk + fk1*fk1                  # F_{2k+1}
    if n & 1:                             # odd: 2k+1
        return (d, c + d)                 # (F_{2k+1}, F_{2k}+F_{2k+1})
    else:                                 # even: 2k
        return (c, d)                     # (F_{2k},   F_{2k+1})

def fib_linear(n):
    """Simple O(n) iterator (used only for harness cross-checks)."""
    a, b = 0, 1
    for _ in range(n):
        a, b = b, a + b
    return a

def digits_of(x):
    """Count decimal digits of nonnegative integer x (0 has 1 digit)."""
    if x == 0:
        return 1
    d = 0
    while x:
        x //= 10
        d += 1
    return d

# ────────────────────────────────── ARC: Answer ──────────────────────────────
def print_answer():
    print("Answer")
    print("======")
    indices = [0, 1, 6, 91, 283, 3674]
    print("Requested values (exact):\n")
    for n in indices:
        fn, _ = fib_fast_doubling(n)
        print(f"F_{n} = {fn}")
        print(f"  (digits: {digits_of(fn)})\n")

# ──────────────────────────────── ARC: Reason why ────────────────────────────
def print_reason():
    print("\nReason why")
    print("==========")
    print("Define Fibonacci by F_0=0, F_1=1, and F_{n+2}=F_{n+1}+F_n.")
    print("A standard addition identity (provable by induction) is:")
    print("  (A)  F_{m+n} = F_m·F_{n+1} + F_{m-1}·F_n  for m,n ≥ 1.\n")
    print("Specializing (A) yields fast-doubling from (F_k, F_{k+1}):")
    print("  F_{2k}   = F_k (2F_{k+1} − F_k)")
    print("  F_{2k+1} = F_{k+1}^2 + F_k^2")
    print("Hence one recursive call on k=floor(n/2) lets us compute both")
    print("F_{2k} and F_{2k+1} in O(1), and then select (F_n, F_{n+1})")
    print("depending on n’s parity. With base (F_0, F_1) = (0, 1), this gives")
    print("O(log n) exact computation using only integer arithmetic.")

# ───────────────────────────── ARC: Check (harness) ──────────────────────────
def print_check():
    print("\nCheck (harness)")
    print("===============")

    ok_all = True

    # 1) Base pair
    f0, f1 = fib_fast_doubling(0)
    ok_base = (f0 == 0 and f1 == 1)
    print(f"Base pair correct? {ok_base}")
    ok_all &= ok_base

    # 2) Recurrence F_{n+2} = F_{n+1} + F_n  (broad spot-checks)
    ok_recur = True
    for n in range(0, 1000):
        fn, fn1 = fib_fast_doubling(n)
        fn2, _  = fib_fast_doubling(n + 2)
        if fn + fn1 != fn2:
            ok_recur = False
            print(f"  Recurrence failed at n={n}")
            break
    print(f"Recurrence holds on n ∈ [0,1000)? {ok_recur}")
    ok_all &= ok_recur

    # 3) Addition identity (A) over a grid
    ok_add = True
    for m in range(1, 60):
        fm, fm1 = fib_fast_doubling(m)
        fm_1, _ = fib_fast_doubling(m - 1)
        for n in range(0, 60):
            fn, fn1 = fib_fast_doubling(n)
            lhs, _ = fib_fast_doubling(m + n)
            rhs = fm * fn1 + fm_1 * fn
            if lhs != rhs:
                ok_add = False
                print(f"  (A) failed at m={m}, n={n}")
                break
        if not ok_add:
            break
    print(f"Addition identity (A) holds on m∈[1,60), n∈[0,60)? {ok_add}")
    ok_all &= ok_add

    # 4) Cross-check vs linear iterator
    ok_cross = True
    for n in range(0, 2000):
        if fib_fast_doubling(n)[0] != fib_linear(n):
            ok_cross = False
            print(f"  Cross-check failed at n={n}")
            break
    print(f"Cross-check vs linear on n ∈ [0,2000)? {ok_cross}")
    ok_all &= ok_cross

    print(f"\nAll checks passed? {ok_all}")

# ─────────────────────────────────── Main ────────────────────────────────────
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

