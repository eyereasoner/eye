#!/usr/bin/env python3
"""
Even perfect numbers via Euclid–Euler (no imports)
==================================================

Goal
----
Find the first `count` even perfect numbers using only built-in Python.
For each, print a single **Explain-and-check** block that merges:
  • the reason why it’s perfect (Euclid–Euler: Mersenne primes),
  • and concrete checks: Lucas–Lehmer residue, sigma(N)=2N, and basic facts.

Background
----------
Euclid–Euler Theorem:
  An even integer N is perfect  ⇔  N = 2^(p-1) * (2^p - 1) with p prime and (2^p - 1) prime.
Lucas–Lehmer Test (LLT):
  For odd prime p, define M = 2^p - 1 and s_0 = 4, s_{k+1} = s_k^2 - 2 (mod M).
  Then M is prime  ⇔  s_{p-2} ≡ 0 (mod M). Special case p=2: M=3 is prime.

Notes
-----
• This script is self-contained.
• It uses a sieve for prime exponents p, and LLT for each candidate Mersenne.
• Printing very large perfect numbers can get long; we also show digit counts.
• No imports. Uses:
  • Sieve for prime exponents p
  • Lucas–Lehmer test for M = 2^p - 1
  • σ(N) check from factorization N = 2^(p-1) * M
  • Optional direct divisor-sum check for small N
"""

# ---------- tiny integer toolbox ----------

def primes_up_to(N: int):
    if N < 2: return []
    s = [True]*(N+1); s[0]=s[1]=False
    p = 2
    while p*p <= N:
        if s[p]:
            for k in range(p*p, N+1, p):
                s[k] = False
        p += 1
    return [i for i in range(N+1) if s[i]]

def lucas_lehmer_is_mersenne_prime(p: int):
    """Return (is_prime, last_residue) for M=2^p-1. For p=2, (True, 0)."""
    if p == 2:
        return True, 0
    M = (1 << p) - 1
    s = 4
    for _ in range(p - 2):
        s = (s*s - 2) % M
    return (s == 0), s

def sigma_from_even_perfect(p: int, M: int, N: int) -> int:
    """
    Using M prime and N = 2^(p-1)*M:
      σ(N) = σ(2^(p-1)) * σ(M) = (2^p - 1) * (1 + M) = 2^p * (2^p - 1) = 2N
    """
    return ((1 << p) - 1) * (1 + M)

def digits(n: int) -> int:
    d = 0
    while n:
        n //= 10
        d += 1
    return max(d, 1)

def sigma_direct(n: int) -> int:
    """Direct sum of divisors (O(sqrt(n))). Use only for small n."""
    if n <= 1: return n
    s = 1 + n
    i = 2
    while i * i <= n:
        if n % i == 0:
            j = n // i
            s += i
            if j != i:
                s += j
        i += 1
    return s

# ---------- generator ----------

def perfect_numbers(count: int, p_limit: int = 200_000):
    """
    Yield (idx, p, M, N, ll_res). Finds even perfect numbers
    N = 2^(p-1)*(2^p-1) with p prime and 2^p-1 prime.
    """
    idx = 0
    for p in primes_up_to(p_limit):
        if p < 2:  # skip 0,1
            continue
        is_mp, res = lucas_lehmer_is_mersenne_prime(p)
        if not is_mp:
            continue
        M = (1 << p) - 1
        N = (1 << (p - 1)) * M
        idx += 1
        yield idx, p, M, N, res
        if idx >= count:
            break

# ---------- ARC sections per number ----------

def print_answer(idx: int, p: int, M: int, N: int):
    print("Answer")
    print("------")
    print(f"{idx}: N = {N}")
    print(f"   with p = {p}, M = 2^{p} - 1 = {M}")
    print(f"   sizes: |M|_10={digits(M)} digits, |N|_10={digits(N)} digits")
    print()

def print_reason_why(p: int, M: int, N: int):
    print("Reason why")
    print("----------")
    print("Euclid–Euler theorem says an even N is perfect iff")
    print("  N = 2^(p-1) * (2^p - 1) with p prime and M = 2^p - 1 prime.")
    print(f"Here p = {p} is prime and M = 2^{p} - 1 = {M} is prime,")
    print(f"so N = 2^{p-1} * M = {N} is perfect.")
    print()

def print_check_harness(p: int, M: int, N: int, ll_res: int):
    print("Check (harness)")
    print("---------------")
    # 1) Lucas–Lehmer primality of M
    if p == 2:
        print("  Lucas–Lehmer: p=2 ⇒ M=3 is prime (base case).")
    else:
        print(f"  Lucas–Lehmer for M=2^{p}-1:")
        print(f"    s_0=4, s_(k+1)=s_k^2-2 (mod M); after p-2={p-2} steps, s_{p-2} ≡ {ll_res} (mod M).")
        print(f"    M is prime? {ll_res == 0}")
    # 2) σ(N) = 2N from factorization (exact)
    sig = sigma_from_even_perfect(p, M, N)
    print(f"  Sigma via factorization: σ(N) = (2^{p}-1)*(1+M) = {sig}  → equals 2N? {sig == 2*N}")
    # 3) Optional direct divisor-sum for small N
    if digits(N) <= 10:  # keep direct check cheap
        sig_direct = sigma_direct(N)
        print(f"  Direct divisor-sum (small N): σ(N) = {sig_direct}  → equals 2N? {sig_direct == 2*N}")
    else:
        print("  Direct divisor-sum: skipped (N is large; check above is exact).")
    # 4) Sanity facts
    print(f"  Sanity: N even? {N % 2 == 0}, M odd? {M % 2 == 1}")
    print()

# ---------- main ----------

if __name__ == '__main__':
    COUNT = 15         # like your original; computing many LL tests can be slow
    P_LIMIT = 200_000  # upper bound for prime exponents to scan

    for idx, p, M, N, res in perfect_numbers(COUNT, P_LIMIT):
        print("=" * 72)
        print_answer(idx, p, M, N)
        print_reason_why(p, M, N)
        print_check_harness(p, M, N, res)

