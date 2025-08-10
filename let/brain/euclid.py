#!/usr/bin/env python3
"""
Euclid’s Infinitude-of-Primes Demo — pure Python, no imports
============================================================

Idea
----
Given any finite list of primes, multiply them, add 1, and you obtain an
integer N that is **not divisible** by any prime in your list. Therefore N is
itself prime or has a prime factor you didn’t have before. Repeat forever:
you can never run out of new primes.

This script reproduces the classic construction, but with:
  • **Answer**: round-by-round results (candidate, factorization, new primes).
  • **Reason why**: the one-line Euclid argument.
  • **Check (harness)**: independent verifications per round.

What we build (no imports)
--------------------------
• First k primes via trial primality on odd integers.
• Deterministic Miller–Rabin + Pollard–Rho factorization (works on big ints).
• A small harness that confirms, for each round r:
    - N_r ≡ 1 (mod p) for every *previous* prime p,
    - gcd(N_r, product(previous primes)) = 1,
    - factorization is correct and contains at least one **new** prime.

Usage
-----
Edit K_START and ROUNDS below (defaults mimic your original: 5, 5) and run:
    python euclid_noimports.py
"""

# --------------------------- tiny arithmetic --------------------------------

def gcd(a: int, b: int) -> int:
    while b:
        a, b = b, a % b
    return abs(a)

def mul(a: int, b: int) -> int:
    return a * b

def prod_list(nums) -> int:
    p = 1
    for x in nums:
        p *= x
    return p

# --------------------------- Miller–Rabin primality --------------------------

def is_probable_prime(n: int) -> bool:
    if n < 2:
        return False
    small_primes = [2,3,5,7,11,13,17,19,23,29,31,37]
    for p in small_primes:
        if n == p:
            return True
        if n % p == 0:
            return n == p
    # write n-1 = d*2^s
    d = n - 1
    s = 0
    while d % 2 == 0:
        d //= 2
        s += 1
    # deterministic bases good for 64-bit; often fine far beyond in practice
    bases = [2, 325, 9375, 28178, 450775, 9780504, 1795265022]  # strong set
    for a in bases:
        if a % n == 0:
            continue
        x = pow(a, d, n)
        if x == 1 or x == n - 1:
            continue
        skip_to_next_n = False
        for _ in range(s - 1):
            x = (x * x) % n
            if x == n - 1:
                skip_to_next_n = True
                break
        if skip_to_next_n:
            continue
        return False
    return True

# --------------------------- Pollard–Rho factorization -----------------------

def _rho_f(x, c, n):
    return (x * x + c) % n

def pollard_rho(n: int) -> int:
    """Return a nontrivial factor of composite n (n is odd, not prime)."""
    if n % 2 == 0:
        return 2
    # try a few deterministic parameter pairs (x0, c)
    seeds = [(2,1), (3,1), (5,1), (2,3), (3,5), (5,7), (7,11)]
    for x0, c in seeds:
        x = x0
        y = x0
        d = 1
        while d == 1:
            x = _rho_f(x, c, n)
            y = _rho_f(_rho_f(y, c, n), c, n)
            d = gcd(abs(x - y), n)
        if d != n:
            return d
    # fall back: simple search for small factor
    f = 3
    while f * f <= n:
        if n % f == 0:
            return f
        f += 2
    return n  # give up (n is prime or hard—caller will recheck)

def factorint(n: int, out=None) -> dict:
    """Return a dict {prime: exponent}."""
    if out is None:
        out = {}
    if n == 1:
        return out
    if is_probable_prime(n):
        out[n] = out.get(n, 0) + 1
        return out
    d = pollard_rho(n)
    if d == n:
        # treat as prime if rho failed (rare with our seeds)
        out[n] = out.get(n, 0) + 1
        return out
    factorint(d, out)
    factorint(n // d, out)
    return out

# --------------------------- prime generation --------------------------------

def first_k_primes(k: int) -> list:
    """First k primes by trial on odds + MR."""
    primes = []
    n = 2
    while len(primes) < k:
        if is_probable_prime(n):
            primes.append(n)
        n = 3 if n == 2 else n + 2
    return primes

# --------------------------- Euclid rounds -----------------------------------

def euclid_rounds(k_start=5, rounds=5):
    primes = first_k_primes(k_start)
    history = []  # list of per-round dicts for the harness

    # -------------------- Answer --------------------
    print("Answer")
    print("======")
    print(f"Starting with the first {k_start} primes:\n  {primes}\n")

    for r in range(1, rounds + 1):
        P = prod_list(primes)
        candidate = P + 1
        fac = factorint(candidate)
        # status
        status_prime = (len(fac) == 1 and list(fac.items())[0][1] == 1 and list(fac.keys())[0] == candidate) or is_probable_prime(candidate)
        # new primes = prime divisors not already present (or candidate itself if prime)
        prev_set = set(primes)
        new_prs = [q for q in fac if q not in prev_set]
        if status_prime and candidate not in prev_set:
            new_prs = [candidate]
        # update
        primes.extend(new_prs)
        primes = sorted(set(primes))

        # print round
        print(f"── Round {r} ─────────────────────────────────────────────")
        print(f"candidate = product(primes_before) + 1 = {candidate}")
        print(f"status    = {'prime' if status_prime else 'composite'}")
        # pretty factor string
        fac_items = sorted(fac.items())
        fac_str = " * ".join([f"{p}^{e}" if e>1 else f"{p}" for p,e in fac_items])
        print(f"factors   = {fac}   ({fac_str or '1'})")
        if new_prs:
            print(f"new prime{'s' if len(new_prs)>1 else ''} found: {sorted(new_prs)}")
        else:
            print("no new primes? (should not happen)")
        print(f"total distinct primes so far: {len(primes)}\n")

        # store for harness
        history.append({
            "round": r,
            "P_before": P,
            "candidate": candidate,
            "factors": fac,
            "new_primes": sorted(new_prs),
            "primes_before": sorted(prev_set),
            "primes_after": primes[:],
            "status_prime": status_prime
        })

    print("Final prime list after all rounds:")
    print(primes)
    return history

# --------------------------- Reason why --------------------------------------

def print_reason():
    print("\nReason why")
    print("==========")
    print("Let p₁, …, p_k be any finite list of primes and set N = p₁⋯p_k + 1.")
    print("For each i we have N ≡ 1 (mod p_i), so none of the p_i divide N.")
    print("Hence N is itself prime or has a prime factor not among {p_i}.")
    print("Doing this repeatedly forces an endless supply of new primes.")

# --------------------------- Check (harness) ---------------------------------

def print_check(history):
    print("\nCheck (harness)")
    print("===============")
    for rec in history:
        r = rec["round"]
        P = rec["P_before"]
        N = rec["candidate"]
        fac = rec["factors"]
        prev = rec["primes_before"]
        newp = rec["new_primes"]

        # 1) Congruence and gcd checks
        cong_ok = all((N % p == 1) for p in prev)
        g = gcd(N, P)
        gcd_ok = (g == 1)

        # 2) Factorization consistency
        #    (a) product of prime powers equals N
        prod_fac = 1
        fac_prime_check = True
        for p, e in fac.items():
            prod_fac *= pow(p, e)
            if not is_probable_prime(p):
                fac_prime_check = False
        fac_ok = (prod_fac == N) and fac_prime_check

        #    (b) at least one factor is new (unless candidate itself was new)
        novelty_ok = any(p not in prev for p in fac.keys()) or (len(fac)==1 and list(fac.keys())[0]==N)

        print(f"Round {r}:")
        print(f"  N % p == 1 for all previous p ?  {cong_ok}")
        print(f"  gcd(N, product(previous)) == 1 ? {gcd_ok}  (gcd={g})")
        print(f"  factorization valid & prime factors prime ? {fac_ok}")
        print(f"  novelty (contains a not-previous prime) ?   {novelty_ok}")

# --------------------------- main -------------------------------------------

if __name__ == "__main__":
    # tweak here if you want different settings
    K_START = 5
    ROUNDS  = 5

    hist = euclid_rounds(k_start=K_START, rounds=ROUNDS)
    print_reason()
    print_check(hist)

