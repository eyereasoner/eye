#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Aristotle & Proof by Contradiction (Apagogé): Infinitely many primes
--------------------------------------------------------------------

Philosophical framing (why this is "Aristotelian"):
- In Greek mathematics (e.g. Euclid), 'proof by the impossible' (ἀπαγωγή)
  is acceptable when the impossibility follows *necessarily* from first
  principles (e.g., basic properties of divisibility).
- Here we assume there are only finitely many primes, derive an impossibility,
  and so (by the Principle of Non-Contradiction) we reject the assumption.

Mathematical content (Euclid's classic proof):
- Suppose the primes are p1, p2, ..., pk (a finite list).
- Consider the Euclid number: N := p1 * p2 * ... * pk + 1.
- For any pi in the list, pi | (product) but pi ∤ N (since N ≡ 1 mod pi).
  Therefore no listed prime divides N. So either N is itself prime (a new one),
  or N is composite but has a prime factor not in the list (still new).
- Contradiction with "we listed all primes" → There must be infinitely many.

What this program prints:
1) Answer  – the conclusion (“there are infinitely many primes”).
2) Reason  – the reductio steps in words.
3) Check   – independent harness that:
     (A) Builds Euclid numbers from initial segments of primes and verifies
         none of those primes divide the Euclid number.
     (B) Factors each Euclid number and shows at least one prime factor is new.
     (C) Sanity: counts primes ≤ N for several N to illustrate unbounded growth.

Implementation notes:
- We *constructively* exhibit new prime divisors from Euclid numbers.
- Factoring uses trial division by primes up to sqrt(n) (fine for our sizes).
"""

import math
import random

# --------------------------- Config --------------------------------------------
RANDOM_SEED = 0
DIGITS = 12

# Limit the harness to moderate sizes so factoring stays fast.
MAX_INITIAL_SEGMENT = 9   # use first 1..9 primes (through 23) for Euclid numbers

# --------------------------- Utilities -----------------------------------------
def section(title: str):
    print("\n" + "="*len(title))
    print(title)
    print("="*len(title))

def prod(it):
    """Simple product helper (exact integers)."""
    out = 1
    for x in it:
        out *= x
    return out

# --------------------------- Primes & factoring --------------------------------
def sieve_primes_upto(n: int):
    """Return list of all primes ≤ n using a simple sieve."""
    if n < 2:
        return []
    sieve = bytearray(b"\x01") * (n + 1)
    sieve[0:2] = b"\x00\x00"
    p = 2
    while p * p <= n:
        if sieve[p]:
            step = p
            start = p * p
            sieve[start:n+1:step] = b"\x00" * ((n - start)//step + 1)
        p += 1
    return [i for i, is_p in enumerate(sieve) if is_p]

def primes_first_k(k: int):
    """Return the first k primes by sieving up to a safe bound."""
    if k <= 0:
        return []
    # crude increasing bound until we collect k primes
    n = 16
    while True:
        ps = sieve_primes_upto(n)
        if len(ps) >= k:
            return ps[:k]
        n *= 2

def trial_factor(n: int):
    """
    Trial-factor n > 1 into prime factors with exponents.
    Returns list of (prime, exponent) pairs.
    """
    if n <= 1:
        return []
    factors = []
    # small primes first
    for p in [2, 3, 5]:
        if n % p == 0:
            e = 0
            while n % p == 0:
                n //= p; e += 1
            factors.append((p, e))
    # wheel over 6k ± 1 candidates
    f = 7
    step = 4  # alternates 4,2,4,2,... to generate 6k±1
    while f * f <= n:
        if n % f == 0:
            e = 0
            while n % f == 0:
                n //= f; e += 1
            factors.append((f, e))
        f += step
        step = 6 - step
    if n > 1:
        factors.append((n, 1))
    return factors

# --------------------------- Euclid construction -------------------------------
def euclid_number(primes_list):
    """
    Given a finite list of primes [p1,...,pk], return N = (p1*...*pk) + 1.
    Core property: for any pi in the list, N % pi == 1.
    """
    return prod(primes_list) + 1

def new_prime_from_euclid(primes_list):
    """
    Compute a Euclid number, factor it, and return a prime divisor not in primes_list.
    This 'exhibits' a new prime, in line with Euclid's argument.
    """
    N = euclid_number(primes_list)
    factors = trial_factor(N)
    listed = set(primes_list)
    for p, _ in factors:
        if p not in listed:
            return p, N, factors
    # Should never happen if factorization is correct:
    return None, N, factors

# --------------------------- 1) Answer -----------------------------------------
def answer():
    section("Answer")
    print("Conclusion: there are infinitely many prime numbers.")

# --------------------------- 2) Reason -----------------------------------------
def reason():
    section("Reason (Aristotelian apagogé / Euclid's reductio)")
    print(
        "Assume for contradiction the primes are finite: p1, p2, …, pk.\n"
        "Form the Euclid number N = p1·p2·…·pk + 1.\n"
        "For any listed prime pi, we have N ≡ 1 (mod pi), so pi ∤ N.\n"
        "Hence no listed prime divides N. Either:\n"
        "  • N is itself prime → a new prime; or\n"
        "  • N is composite → its prime divisors are not in the list (still new).\n"
        "Contradiction with finitude. Therefore, primes are infinite."
    )

# --------------------------- 3) Independent Check (harness) --------------------
def check():
    section("Independent Check (harness)")
    random.seed(RANDOM_SEED)
    ok = True

    # (A) For initial segments of the prime sequence, verify the Euclid property.
    base_primes = primes_first_k(MAX_INITIAL_SEGMENT)
    print(f"[INFO] Using initial primes: {base_primes}")

    for k in range(1, len(base_primes) + 1):
        segment = base_primes[:k]
        N = euclid_number(segment)
        P = prod(segment)
        # Core property: gcd(N, P) = 1 and N % p == 1 for each p in the segment
        if math.gcd(N, P) != 1:
            ok = False; print(f"[FAIL] gcd(N,P) != 1 for k={k}"); break
        remainders_ok = all((N % p) == 1 for p in segment)
        if not remainders_ok:
            ok = False; print(f"[FAIL] Some p in segment divides N for k={k}"); break
        print(f"[PASS] Euclid property holds for first {k} primes → N = product+1 = {N}")

    # (B) Exhibit a new prime factor for each segment.
    if ok:
        for k in range(1, len(base_primes) + 1):
            segment = base_primes[:k]
            new_p, N, fac = new_prime_from_euclid(segment)
            if new_p is None:
                ok = False; print(f"[FAIL] Could not find a new prime factor for N={N}")
                break
            if new_p in segment:
                ok = False; print(f"[FAIL] Found 'new' prime {new_p} already in segment")
                break
            fac_str = " * ".join([f"{p}^{e}" if e>1 else f"{p}" for p,e in fac])
            print(f"[PASS] For first {k} primes: N = {N} factors as {fac_str}; new prime: {new_p}")

    # (C) Sanity: counts of primes ≤ N keep growing with N (illustrative only).
    if ok:
        checkpoints = [10, 100, 1000, 5000, 20000]
        counts = [len(sieve_primes_upto(n)) for n in checkpoints]
        monotone = all(counts[i] < counts[i+1] for i in range(len(counts)-1))
        if monotone:
            pairs = ", ".join([f"π({n})={c}" for n, c in zip(checkpoints, counts)])
            print(f"[PASS] Prime counts grow with N: {pairs}")
        else:
            ok = False
            print("[FAIL] Non-monotone prime counts (should not happen)")

    print("\nResult:", "ALL CHECKS PASSED ✅" if ok else "Some checks FAILED ❌")
    return ok

# --------------------------- Main ----------------------------------------------
if __name__ == "__main__":
    answer()
    reason()
    check()

