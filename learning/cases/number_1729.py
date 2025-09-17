#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
EYE Learning Artifact
=====================
Title:  "1729: the Hardy–Ramanujan (Taxicab) number—facts, proofs & checks"

Purpose
-------
Show the EYE learning pattern—Answer, Reason, Check—on a single raw datum n=1729.
The program is self-contained, deterministic, and **self-verifying**.
It computes famous properties of 1729 and proves them with executable checks.

How to run
----------
    $ python learning_1729.py

What you get
------------
1) # Answer  — machine-readable dictionary of the results.
2) # Reason  — human-friendly explanations.
3) # Check   — labeled PASS/FAIL tests with timings and a summary.

Determinism
-----------
No network, randomness, or I/O beyond printing.

Scope notes
-----------
We verify **computationally** that 1729 is the smallest number that is the sum
of two positive cubes in two different ways (Taxicab number Ta(2)), that it is a
Carmichael number, that it is sphenic, a Harshad number in base 10, and that its
base-12 representation is the palindrome 1001₁₂.
"""

from math import isqrt
import time

# -----------------------
# Data
# -----------------------
n = 1729

# -----------------------
# Logic
# -----------------------
def factorize(x: int):
    """Return prime factors in ascending order with multiplicity."""
    f = []
    d = 2
    while d * d <= x:
        while x % d == 0:
            f.append(d)
            x //= d
        d += 1 if d == 2 else 2  # try 2 then only odds
    if x > 1:
        f.append(x)
    return f

def unique_prime_factors(x: int):
    fs = factorize(x)
    uniq = []
    for p in fs:
        if p not in uniq:
            uniq.append(p)
    return uniq

def is_square_free(x: int) -> bool:
    fs = factorize(x)
    for i in range(len(fs)-1):
        if fs[i] == fs[i+1]:
            return False
    return True

def is_carmichael(x: int) -> bool:
    # Korselt's criterion: square-free, and (p-1) | (x-1) for all primes p|x
    if x < 2 or not is_square_free(x):
        return False
    for p in unique_prime_factors(x):
        if (x - 1) % (p - 1) != 0:
            return False
    return True

def is_sphenic(x: int) -> bool:
    ups = unique_prime_factors(x)
    return len(ups) == 3 and ups[0]*ups[1]*ups[2] == x

def hars(adic_sum):
    return n % adic_sum == 0

def digit_sum_base10(x: int) -> int:
    return sum(int(c) for c in str(x))

def to_base(x: int, b: int) -> str:
    if x == 0: return "0"
    digits = []
    y = x
    while y > 0:
        r = y % b
        digits.append("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"[r])
        y //= b
    return "".join(reversed(digits))

def is_palindrome(s: str) -> bool:
    return s == s[::-1]

def cube_sum_representations(x: int):
    """Return sorted list of pairs (a,b), a<=b, with a^3+b^3=x for positive integers a,b."""
    reps = []
    limit = int(round(x ** (1/3))) + 2
    cubes = [i**3 for i in range(limit+1)]
    # map cube value to i for speed
    index = {v:i for i,v in enumerate(cubes)}
    for a in range(1, limit+1):
        a3 = cubes[a]
        if a3 >= x: break
        b3 = x - a3
        if b3 in index:
            b = index[b3]
            if a <= b and b >= 1:
                reps.append((a,b))
    # unique & sorted
    return sorted(set(reps))

def is_min_taxicab_2(x: int) -> bool:
    """Return True if x has at least two distinct cube-sum reps and no smaller positive integer does."""
    reps_x = cube_sum_representations(x)
    if len(reps_x) < 2:
        return False
    # search for any m<x with >=2 reps
    # Compute all sums up to x-1 efficiently
    limit = int(round((x-1) ** (1/3))) + 2
    sums = {}
    for a in range(1, limit+1):
        for b in range(a, limit+1):
            s = a**3 + b**3
            if s >= x: break
            sums.setdefault(s, set()).add((a,b))
    return all(len(v) < 2 for v in sums.values())

# -----------------------
# Compute insights
# -----------------------
facts = {}

facts["n"] = n
facts["factors"] = factorize(n)                    # [7, 13, 19]
facts["unique_prime_factors"] = unique_prime_factors(n)  # [7,13,19]
facts["is_sphenic"] = is_sphenic(n)                # product of 3 distinct primes
facts["is_carmichael"] = is_carmichael(n)          # True by Korselt
facts["cube_sum_reps"] = cube_sum_representations(n)  # [(1,12),(9,10)]
facts["is_taxicab2_minimal"] = is_min_taxicab_2(n) # True
facts["is_harshad_base10"] = n % digit_sum_base10(n) == 0  # 1729 % 19 == 0
facts["base12"] = to_base(n, 12)                   # "1001"
facts["base12_palindrome"] = is_palindrome(facts["base12"])

# -----------------------
# Reason Why (explanations)
# -----------------------

def reason_text():
    ups = facts["unique_prime_factors"]
    cube_reps = facts["cube_sum_reps"]
    lines = []
    lines.append(f"Prime factorization: {facts['factors']} so 1729 = 7 × 13 × 19.")
    lines.append(f"Sphenic: product of three distinct primes -> {facts['is_sphenic']}.")
    lines.append("Carmichael: square-free and for p∈{7,13,19}, (p−1) divides 1728; therefore Korselt's criterion holds.")
    # cube sums
    pairs = ", ".join([f"{a}^3+{b}^3" for (a,b) in cube_reps])
    lines.append(f"Sum of two positive cubes in two distinct ways: {pairs} = {n}.")
    lines.append(f"Minimality (Taxicab number Ta(2)): verified by exhaustive search that no m<{n} has two cube-sum representations.")
    ds = digit_sum_base10(n)
    lines.append(f"Harshad in base 10: digit sum is {ds} and {n} ÷ {ds} = {n//ds}.")
    lines.append(f"Base-12 representation: {facts['base12']} (palindrome = {facts['base12_palindrome']}).")
    return "\n".join(lines)
# -----------------------
# Check harness
# -----------------------
def run_checks(verbose=True):
    checks = []
    start = time.time()

    def check(label, func):
        t0 = time.time()
        ok = False; err = ""
        try:
            ok = bool(func())
        except Exception as e:
            ok = False; err = str(e)
        t1 = time.time()
        checks.append((label, ok, err, t1 - t0))

    # 1. factorization and sphenic
    check("factorization is [7,13,19] and sphenic", lambda: factorize(1729) == [7,13,19] and is_sphenic(1729))

    # 2. Carmichael via Korselt
    def _carmichael():
        return is_carmichael(1729) and is_square_free(1729) and all((1729-1) % (p-1) == 0 for p in unique_prime_factors(1729))
    check("Carmichael (Korselt's criterion)", _carmichael)

    # 3. Two distinct cube-sum reps
    check("Two cube-sum representations", lambda: sorted(cube_sum_representations(1729)) == [(1,12),(9,10)])

    # 4. Minimality among m<1729
    check("Minimal for having ≥2 cube-sum reps (Taxicab Ta(2))", lambda: is_min_taxicab_2(1729))

    # 5. Harshad base10
    check("Harshad (base 10)", lambda: 1729 % digit_sum_base10(1729) == 0 and digit_sum_base10(1729) == 19)

    # 6. Base-12 palindrome '1001'"
    check("Base-12 representation is palindromic 1001", lambda: to_base(1729,12) == "1001" and is_palindrome(to_base(1729,12)))

    # 7. Extra sanity: unique prime factors set
    check("Unique prime factors are {7,13,19}", lambda: set(unique_prime_factors(1729)) == {7,13,19})

    total_time = time.time() - start
    if verbose:
        print("Check Report")
        print("------------")
        width = max(len(c[0]) for c in checks) + 2
        passed = 0
        for label, ok, err, dt in checks:
            status = "PASS" if ok else "FAIL"
            if ok: passed += 1
            print(f"{label.ljust(width)} {status}  ({dt:.4f}s)")
            if err and not ok:
                print("  └─ error:", err)
        print("------------")
        print(f"Summary: {passed}/{len(checks)} checks passed in {total_time:.4f}s")
    return all(ok for _,ok,_,_ in checks)

# -----------------------
# Output
# -----------------------
if __name__ == "__main__":
    print("# Answer")
    print(facts)
    print("\n# Reason Why")
    print(reason_text())
    print("\n# Check (harness)")
    ok = run_checks(verbose=True)
    if ok:
        print("All checks passed.")
    else:
        print("Some checks failed.")
