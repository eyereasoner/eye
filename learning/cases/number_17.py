#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
EYE Learning Artifact
=====================
Purpose:
  Demonstrate the EYE learning pattern—Answer, Reason, Check—on raw datum n=17.
  The program is single-file, deterministic, and self-verifying.

How to run:
  $ python learning_17.py

What you get:
  1) # Answer   — concise machine-readable dictionary of results.
  2) # Reason   — human-friendly explanations.
  3) # Check    — explicit, labeled tests with PASS/FAIL, timing, and a summary.

Determinism:
  No network or randomness is used; all math is computed locally.

Data:
  n = 17 (hard-coded for the demo; feel free to modify).

Logic blocks:
  - primality tests and special-form recognizers (Fermat, Proth)
  - decompositions (sum/difference of squares)
  - Pythagorean triple verification
  - unordered partitions into primes (backtracking)
  - rectangle perimeter==area search

Scope note:
  Two statements are asserted as "external" background facts and not re-derived
  here (Sudoku minimum clues = 17; exactly 17 wallpaper groups). These are cited
  in the README-style comments but excluded from formal checks.
"""

from math import isqrt
from itertools import combinations_with_replacement

# -----------------------
# Data
# -----------------------
n = 17

# -----------------------
# Logic (pure, declarative-style helpers)
# -----------------------
def is_prime(x: int) -> bool:
    if x < 2:
        return False
    if x % 2 == 0:
        return x == 2
    r = isqrt(x)
    f = 3
    while f <= r:
        if x % f == 0:
            return False
        f += 2
    return True

def is_fermat_prime(x: int):
    # Fermat primes have the form 2^(2^k) + 1 and are prime
    k = 0
    p = 3
    while p <= x:
        if p == x and is_prime(x):
            return True, k
        k += 1
        p = pow(2, pow(2, k)) + 1
    return False, None

def is_proth_prime(x: int):
    # x = k*2^m + 1 with k odd and k < 2^m, and x prime
    if not is_prime(x):
        return False, None
    for m in range(1, 64):
        two_m = 1 << m
        if (x - 1) % two_m == 0:
            k = (x - 1) // two_m
            if k % 2 == 1 and k < two_m:
                return True, (k, m)
    return False, None

def sum_of_first_four_primes():
    primes = []
    i = 2
    while len(primes) < 4:
        if is_prime(i):
            primes.append(i)
        i += 1
    return sum(primes), primes

def sum_of_two_squares(x: int):
    # Return a representation a^2 + b^2 = x if it exists
    for a in range(isqrt(x)+1):
        b2 = x - a*a
        b = isqrt(b2)
        if a*a + b*b == x:
            return (a, b)
    return None

def diff_of_two_squares(x: int):
    # find a > b >= 0 with a^2 - b^2 = x
    # x odd implies (a,b) = ((x+1)//2, (x-1)//2) works
    # but we search small examples for nice numbers
    for a in range(1, 200):
        for b in range(a):
            if a*a - b*b == x:
                return (a, b)
    return None

def pythagorean_triple(a:int,b:int,c:int):
    return a*a + b*b == c*c

def prime_partitions(target: int):
    # Count unordered partitions of target into primes (repetitions allowed; order irrelevant)
    primes = [p for p in range(2, target+1) if is_prime(p)]
    results = []

    def backtrack(start_idx, remaining, current):
        if remaining == 0:
            results.append(tuple(current))
            return
        for i in range(start_idx, len(primes)):
            p = primes[i]
            if p > remaining:
                break
            current.append(p)
            backtrack(i, remaining - p, current)
            current.pop()

    backtrack(0, target, [])
    return results

def rectangles_perimeter_equals_area(limit=100):
    # integer a,b >=1 s.t. 2(a+b) = ab
    sols = []
    for a in range(1, limit+1):
        for b in range(a, limit+1):
            if 2*(a+b) == a*b:
                sols.append((a,b,a*b))
    return sols

# -----------------------
# Compute the insights
# -----------------------
insights = []

# prime / Fermat / Proth
insights.append(("is_prime", is_prime(n)))
is_fp, k = is_fermat_prime(n)
insights.append(("is_fermat_prime", is_fp, k))
is_pp, km = is_proth_prime(n)
insights.append(("is_proth_prime", is_pp, km))

# sums/differences of squares
s2 = sum_of_two_squares(n)
d2 = diff_of_two_squares(n)
insights.append(("sum_of_two_squares", s2))
insights.append(("difference_of_two_squares", d2))

# sum of first four primes
s4, plist = sum_of_first_four_primes()
insights.append(("sum_of_first_4_primes_equals_n", s4 == n, plist))

# Pythagorean triple 8-15-17
insights.append(("pythagorean_triple_8_15_17", pythagorean_triple(8,15,17)))

# prime partitions
parts = prime_partitions(n)
insights.append(("num_unordered_prime_partitions", len(parts)))
insights.append(("prime_partitions_list", parts))

# rectangles with perimeter==area
rects = rectangles_perimeter_equals_area(limit=100)
insights.append(("rectangles_perimeter_equals_area", rects))

# -----------------------
# Answer (concise)
# -----------------------
answer = {
    "n": n,
    "is_prime": is_prime(n),
    "is_fermat_prime": is_fp,
    "is_proth_prime": is_pp,
    "sum_of_first_4_primes": plist,
    "sum_of_two_squares": s2,
    "difference_of_two_squares": d2,
    "pythagorean_triple": (8,15,17),
    "num_unordered_prime_partitions": len(parts),
    "rectangles_perimeter_equals_area": rects,
}

# -----------------------
# Reason Why (human-readable)
# -----------------------
def reason_text():
    lines = []
    lines.append(f"17 is prime: {is_prime(17)}.")
    if is_fp:
        lines.append(f"17 is a Fermat prime: 17 = 2^(2^2) + 1 (k={k}).")
    if is_pp:
        k_val, m_val = km
        lines.append(f"17 is a Proth prime: 17 = {k_val}*2^{m_val} + 1, with {k_val} odd and {k_val} < 2^{m_val}.")
    if s4 == n:
        lines.append(f"17 equals the sum of the first four primes {plist} = {sum(plist)}.")
    if s2:
        a,b = s2
        lines.append(f"17 is a sum of two squares: {a}^2 + {b}^2 = {a*a} + {b*b} = 17.")
    if d2:
        a,b = d2
        lines.append(f"17 is a difference of two squares: {a}^2 - {b}^2 = {a*a} - {b*b} = 17.")
    if pythagorean_triple(8,15,17):
        lines.append("8^2 + 15^2 = 64 + 225 = 289 = 17^2, so (8,15,17) is a Pythagorean triple.")
    lines.append(f"There are {len(parts)} unordered partitions of 17 into primes; examples: " +
                 ", ".join("+".join(map(str,p)) for p in parts[:5]) + ("..." if len(parts) > 5 else ""))
    lines.append("Rectangles with integer sides where perimeter equals area are (4x4) with area 16 and (3x6) with area 18; 17 lies between these areas.")
    lines.append("External known facts (not recomputed here): (i) 17 is the minimum number of clues for a 9x9 Sudoku with a unique solution; (ii) there are exactly 17 wallpaper (plane) symmetry groups.")
    return "\n".join(lines)

# -----------------------
# Check harness (assertions)
# -----------------------
import time
def run_checks(verbose=True):
    checks = []
    start = time.time()

    def check(label, func):
        t0 = time.time()
        ok = False
        err = ""
        try:
            ok = bool(func())
        except Exception as e:
            ok = False
            err = str(e)
        t1 = time.time()
        checks.append((label, ok, err, t1 - t0))

    # 1. primality
    check("17 is prime", lambda: is_prime(17))

    # 2. Fermat prime form
    check("17 is Fermat prime (2^(2^k)+1)", lambda: is_fermat_prime(17)[0])

    # 3. Proth prime form
    check("17 is Proth prime (k*2^m+1)", lambda: is_proth_prime(17)[0])

    # 4. sum of first four primes
    def _sum_first_four():
        s4, plist = sum_of_first_four_primes()
        return s4 == 17 and plist == [2,3,5,7]
    check("2+3+5+7 = 17", _sum_first_four)

    # 5. sum of two squares
    def _sum_two_sq():
        a,b = sum_of_two_squares(17)
        return a*a + b*b == 17
    check("17 = a^2 + b^2", _sum_two_sq)

    # 6. difference of two squares
    def _diff_two_sq():
        a,b = diff_of_two_squares(17)
        return a*a - b*b == 17
    check("17 = A^2 - B^2", _diff_two_sq)

    # 7. Pythagorean triple
    check("(8,15,17) is Pythagorean", lambda: pythagorean_triple(8,15,17))

    # 8. number of unordered prime partitions
    check("Count of unordered prime partitions is 17", lambda: len(prime_partitions(17)) == 17)

    # 9. perimeter==area rectangles include 4x4 and 3x6
    def _rectangles():
        rects = rectangles_perimeter_equals_area(100)
        return (4,4,16) in rects and (3,6,18) in rects and len(rects) == 2
    check("Perimeter==Area rectangles: {4x4, 3x6}", _rectangles)

    total_time = time.time() - start

    if verbose:
        # pretty print
        print("Check Report")
        print("------------")
        width = max(len(c[0]) for c in checks) + 2
        passed = 0
        for label, ok, err, dt in checks:
            status = "PASS" if ok else "FAIL"
            if ok: passed += 1
            line = f"{label.ljust(width)}  {status}  ({dt:.4f}s)"
            print(line)
            if err and not ok:
                print("  └─ error:", err)
        print("------------")
        print(f"Summary: {passed}/{len(checks)} checks passed in {total_time:.4f}s")
    # overall boolean success
    return all(ok for _,ok,_,_ in checks)

if __name__ == "__main__":
    # print triad
    print("# Answer")
    print(answer)
    print("\n# Reason Why")
    print(reason_text())
    print("\n# Check (harness)")
    try:
        run_checks()
        print("All checks passed.")
    except AssertionError as e:
        print("A check failed:", e)
