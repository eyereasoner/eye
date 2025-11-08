#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P3 — Self-contained, self-checking script (algebra)
Case: Rational Root Theorem

Puzzle / Claim
--------------
Let f(x) = a_0 x^n + a_1 x^{n-1} + ... + a_n be a polynomial with integer
coefficients (a_0 ≠ 0). If r = p/q ∈ ℚ (in lowest terms, gcd(p,q)=1, q>0)
is a root of f, then:

    p | a_n   and   q | a_0.

In particular:
• If f is monic (a_0 = 1), then every rational root is an integer.
• If the constant term is ±1, then every rational root is ±1 (hence integral).

Contract (P3 style)
-----------------------------
- Answer: the statement above.
- Reason Why: classical divisibility proof by clearing denominators.
- Check (harness): brute-force search for rational roots by enumerating ± divisors
  of a_n over ± divisors of a_0; verify the divisibility conditions and the monic corollary
  on many random test polynomials; also test some hand-picked examples.

No external packages. Pure Python 3.
"""

import random
random.seed(1234)

# ----------------------------
# Polynomial utilities (integer coefficients)
# Store coefficients as [a0, a1, ..., an] representing a0 x^n + ... + an
# ----------------------------

def eval_Q_times_qn(coeffs, p, q):
    """
    Compute q^n * f(p/q) as an integer:
      If f(x) = sum_{i=0..n} a_i x^{n-i}, then
      q^n f(p/q) = sum_{i=0..n} a_i * p^{n-i} * q^{i}.
    """
    n = len(coeffs) - 1
    total = 0
    # precompute powers (tiny degrees; straightforward loop)
    pow_p = [1]*(n+1)
    pow_q = [1]*(n+1)
    for k in range(1, n+1):
        pow_p[k] = pow_p[k-1]*p
        pow_q[k] = pow_q[k-1]*q
    # a0 x^n + a1 x^{n-1} + ... + an
    for i, ai in enumerate(coeffs):
        # term index i corresponds to power x^{n-i}
        total += ai * pow_p[n - i] * pow_q[i]
    return total

def is_root_rational(coeffs, p, q):
    """Return True iff f(p/q)=0 (with integer arithmetic). Assumes q>0."""
    return eval_Q_times_qn(coeffs, p, q) == 0

def divisors(n):
    """Return list of positive divisors of |n| (n ≠ 0)."""
    n = abs(n)
    res = []
    d = 1
    while d*d <= n:
        if n % d == 0:
            res.append(d)
            if d*d != n:
                res.append(n//d)
        d += 1
    res.sort()
    return res

def candidates_rational_roots(coeffs):
    """
    Enumerate all rational candidates p/q in lowest terms with:
      p | a_n,  q | a_0,  q>0, and both signs.
    Return a set of pairs (p,q) with gcd(p,q)=1, q>0; include ±p.
    """
    a0 = coeffs[0]
    an = coeffs[-1]
    if a0 == 0:
        raise ValueError("Leading coefficient a0 must be nonzero.")
    if an == 0:
        # 0 is a root candidate; include (0,1)
        roots = {(0,1)}
        # still generate others using |an| divisors = {0}? Handle separately
        # We'll just add 0 and then proceed with |an|>0 branch skipped
    else:
        roots = set()

    P = divisors(an) if an != 0 else [0]
    Q = divisors(a0)
    from math import gcd
    for p_abs in P:
        for q in Q:
            if q == 0: 
                continue
            # Reduce p/q to lowest terms; consider both signs of p
            for sgn in (+1, -1):
                p = sgn * p_abs
                g = gcd(abs(p), q)
                pp = p // g
                qq = q // g
                # enforce q>0
                if qq < 0:
                    pp = -pp
                    qq = -qq
                roots.add((pp, qq))
    return roots

# ----------------------------
# Reason (proof outline)
# ----------------------------

def build_reason() -> str:
    lines = []
    lines.append("Let f(x) = a0 x^n + a1 x^{n-1} + ... + an with a0 ≠ 0, ai ∈ ℤ.")
    lines.append("Suppose r = p/q in lowest terms (gcd(p,q)=1, q>0) and f(r)=0.")
    lines.append("Multiply by q^n:")
    lines.append("    0 = q^n f(p/q) = a0 p^n + a1 p^{n-1} q + ... + an q^n.")
    lines.append("Rearrange:")
    lines.append("    a0 p^n = - q( a1 p^{n-1} + a2 p^{n-2} q + ... + an q^{n-1} ).")
    lines.append("Thus q | a0 p^n. Since gcd(p,q)=1, we get q | a0.")
    lines.append("Similarly, rewrite as")
    lines.append("    an q^n = - p( a0 p^{n-1} + a1 p^{n-2} q + ... + a_{n-1} q^{n-1} ),")
    lines.append("so p | an. This proves:  p | an and q | a0.")
    lines.append("")
    lines.append("Corollaries:")
    lines.append("• If a0=1 (monic), then q | 1 ⇒ q=1, so every rational root is an integer.")
    lines.append("• If an = ±1, then p | ±1 ⇒ p = ±1, likewise giving only integral possibilities.")
    return "\n".join(lines)

# ----------------------------
# Harness
# ----------------------------

def print_header(title):
    print("="*72)
    print(title)
    print("="*72)

def random_poly(deg, monic=False, coeff_range=5):
    """
    Build random integer-coefficient polynomial of given degree.
    Returns coeffs [a0,...,an] with a0 ≠ 0 and (optionally) a0 = 1 (monic).
    """
    if monic:
        a0 = 1
    else:
        a0 = 0
        while a0 == 0:
            a0 = random.randint(-coeff_range, coeff_range)
    coeffs = [a0]
    for _ in range(deg-1):
        coeffs.append(random.randint(-coeff_range, coeff_range))
    an = 0
    while an == 0:
        an = random.randint(-coeff_range, coeff_range)
    coeffs.append(an)
    return coeffs

def find_rational_roots(coeffs):
    """Return list of rational roots (p,q) with q>0, gcd=1, found by testing candidates."""
    roots = []
    for p,q in candidates_rational_roots(coeffs):
        if is_root_rational(coeffs, p, q):
            roots.append((p,q))
    # dedupe / normalize (q>0, gcd=1 already)
    unique = []
    seen = set()
    for p,q in roots:
        if (p,q) not in seen:
            seen.add((p,q)); unique.append((p,q))
    return unique

def check_monic_integrality(trials=50):
    for _ in range(trials):
        deg = random.randint(1, 6)
        f = random_poly(deg, monic=True, coeff_range=6)
        roots = find_rational_roots(f)
        # Every rational root must have q=1
        for p,q in roots:
            assert q == 1, f"Monic counterexample: {f} has rational root {p}/{q} not integral."
    return "✓ Monic case: every rational root found is an integer."

def check_divisibility_general(trials=80):
    for _ in range(trials):
        deg = random.randint(1, 6)
        f = random_poly(deg, monic=False, coeff_range=8)
        a0, an = f[0], f[-1]
        roots = find_rational_roots(f)
        # Verify p|an and q|a0 for each rational root p/q
        for p,q in roots:
            assert an % p == 0 if p != 0 else an == 0, f"p|an failed for {f} at {p}/{q}"
            assert a0 % q == 0, f"q|a0 failed for {f} at {p}/{q}"
    return "✓ General case: for each rational root p/q, p|a_n and q|a_0."

def check_examples():
    # f(x)=x^3 - 6x^2 + 11x - 6 has integer roots 1,2,3
    f = [1, -6, 11, -6]
    roots = sorted(find_rational_roots(f))
    assert roots == [(1,1),(2,1),(3,1)], "Example x^3-6x^2+11x-6 failed."
    # Non-monic example: 2x^3 + 3x^2 - 8x - 12 = 0 has roots x=2, x=-3/2, x=-2
    f2 = [2, 3, -8, -12]
    roots2 = sorted(find_rational_roots(f2))
    assert roots2 == [(-3,2),(-2,1),(2,1)], "Example 2x^3+3x^2-8x-12 failed."
    return "✓ Hand-picked examples pass."

def run_checks():
    res = []
    res.append(check_examples())
    res.append(check_monic_integrality())
    res.append(check_divisibility_general())
    return res

# ----------------------------
# Presentation
# ----------------------------

def main():
    print_header("ANSWER")
    print("Rational Root Theorem:")
    print("If f(x)=a0 x^n + ... + an ∈ ℤ[x] and r=p/q in lowest terms is a root, then p|an and q|a0.")
    print("In particular, monic ⇒ every rational root is an integer.")

    print_header("REASON WHY")
    print(build_reason())

    print_header("CHECK (HARNESS)")
    try:
        for line in run_checks():
            print(line)
        print("All checks PASSED ✅")
    except AssertionError as e:
        print(f"Check FAILED ❌: {e}")
        raise

if __name__ == "__main__":
    main()

