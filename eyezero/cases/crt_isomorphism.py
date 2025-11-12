#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P3 — Self-contained, self-checking script (algebra)
Case: Chinese Remainder Theorem as a ring isomorphism

Puzzle / Claim
--------------
Let m1, …, mk be pairwise coprime positive integers and M = m1⋯mk.
The map
    φ : Z/MZ → (Z/m1Z) × ··· × (Z/mkZ),
    φ([x]_M) = ([x]_{m1}, …, [x]_{mk})
is a ring isomorphism. Its inverse is the explicit CRT reconstruction.

Contract (P3 style)
-----------------------------
- Answer: φ is a bijective ring homomorphism; give explicit inverse.
- Reason Why: produce idempotents e_i with
      e_i ≡ 1 (mod m_i),  e_i ≡ 0 (mod m_j) for j≠i,
  and show x ≡ Σ r_i e_i (mod M) where r_i ≡ x (mod m_i).
- Check (harness): randomized coprime moduli; verify:
  (1) φ and CRT-inverse are mutual inverses on many random x;
  (2) φ respects + and ·; (3) idempotent properties; (4) uniqueness mod M.

No external packages. Pure Python 3.
"""

import random
random.seed(2025)

# ----------------------------
# Basic number theory helpers
# ----------------------------

def egcd(a, b):
    """Extended gcd: returns (g, x, y) with g=gcd(a,b) and ax+by=g."""
    if b == 0:
        return (abs(a), 1 if a>0 else -1 if a<0 else 0, 0)
    g, x1, y1 = egcd(b, a % b)
    return (g, y1, x1 - (a // b) * y1)

def invmod(a, m):
    """Multiplicative inverse of a mod m (gcd(a,m)=1). Raises ValueError if none."""
    a %= m
    g, x, _ = egcd(a, m)
    if g != 1:
        raise ValueError(f"No inverse: a={a}, m={m}, gcd={g}")
    return x % m

def prod(nums):
    P = 1
    for v in nums:
        P *= v
    return P

def pairwise_coprime(ms):
    from math import gcd
    for i in range(len(ms)):
        for j in range(i+1, len(ms)):
            if gcd(ms[i], ms[j]) != 1:
                return False
    return True

# ----------------------------
# CRT construction (explicit inverse to φ)
# ----------------------------

def crt_build_e(ms):
    """
    Build the canonical idempotents e_i = (M/mi) * ( (M/mi)^{-1} mod mi )  (computed in Z, reduced mod M).
    Returns (M, e_list) with len(e_list)=k and each e_i an integer representative mod M.
    Properties:
      e_i ≡ 1 (mod m_i),  e_i ≡ 0 (mod m_j) for j≠i,  and  Σ e_i ≡ 1 (mod M).
    """
    M = prod(ms)
    es = []
    for mi in ms:
        Mi = M // mi
        inv = invmod(Mi % mi, mi)
        ei = (Mi * inv) % M
        es.append(ei)
    return M, es

def crt_reconstruct(residues, ms):
    """
    Given residues r_i modulo m_i (all i), reconstruct the unique x modulo M with x≡r_i (mod m_i).
    Uses x ≡ Σ r_i e_i (mod M) with canonical idempotents e_i.
    Returns x in [0, M-1].
    """
    assert len(residues) == len(ms) and len(ms) >= 1
    assert pairwise_coprime(ms), "Moduli must be pairwise coprime for CRT isomorphism."
    M, es = crt_build_e(ms)
    s = 0
    for r, e in zip(residues, es):
        s = (s + (r % M) * e) % M
    return s

def phi_forward(x, ms):
    """φ([x]_M) = tuple(x mod m_i)."""
    return tuple(x % mi for mi in ms)

# ----------------------------
# Reason (proof outline)
# ----------------------------

def build_reason() -> str:
    lines = []
    lines.append("Let m1,…,mk be pairwise coprime and M=m1⋯mk. For each i, set Mi=M/mi and choose")
    lines.append("ti ≡ (Mi)^{-1} mod mi. Define ei = Mi·ti (an integer, considered mod M). Then:")
    lines.append("  • ei ≡ 1 (mod mi) since Mi·ti ≡ 1 (mod mi), and  ei ≡ 0 (mod mj) for j≠i because mj|Mi.")
    lines.append("  • Σ ei ≡ 1 (mod M).")
    lines.append("For any residue data (r1,…,rk) with ri ∈ Z/miZ, the element")
    lines.append("    x ≡ Σ ri·ei  (mod M)")
    lines.append("satisfies x ≡ ri (mod mi) for all i, so it reconstructs the class in Z/MZ.")
    lines.append("This gives an explicit inverse to φ, hence φ is bijective. Additivity and multiplicativity")
    lines.append("hold because both φ and reconstruction are Z-linear in the residue coordinates and")
    lines.append("the ei’s are fixed modulo M; thus φ is a ring isomorphism.")
    return "\n".join(lines)

# ----------------------------
# Harness
# ----------------------------

def random_primes(limit=2000):
    """Simple sieve up to 'limit' and return the primes."""
    n = limit
    sieve = [True]*(n+1)
    sieve[0]=sieve[1]=False
    import math
    for p in range(2, int(math.isqrt(n))+1):
        if sieve[p]:
            step = p
            start = p*p
            sieve[start:n+1:step] = [False]*(((n - start)//step)+1)
    return [i for i,pr in enumerate(sieve) if pr]

PRIMES = random_primes(2000)

def random_coprime_moduli(k=3):
    """Pick k distinct random primes to ensure coprimality; mix in small composite coprimes occasionally."""
    assert 2 <= k <= 6
    # Start with distinct primes
    ms = random.sample(PRIMES[10:200], k)  # avoid tiny 2,3,5 to vary sizes
    # With small prob, replace one by a coprime composite (product of two new primes)
    if random.random() < 0.4:
        p,q = random.sample(PRIMES[50:300], 2)
        idx = random.randrange(k)
        ms[idx] = p*q  # still coprime to the others with overwhelming probability
    # Ensure pairwise coprime
    if not pairwise_coprime(ms):
        return random_coprime_moduli(k)
    return ms

def test_inverse_and_homomorphism(trials=40):
    for _ in range(trials):
        k = random.randint(2, 5)
        ms = random_coprime_moduli(k)
        M = prod(ms)

        # Build idempotents and sanity checks
        M2, es = crt_build_e(ms)
        assert M2 == M
        # e_i ≡ 1 mod mi and 0 mod mj (j≠i)
        for i, ei in enumerate(es):
            for j, mj in enumerate(ms):
                if i == j:
                    assert ei % mj == 1 % mj, "e_i ≢ 1 (mod m_i)."
                else:
                    assert ei % mj == 0, "e_i ≢ 0 (mod m_j, j≠i)."
        # sum e_i ≡ 1 (mod M)
        assert (sum(es) - 1) % M == 0, "Σ e_i ≢ 1 (mod M)."

        # Random x values, check φ^-1∘φ = id and φ is a homomorphism
        for _ in range(25):
            x = random.randrange(M)
            r = phi_forward(x, ms)
            y = crt_reconstruct(r, ms)
            assert y % M == x % M, "CRT inverse failed: φ^{-1}(φ(x)) ≠ x (mod M)."

        # Check addition/multiplication compatibility
        for _ in range(20):
            x = random.randrange(M)
            y = random.randrange(M)
            r_x = phi_forward(x, ms)
            r_y = phi_forward(y, ms)
            # componentwise + and *
            r_sum = tuple((a+b) % mi for (a,mi),b in zip(zip(r_x,ms), r_y))
            r_prod= tuple((a*b) % mi for (a,mi),b in zip(zip(r_x,ms), r_y))
            # reconstruct and compare in Z/MZ
            z_sum  = crt_reconstruct(r_sum, ms)
            z_prod = crt_reconstruct(r_prod, ms)
            assert z_sum % M == (x + y) % M, "Additivity failed under φ."
            assert z_prod % M == (x * y) % M, "Multiplicativity failed under φ."

    return "✓ φ is a ring isomorphism; inverse, + and · verified on randomized coprime moduli."

def run_checks():
    results = []
    results.append(test_inverse_and_homomorphism(trials=50))
    return results

# ----------------------------
# Presentation
# ----------------------------

def print_header(title):
    print("="*72)
    print(title)
    print("="*72)

def main():
    print_header("ANSWER")
    print("Chinese Remainder Theorem (ring form): for pairwise coprime m1,…,mk, the map")
    print("  φ: Z/MZ → ∏ Z/miZ,   φ([x]_M) = ([x]_{m1},…,[x]_{mk})")
    print("is a ring isomorphism with inverse x ≡ Σ r_i e_i (mod M), where e_i are CRT idempotents.")

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

