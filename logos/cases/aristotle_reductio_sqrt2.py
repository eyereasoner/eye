#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Aristotle & Proof by Contradiction (Apagogé): √2 is irrational
--------------------------------------------------------------

What this program models (philosophically):
- Aristotle accepts 'proof by the impossible' when the impossibility
  (a contradiction) follows *necessarily* from true principles,
  especially the Principle of Non-Contradiction (PNC).
- In classical Greek mathematics (Euclid), irrationality of √2 is
  established *by contradiction*: assuming √2 is a ratio of whole
  numbers in lowest terms leads to both numerator and denominator being
  even, which contradicts "lowest terms." The contradiction is not
  rhetorical but necessary (parity facts + definitions).

How we encode that here:
- We pick the proposition P: "√2 is rational."
- We assume P and derive an impossibility: a pair (p, q) in lowest terms
  with p^2 = 2 q^2 must have p and q both even — contradicting "lowest terms."
- By PNC, we reject P; hence ¬P: √2 is irrational.

Program outputs:
1) Answer  – states √2 is irrational.
2) Reason  – explains the reductio steps concisely.
3) Check   – an independent harness:
     (A) Parity lemma: p^2 is even  <=>  p is even.
     (B) Non-square fact: 2·q^2 is never a perfect square (for many q).
     (C) No coprime solution to p^2 = 2 q^2 in a broad search.
     (D) Convergents to √2 get close but never equal it (sanity only).

Note: This is arithmetic/geometry—exactly the sort of domain Aristotle
took to be apt for strict demonstration (ἀπόδειξις).
"""

import math
import random

# --------------------------- Utilities -----------------------------------------

EPS = 1e-12
RANDOM_SEED = 0

def section(title: str):
    print("\n" + "="*len(title))
    print(title)
    print("="*len(title))

def is_even(n: int) -> bool:
    return (n & 1) == 0

def is_square(n: int) -> bool:
    if n < 0: return False
    r = math.isqrt(n)
    return r*r == n

# --------------------------- Core lemmas ----------------------------------------

def parity_square_equivalence(n: int) -> bool:
    """
    Lemma: n^2 is even  <=>  n is even.
    -> (=>) If n^2 even then n is even: standard parity argument.
    -> (<=) If n even, then n^2 even: immediate.
    We check equivalence numerically for many n (harness), but it is a theorem.
    """
    n2_even = is_even(n*n)
    n_even  = is_even(n)
    return (n2_even == n_even)

def sqrt2_rational_contradiction_exists(limit_q: int = 2000) -> bool:
    """
    Search for coprime integers p, q (1 <= q <= limit_q) s.t. p^2 = 2 q^2.
    If found, returns True (which would indicate a contradiction of mathematics).
    We expect to find none.
    """
    for q in range(1, limit_q + 1):
        m = 2*q*q
        p = math.isqrt(m)
        if p*p == m:
            # A candidate; check if coprime
            if math.gcd(p, q) == 1:
                # This would contradict the theorem (and Euclid) — should never happen.
                return True
    return False

def convergents_sqrt2(k: int):
    """
    Generate first k+1 convergents to sqrt(2).
    sqrt(2) = [1; 2, 2, 2, ...]
    Recurrence:
        p_{-2}=0, p_{-1}=1; q_{-2}=1, q_{-1}=0
        p_k = a_k p_{k-1} + p_{k-2},  q_k = a_k q_{k-1} + q_{k-2}
    where a_0=1 and a_k=2 for k>=1.
    """
    a0 = 1
    p_prev2, p_prev1 = 0, 1
    q_prev2, q_prev1 = 1, 0

    # k=0:
    p0 = a0*p_prev1 + p_prev2   # 1*1 + 0 = 1
    q0 = a0*q_prev1 + q_prev2   # 1*0 + 1 = 1
    seq = [(p0, q0)]

    p_prev2, p_prev1 = p_prev1, p0
    q_prev2, q_prev1 = q_prev1, q0

    for _ in range(1, k+1):
        a = 2
        p = a*p_prev1 + p_prev2
        q = a*q_prev1 + q_prev2
        seq.append((p, q))
        p_prev2, p_prev1 = p_prev1, p
        q_prev2, q_prev1 = q_prev1, q

    return seq

# --------------------------- 1) Answer -----------------------------------------

def answer():
    section("Answer")
    print("Conclusion: √2 is irrational (not a ratio of whole numbers).")

# --------------------------- 2) Reason -----------------------------------------

def reason():
    section("Reason (Aristotelian apagogé / reductio)")
    print(
        "Assume, for contradiction, that √2 = p/q in lowest terms (gcd(p,q)=1).\n"
        "Then 2 = p²/q², so p² = 2·q². Thus p² is even, hence p is even (parity lemma).\n"
        "Write p = 2k. Substituting, (2k)² = 2·q² ⇒ 4k² = 2·q² ⇒ q² = 2k², so q is even.\n"
        "But then p and q are both even, contradicting the 'lowest terms' assumption.\n"
        "Therefore the assumption is impossible; by the Principle of Non-Contradiction,\n"
        "we reject it. Hence √2 is irrational."
    )

# --------------------------- 3) Independent Check (harness) --------------------

def check():
    section("Independent Check (harness)")
    random.seed(RANDOM_SEED)
    ok = True

    # (A) Parity lemma sanity: n^2 even <=> n even, over a wide sample
    for n in range(-2000, 2001):
        if not parity_square_equivalence(n):
            ok = False; print("[FAIL] Parity lemma fails at n =", n); break
    if ok: print("[PASS] Parity lemma: n^2 even  ⇔  n even (tested on [-2000,2000])")

    # (B) Non-square fact: for many q, 2·q^2 is NOT a perfect square
    if ok:
        for q in range(1, 5000):
            if is_square(2*q*q):
                ok = False; print("[FAIL] Found q with 2*q^2 a perfect square:", q); break
        if ok: print("[PASS] For 1 ≤ q ≤ 4999, 2·q^2 is never a perfect square")

    # (C) No coprime solution p^2 = 2 q^2 in search
    if ok:
        if sqrt2_rational_contradiction_exists(limit_q=5000):
            ok = False; print("[FAIL] Found coprime p,q with p^2 = 2 q^2")
        else:
            print("[PASS] No coprime p,q satisfy p^2 = 2 q^2 up to q ≤ 5000")

    # (D) Convergents approach √2 but never equal it (sanity)
    if ok:
        root2 = math.sqrt(2.0)
        conv = convergents_sqrt2(8)   # a few convergents
        all_ok = True
        for (p, q) in conv:
            approx = p / q
            if abs(approx - root2) < 1e-15:  # impossible in rationals, but just to be explicit
                all_ok = False
                print("[FAIL] Convergent equals √2 exactly:", p, "/", q)
                break
        if all_ok:
            print("[PASS] Convergents get close to √2 but (as rationals) never equal it")
            print("      First few convergents and errors:")
            for (p, q) in conv:
                err = abs(p/q - root2)
                print(f"        {p}/{q}   error ≈ {err:.3e}")

    print("\nResult:", "ALL CHECKS PASSED ✅" if ok else "Some checks FAILED ❌")
    return ok

# --------------------------- Main ----------------------------------------------

if __name__ == "__main__":
    answer()
    reason()
    check()

