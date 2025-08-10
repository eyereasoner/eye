#!/usr/bin/env python3
"""
a^p = b! + p  (p prime) — "Reason why" + "Check (harness)"
===========================================================

What this file does
-------------------
• Exhaustively searches for positive integers (a, b, p prime) with
      a**p = b! + p
  subject to bounds MAX_P, MAX_B (tweak below).

• For each solution found, prints:
  - Reason why (number-theoretic structure explaining why this (a,b,p) can work),
  - Check (harness): concrete numeric confirmations
    (exact p-th root, p-adic valuation, modular sanity, and uniqueness in the window).

Number-theory guide (why so few solutions)
------------------------------------------
Let p be prime. Write N = b! + p.

1) If b ≥ 2p then v_p(b!) ≥ 2, so
        N = p * (something ≡ 1 mod p)  ⇒  v_p(N) = 1.
   But a^p has p-adic valuation p * v_p(a), a multiple of p.
   Since 1 is not a multiple of p, **impossible**. Hence b < 2p.

2) If b = p then b! = p·(p−1)! and Wilson’s theorem gives (p−1)! ≡ −1 (mod p).
   Thus N = p·((p−1)! + 1) has v_p(N) ≥ 2. For p ≥ 3 this valuation (≥2) is
   **not a multiple of p**, so a^p cannot equal N. The exceptional p=2 gives
        2! + 2 = 4 = 2^2  →  (a,b,p) = (2,2,2).

3) If p < b < 2p, write b = p + k with 1 ≤ k ≤ p−1. Then v_p(b!) = 1 and
        N = p·(T + 1),  where  T = b!/p ≡ −k!  (mod p).
   So v_p(N) = 1 + v_p(T+1). For equality a^p = N we need v_p(N) ≡ 0 (mod p),
   hence v_p(T+1) ≡ −1 (mod p) ⇒ v_p(T+1) = p−1 (typically impossible) —
   except for small primes where higher divisibility can occur.
   Indeed for p=3, b=4 one gets 4! + 3 = 27 = 3^3.

In the common search window, the only hits are the classical:
    (a,b,p) = (2,2,2) and (3,4,3).

Implementation notes
--------------------
• Pure Python only (no imports). We implement:
  - a prime sieve,
  - an integer p-th root via binary search,
  - a p-adic valuation v_p(n),
  - a small harness for modular checks and uniqueness-by-exhaustion.

• Complexity is dominated by growing b! and testing the integer p-th root.
  This is fine for the modest bounds below; increase as desired.
"""

# --------------------------- tiny integer toolbox ---------------------------

def primes_up_to(N: int):
    """Sieve of Eratosthenes: list all primes ≤ N."""
    if N < 2: return []
    sieve = [True]*(N+1)
    sieve[0]=sieve[1]=False
    p=2
    while p*p<=N:
        if sieve[p]:
            for k in range(p*p, N+1, p):
                sieve[k]=False
        p+=1
    return [i for i in range(N+1) if sieve[i]]

def int_nth_root(n: int, p: int):
    """Return (r, exact) where r = floor(n**(1/p)) and exact iff r**p == n."""
    if n in (0,1): return n, True
    lo, hi = 1, 1
    while pow(hi, p) < n:
        hi <<= 1
    lo = hi >> 1
    while lo < hi:
        mid = (lo + hi + 1)//2
        t = pow(mid, p)
        if t <= n: lo = mid
        else: hi = mid - 1
    r = lo
    return r, (pow(r, p) == n)

def vp(n: int, p: int) -> int:
    """p-adic valuation v_p(n): largest t with p^t | n (p prime)."""
    if n == 0: return 10**9
    t=0
    while n % p == 0:
        n//=p
        t+=1
    return t

# --------------------------- search params ----------------------------------

MAX_P, MAX_B = 97, 200

# --------------------------- search ----------------------------------------

solutions = []      # list of (a,b,p)
hits_by_p = {}      # p -> list of (a,b) (for uniqueness display)

for p in primes_up_to(MAX_P):
    hits_by_p[p] = []
    fact = 1
    for b in range(1, MAX_B+1):
        fact *= b
        N = fact + p
        r, exact = int_nth_root(N, p)
        a = r if exact else (r+1 if pow(r+1, p) == N else None)
        if a is not None:
            solutions.append((a,b,p))
            hits_by_p[p].append((a,b))

# --------------------------- explain & harness ------------------------------

def reason_why(a:int, b:int, p:int) -> str:
    """Short textual reason specialized to (a,b,p), echoing the guide above."""
    parts = []
    if b >= 2*p:
        parts.append(f"Since b≥2p, v_{p}(b!)≥2 ⇒ v_{p}(b!+{p})=1,")
        parts.append("but v_p(a^p) is a multiple of p — impossible (no such solutions).")
    elif b == p:
        if p == 2:
            parts.append("Here b=p=2: 2!+2=4=2^2 is the classical small solution.")
        else:
            parts.append("Here b=p; Wilson: (p−1)!≡−1 (mod p) ⇒ v_p(b!+p)≥2,")
            parts.append("which is not a multiple of p for p≥3, so equality cannot hold.")
    elif p < b < 2*p:
        if p == 3 and b == 4 and a == 3:
            parts.append("Here p=3, b=4: 4!+3=27=3^3 works; v_3 equals 3 as required.")
        else:
            parts.append("Here p<b<2p: v_p(b!)=1 ⇒ v_p(b!+p)=1+v_p((b!/p)+1);")
            parts.append("needing a multiple of p is rare — this window is checked exhaustively.")
    else:  # b < p
        parts.append("Here b<p, so p∤b! and b!+p≡p (mod p)≠0;")
        parts.append("thus a is not divisible by p and a^p≡b! (mod p) must still match exactly.")
    return "\n  ".join(parts)

def check_harness(a:int, b:int, p:int) -> str:
    """Independent verification: equality, exact root, valuation, modular, uniqueness."""
    # recompute facts deterministically
    fact = 1
    for k in range(1, b+1): fact *= k
    N = fact + p
    eq_ok = (pow(a,p) == N)

    r, exact = int_nth_root(N, p)
    vN = vp(N, p)
    va = vp(a, p)

    # modular sanity
    if b >= p:
        mod_ok = (N % p == 0) and (pow(a, p, p) == 0)
        mod_line = f"b≥p ⇒ b!≡0 (mod {p}); N≡0 and a^{p}≡0 (mod {p}) → {mod_ok}"
    else:
        mod_ok = (pow(a, p, p) == (fact % p))
        mod_line = f"b<p ⇒ N≡p (mod {p}); check a^{p}≡b! (mod {p}) → {mod_ok}"

    # uniqueness for this p in window
    peers = [pair for pair in hits_by_p[p] if pair != (a,b)]
    uniq = (len(peers) == 0)
    uniq_line = f"Uniqueness in window (p≤{MAX_P}, b≤{MAX_B}): " + \
                ("unique for this p" if uniq else f"also {peers}")

    lines = []
    lines.append(f"Compute: {b}! = {fact}, so {b}! + {p} = {N} and {a}^{p} = {pow(a,p)}.")
    lines.append(f"Exact p-th root: floor(({N})**(1/{p})) = {r}, exact={exact}.")
    lines.append(f"p-adic valuation: v_{p}({N}) = {vN}, v_{p}({a}^{p}) = {p}·v_{p}({a}) = {p}·{va} = {p*va}.")
    lines.append(f"Equality check: {a}^{p} == {N} → {eq_ok}.")
    lines.append("Modular check: " + mod_line + ".")
    lines.append(uniq_line + ".")
    return "\n  ".join(lines)

# --------------------------- output ----------------------------------------

if __name__ == "__main__":
    if not solutions:
        print(f"No solutions up to p≤{MAX_P}, b≤{MAX_B}.")
    else:
        print(f"Solutions up to p≤{MAX_P}, b≤{MAX_B}:")
        for a,b,p in solutions:
            print(f"  • (a,b,p) = ({a}, {b}, {p})")

        for a,b,p in solutions:
            print("\n" + "="*70)
            print(f"Solution (a,b,p)=({a},{b},{p})")
            print("Reason why:")
            print("  " + reason_why(a,b,p))
            print("\nCheck (harness):")
            print("  " + check_harness(a,b,p))

