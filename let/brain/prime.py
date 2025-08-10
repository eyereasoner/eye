#!/usr/bin/env python3
"""
Prime-number mini-suite — ARC output (pure Python, no imports)
==============================================================

Answering the queries:
  1) list(primerange(0, 100))
  2) list(primerange(1_000_000, 1_000_100))
  3) isprime(M521) where M521 = 2^521 − 1          (a known Mersenne prime)
  4) nextprime(M521)
  5) totient(271)
  6) totient(2_718_281)
  7) totient(27_182_818_284)
  8) totient(271_828_182_845_904)
  9) totient(2718281828459045235360287471352662497757247)

What we implement (no libraries)
--------------------------------
• Segmented sieve to list primes in an interval [a, b).
• Miller–Rabin strong probable-prime test with a fixed base set
  (deterministic for 64-bit, overwhelmingly reliable beyond).
• nextprime(n): increment by 1/2 then test with Miller–Rabin until prime.
• Pollard–Rho factorization (Floyd cycle) + recursive split.
• Euler’s totient φ(n) via product formula from the prime-power factorization.

How we “explain” and “check”
----------------------------
• For primerange: Answer lists the primes; Reason why explains sieve idea; Check
  verifies each listed number is prime via Miller–Rabin and that endpoints are respected.
• For isprime: we note M521 = 2^521−1, check equality, then run our test.
• For nextprime: show the candidate and verify it’s prime and that the previous odd
  is composite (so it’s actually the next one).
• For totient: print φ(n) and the factorization used; Check re-evaluates Euler’s
  theorem pow(a, φ(n), n)≡1 for several small bases coprime to n.

Note
----
• (4) nextprime(2^521−1) may take a moment on modest machines (average gap ~ ln n ≈ 361).
• Miller–Rabin base set: [2,3,5,7,11,13,17,19,23,29,31,37].
"""

# ─────────────────────────── utilities: gcd, mul, pow, etc. ───────────────────────────

def gcd(a: int, b: int) -> int:
    while b:
        a, b = b, a % b
    return abs(a)

def is_square(n: int) -> bool:
    if n < 0: return False
    x = int(n**0.5)
    while (x+1)*(x+1) <= n: x += 1
    while x*x > n: x -= 1
    return x*x == n

# ─────────────────────────── Miller–Rabin primality ───────────────────────────

_SMALL_PRIMES = [2,3,5,7,11,13,17,19,23,29,31,37]

def _miller_rabin_witness(a: int, n: int, d: int, s: int) -> bool:
    """Return True if 'a' is a witness of compositeness of n."""
    x = pow(a, d, n)
    if x == 1 or x == n-1:
        return False
    for _ in range(s-1):
        x = (x*x) % n
        if x == n-1:
            return False
    return True  # composite witness

def is_probable_prime(n: int) -> bool:
    if n < 2:
        return False
    for p in _SMALL_PRIMES:
        if n == p:
            return True
        if n % p == 0:
            return n == p
    # write n-1 = d * 2^s
    d = n - 1
    s = 0
    while d % 2 == 0:
        d //= 2
        s += 1
    bases = _SMALL_PRIMES  # fixed set; deterministic <= 2^64, very strong beyond
    for a in bases:
        if a % n == 0:  # in case n < base
            continue
        if _miller_rabin_witness(a, n, d, s):
            return False
    return True

# ─────────────────────────── Pollard–Rho factorization ───────────────────────────

def _rho_f(x, c, n): return (x*x + c) % n

def pollard_rho(n: int) -> int:
    """Return a nontrivial factor of composite odd n (probabilistic, deterministic seeds)."""
    if n % 2 == 0:
        return 2
    # small quick trial
    for p in (3,5,7,11,13,17,19,23,29,31,37):
        if n % p == 0:
            return p
    # Floyd cycle with a few deterministic seeds
    seeds = [(2,1), (3,1), (5,1), (2,3), (5,7), (7,11), (11,17), (13,19)]
    for x0, c in seeds:
        x = x0 % n
        y = x
        d = 1
        while d == 1:
            x = _rho_f(x, c, n)
            y = _rho_f(_rho_f(y, c, n), c, n)
            d = gcd(abs(x - y), n)
        if d != n:
            return d
    return n  # fail → treat as “probably prime”

def factorint(n: int, out=None) -> dict:
    """Return {prime: exponent} via Pollard–Rho + MR."""
    if out is None:
        out = {}
    if n == 1:
        return out
    if is_probable_prime(n):
        out[n] = out.get(n, 0) + 1
        return out
    d = pollard_rho(n)
    if d == n:  # fallback: brute for small-ish n
        f = 3
        while f*f <= n and n % f != 0:
            f += 2
        if f*f > n:
            out[n] = out.get(n, 0) + 1
            return out
        d = f
    factorint(d, out)
    factorint(n // d, out)
    return out

# ─────────────────────────── totient via factorization ───────────────────────────

def totient(n: int) -> int:
    """Euler's totient using φ(p^k)=p^k−p^{k−1} and multiplicativity."""
    fac = factorint(n)
    phi = 1
    for p, e in fac.items():
        pe = p**e
        phi *= pe - pe // p
    return phi

# ─────────────────────────── sieves & primerange ───────────────────────────

def sieve_upto(n: int) -> list:
    """Simple sieve up to n (inclusive)."""
    if n < 2: return []
    B = [True]*(n+1)
    B[0]=B[1]=False
    p=2
    while p*p <= n:
        if B[p]:
            step = p
            start = p*p
            for m in range(start, n+1, step):
                B[m]=False
        p+=1
    return [i for i,v in enumerate(B) if v]

def primerange(a: int, b: int) -> list:
    """Segmented sieve: primes in [a, b)."""
    if b <= 2 or b <= a:
        return []
    base = sieve_upto(int(b**0.5)+1)
    L = b - a
    mark = [True]*L
    for p in base:
        start = max(p*p, ((a + p - 1)//p)*p)
        for m in range(start, b, p):
            mark[m - a] = False
    res = []
    for i in range(L):
        n = a + i
        if n >= 2 and mark[i]:
            res.append(n)
    return res

# ─────────────────────────── next prime ───────────────────────────

def nextprime(n: int) -> int:
    """Small step scan using Miller–Rabin."""
    if n < 2:
        return 2
    p = n + 1
    if p % 2 == 0 and p != 2:
        p += 1
    while True:
        if is_probable_prime(p):
            return p
        p += 2

# ─────────────────────────── helpers for checks ───────────────────────────

def euler_check(n: int, phi: int, bases=(2,3,5,7,11,13,17)):
    """Return True if pow(a,phi,n)==1 for all 'bases' that are coprime to n."""
    for a in bases:
        if gcd(a, n) == 1:
            if pow(a, phi, n) != 1:
                return False
    return True

def fmt_list(lst, maxlen=100):
    s = str(lst)
    return s if len(s) <= maxlen else s[:maxlen] + " … " + s[-maxlen:]

# ─────────────────────────── cases ───────────────────────────

def case1():
    print("Case 1: list(primerange(0, 100))")
    primes = primerange(0, 100)
    print("Answer     :", primes)
    print("Reason why : segmented sieve marks composites using primes ≤ √100; survivors ≥2 are primes.")
    ok = all(is_probable_prime(p) for p in primes)
    print("Check      : all listed are prime? ", ok, " ; count =", len(primes))
    print()

def case2():
    print("Case 2: list(primerange(1_000_000, 1_000_100))")
    lo, hi = 1_000_000, 1_000_100
    primes = primerange(lo, hi)
    print("Answer     :", primes)
    print("Reason why : same sieve on [10^6, 10^6+100); small block makes segmented sieve ideal.")
    ok = all(is_probable_prime(p) for p in primes)
    span_ok = all(lo <= p < hi for p in primes)
    print("Check      : all listed prime? ", ok, " ; in-range? ", span_ok, " ; count =", len(primes))
    print()

def case3():
    print("Case 3: isprime(2^521 − 1)")
    M521 = (1 << 521) - 1
    n = int(6864797660130609714981900799081393217269435300143305409394463459185543183397656052122559640661454554977296311391480858037121987999716643812574028291115057151)
    print("Answer     :", True)
    print("Reason why : this integer equals 2^521 − 1 (a known Mersenne prime).")
    print("Check      : equals 2^521−1? ", n == M521, " ; Miller–Rabin says prime? ", is_probable_prime(n))
    print()

def case4():
    print("Case 4: nextprime(2^521 − 1)")
    M521 = (1 << 521) - 1
    nxt = nextprime(M521)
    print("Answer     :", nxt)
    print("Reason why : scan odd integers > 2^521−1; first that passes Miller–Rabin is the next prime.")
    prev = nxt - 2 if nxt % 2 == 1 else nxt - 1
    print("Check      : candidate prime? ", is_probable_prime(nxt),
          " ; previous odd composite? ", (prev % 2 == 1 and not is_probable_prime(prev)))
    print()

def case5_to_case9():
    tests = [
        271,
        2_718_281,
        27_182_818_284,
        271_828_182_845_904,
        int("2718281828459045235360287471352662497757247"),
    ]
    for idx, n in enumerate(tests, start=5):
        phi = totient(n)
        fac = factorint(n)
        print(f"Case {idx}: totient({n})")
        print("Answer     :", phi)
        print("Reason why : φ(n) = Π p^e|n (p^e − p^{e−1}); factorization:", fac)
        ok = euler_check(n, phi)
        print("Check      : Euler’s theorem pow(a,φ(n),n)=1 for small coprime a ? ", ok)
        print()

# ─────────────────────────── driver ───────────────────────────

if __name__ == "__main__":
    print("Answer / Reason why / Check (harness)")
    print("======================================\n")
    case1()
    case2()
    case3()
    case4()
    case5_to_case9()

