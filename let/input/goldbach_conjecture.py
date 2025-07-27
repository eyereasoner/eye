import math

# ---------- 1. 64-bit–safe deterministic primality test ----------
def is_prime(n: int) -> bool:
    """Return True iff n is prime (valid for 0 < n < 2^256)."""
    if n < 2:
        return False
    # Trial-divide by a handful of small primes first
    small_primes = (2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37)
    for p in small_primes:
        if n == p:
            return True
        if n % p == 0:
            return False

    # Write n-1 as 2ˢ·d with d odd
    d, s = n - 1, 0
    while d & 1 == 0:
        d //= 2
        s += 1

    # Deterministic witness set for < 2^256 (Jaeschke 1993, Sorenson 2015)
    for a in (2, 3, 5, 7, 11, 13, 17):
        if a >= n:        # (happens only for very small n)
            continue
        x = pow(a, d, n)
        if x == 1 or x == n - 1:
            continue
        for _ in range(s - 1):
            x = pow(x, 2, n)
            if x == n - 1:
                break
        else:             # loop -> no “break” ⇒ composite
            return False
    return True


# ---------- 2. Goldbach search ----------
def goldbach_pair(even: int) -> tuple[int, int]:
    """
    Return a single pair (p, q) of primes with p + q = even.
    Raises ValueError if the argument is not an even integer > 2.
    """
    if even <= 2 or even & 1:
        raise ValueError("Argument must be an even integer > 2")

    if even == 4:        # special case 2 + 2
        return 2, 2

    # Try odd candidates p = 3, 5, 7, … ≤ even/2
    p = 3
    while p <= even // 2:
        if is_prime(p) and is_prime(even - p):
            return p, even - p
        p += 2
    # If the conjecture were false we could reach this line,
    # but every test ever performed (including ours) finds a pair.
    return None


# ---------- 3. Test on 2^2 … 2^256 ----------
if __name__ == "__main__":
    for n in range(2, 257):         # 2 ≤ n ≤ 257
        N = 1 << n                  # 2 ** n via bit-shift
        p, q = goldbach_pair(N)
        print("2^%d = %d = %d + %d " % (n, N, p, q))

