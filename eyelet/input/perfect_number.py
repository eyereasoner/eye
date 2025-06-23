'''
perfect_number.py

Conversion of the provided Prolog perfect-number generator into Python.
See https://en.wikipedia.org/wiki/Perfect_number for reference.
'''

import sys
sys.setrecursionlimit(10000)


def divisible(x: int, y: int) -> bool:
    """
    Mimics Prolog:
      divisible(X, Y) :-
        N is Y*Y,
        N =< X,
        X mod Y =:= 0.
      divisible(X, Y) :-
        Y < X,
        Y1 is Y + 1,
        divisible(X, Y1).
    Returns True if any divisor of x is found starting at y.
    """
    if y * y <= x and x % y == 0:
        return True
    if y < x:
        return divisible(x, y + 1)
    return False


def isprime_list(xs: list[int]) -> list[int]:
    """
    Mimics Prolog findall(L, isprime(R, L), P):
    Selects all X in xs where X > 1 and not divisible(X, 2).
    """
    primes = []
    for x in xs:
        if x > 1 and not divisible(x, 2):
            primes.append(x)
    return primes


def power(n: int, k: int) -> int:
    """
    Recursive power: power(N, 0) = 1; power(N, K) = N * power(N, K-1).
    """
    if k == 0:
        return 1
    return n * power(n, k - 1)


def calc(k: int) -> int:
    """
    Prolog: calc(2, K, R) :-
      power(2, K, X),
      R1 is X-1,
      power(2, K-1, R2),
      R is R1 * R2.
    Computes 2^(p-1) * (2^p - 1).
    """
    x = power(2, k)
    r1 = x - 1
    r2 = power(2, k - 1)
    return r1 * r2


def generate_list(n: int) -> list[int]:
    """
    Prolog: generateList(0, []).
    generateList(N, [X|Xs]) :-
      N > 0,
      X is N+1,
      N1 is N-1,
      generateList(N1, Xs).
    Builds [n+1, n, ..., 2].
    """
    if n <= 0:
        return []
    return [n + 1] + generate_list(n - 1)


def perfect(n: int) -> list[int]:
    """
    Prolog: perfect(N, C) :-
      generateList(N, R),
      findall(L, isprime(R, L), P),
      listperf(P, C).
    Generates perfect numbers for each prime exponent up to N+1.
    """
    # Step 1: generate list of candidate exponents
    exponents = generate_list(n)
    # Step 2: filter primes
    primes = isprime_list(exponents)
    # Step 3: compute perfect numbers for each prime
    return [calc(p) for p in primes]


if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(
        description='Generate perfect numbers based on converted Prolog logic.'
    )
    parser.add_argument(
        'N', nargs='?', type=int, default=2025,
        help='Generate list size N (will test exponents up to N+1). Defaults to 2025 if omitted.'
    )
    args = parser.parse_args()

    results = perfect(args.N)
    for num in results:
        print(num)

