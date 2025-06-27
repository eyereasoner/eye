#!/usr/bin/env python
"""
Euclid’s Infinitude-of-Primes Demo  ─ using SymPy
-------------------------------------------------
Given any finite list of primes, multiply them, add 1, and
discover a prime you didn’t have before.  Repeat forever!

Requirements
------------
    pip install sympy

Usage
-----
    python euclid.py           # defaults: 5 start primes, 5 rounds
    python euclid.py -k 10 -r 6
"""
from math import prod
import argparse
from sympy import factorint, nextprime, isprime


# ─────────────────────────────────────────────────────────────── helper utils ──
def first_k_primes(k: int) -> list[int]:
    """Return the first k primes using SymPy’s nextprime()."""
    primes = []
    p = 2
    while len(primes) < k:
        primes.append(p)
        p = nextprime(p)
    return primes


def euclid_rounds(k_start: int = 5, rounds: int = 5) -> None:
    """Run Euclid’s construction for the requested number of rounds."""
    primes: list[int] = first_k_primes(k_start)
    print(f"Starting with the first {k_start} primes:\n  {primes}\n")

    for r in range(1, rounds + 1):
        candidate = prod(primes) + 1
        factors = factorint(candidate)            # {prime: exponent}
        new_primes = [q for q in factors if q not in primes]

        print(f"── Round {r} ─────────────────────────────────────────────")
        print(f"candidate = product(primes) + 1 = {candidate:,}")
        print(f"status    = {'prime' if isprime(candidate) else 'composite'}")
        print(f"factors   = {factors}")

        if new_primes:
            print(f"new prime{'s' if len(new_primes) > 1 else ''} found: {new_primes}")
        else:     # candidate itself is a new prime
            new_primes = [candidate]
            print("candidate itself is a new prime!")

        primes.extend(new_primes)
        primes = sorted(set(primes))
        print(f"total distinct primes so far: {len(primes)}\n")

    print("Final prime list after all rounds:")
    print(primes)


# ────────────────────────────────────────────────────────────────── CLI driver ─
if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Euclid’s infinitude-of-primes demonstration using SymPy."
    )
    parser.add_argument(
        "-k", "--kstart",
        type=int, default=5,
        help="How many primes to start with (default: 5)."
    )
    parser.add_argument(
        "-r", "--rounds",
        type=int, default=5,
        help="How many Euclid rounds to perform (default: 5)."
    )
    args = parser.parse_args()
    euclid_rounds(k_start=args.kstart, rounds=args.rounds)
