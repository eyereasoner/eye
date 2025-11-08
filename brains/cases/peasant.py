#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
peasant.py — Ancient Egyptian (peasant) multiply + power, ARC-ified
────────────────────────────────────────────────────────────────────────

Description
-----------
Two classic binary algorithms, implemented without using Python’s `*` inside
the core loops:

1) peasant_multiply(a, b)
   • Handles signs.
   • Repeatedly doubles `a` and halves `b`, accumulating `a` into the result
     whenever the current least-significant bit of `b` is 1.
   • Time ~ O(log |b|) additions of (growing) integers.

2) peasant_power(base, exponent)
   • Square-and-multiply (binary exponentiation), but uses `peasant_multiply`
     for *every* multiplication.
   • Time ~ O(log exponent) multiplies; each multiply ~ O(log |b|) additions.

ARC output
----------
• Answer — demos (including a worked 18×23 table) and quick stats.
• Reason why — explains the method(s) and bit-level invariants.
• Check (harness) — verifies equivalence to Python’s `a*b` and `pow`,
  edge cases, sign rules, and a few algebraic properties.
"""

from __future__ import annotations
import sys
from typing import List, Tuple

# Allow printing very large integers if Python 3.11+ (safe to noop earlier)
if hasattr(sys, 'set_int_max_str_digits'):
    sys.set_int_max_str_digits(10_000_000)

# ─────────────────────────────────────────────────────────────
# Core algorithms (no Python '*' inside the loops)
# ─────────────────────────────────────────────────────────────
def peasant_multiply(a: int, b: int) -> int:
    """Ancient Egyptian (peasant) multiplication with sign handling."""
    # Handle sign, work with non-negative a,b
    sign = -1 if (a < 0) ^ (b < 0) else 1
    a, b = abs(a), abs(b)
    result = 0
    while b > 0:
        if b & 1:
            result += a        # accumulate
        a <<= 1               # double a
        b >>= 1               # halve b
    return sign * result

def peasant_power(base: int, exponent: int) -> int:
    """Binary exponentiation using peasant_multiply for every product."""
    if exponent < 0:
        raise ValueError("Exponent must be non-negative")
    result = 1
    b = base
    e = exponent
    while e > 0:
        if e & 1:
            result = peasant_multiply(result, b)
        b = peasant_multiply(b, b)
        e >>= 1
    return result

# ─────────────────────────────────────────────────────────────
# Instrumented helpers for demos & “Reason why”
# ─────────────────────────────────────────────────────────────
def peasant_trace(a: int, b: int) -> Tuple[int, List[Tuple[int,int,int,int]]]:
    """
    Return product and a trace table for |a|×|b| with rows:
      (row_idx, a, b, added_flag)  — sign ignored in the table for clarity.
    """
    sign = -1 if (a < 0) ^ (b < 0) else 1
    a, b = abs(a), abs(b)
    res = 0
    rows: List[Tuple[int,int,int,int]] = []
    i = 1
    while b > 0:
        add = 1 if (b & 1) else 0
        rows.append((i, a, b, add))
        if add:
            res += a
        a <<= 1
        b >>= 1
        i += 1
    return sign * res, rows

def power_counts(base: int, exponent: int) -> Tuple[int, int, int]:
    """
    Run peasant_power(base, exponent) and return:
      (value, multiply_calls, square_calls) — both counted as peasant_multiply uses.
    """
    if exponent < 0:
        raise ValueError("Exponent must be non-negative")
    result = 1
    b = base
    e = exponent
    mults = squares = 0
    while e > 0:
        if e & 1:
            result = peasant_multiply(result, b); mults += 1
        b = peasant_multiply(b, b); squares += 1
        e >>= 1
    return result, mults, squares

# ─────────────────────────────────────────────────────────────
# ARC — Answer
# ─────────────────────────────────────────────────────────────
def arc_answer() -> None:
    print("Answer")
    print("------")

    # Demo 1: Worked 18 × 23
    prod, table = peasant_trace(18, 23)
    print("Worked example: 18 × 23 using halving/doubling")
    print(" row |     a | b (bin) | add?")
    for i, a, b, add in table:
        print(f"{i:>4} | {a:>5} | {b:>7b} | {'✓' if add else ' '}")
    print(f"Result: {prod}\n")

    # Demo 2: a few multiplications (including signs & zero)
    pairs = [(3,0), (5,6), (238,13), (8367238,27133), (-12345,6789), (-222,-333)]
    for a,b in pairs:
        print(f"{a} × {b} = {peasant_multiply(a,b)}")
    print()

    # Demo 3: powers
    pow_cases = [(3,0), (5,6), (12,7)]
    for base, exp in pow_cases:
        val, muls, sqs = power_counts(base, exp)
        print(f"{base}^{exp} = {val}   (multiplies={muls}, squares={sqs})")
    print()

# ─────────────────────────────────────────────────────────────
# ARC — Reason why
# ─────────────────────────────────────────────────────────────
def arc_reason() -> None:
    print("Reason why")
    print("----------")
    print("Peasant multiply (a × b):")
    print("  • Write b in binary. For each bit from LSB to MSB:")
    print("      - if bit is 1, add current a to the result,")
    print("      - double a, halve b and drop the LSB.")
    print("  • Invariant: after k iterations, result equals the sum of those")
    print("    doubled a’s whose corresponding b bits were 1. That sum is a×b.")
    print()
    print("Binary exponentiation (base^exp):")
    print("  • Maintain result=1, base b, exponent e.")
    print("  • For each bit of e:")
    print("      - if bit is 1: result ← result×b")
    print("      - b ← b×b (square), e >>= 1")
    print("  • Uses O(log e) multiplications; here, each multiplication is itself")
    print("    implemented by the peasant method (no built-in ‘*’ inside loops).")
    print()

# ─────────────────────────────────────────────────────────────
# ARC — Check (harness)
# ─────────────────────────────────────────────────────────────
def arc_check() -> None:
    print("Check (harness)")
    print("---------------")

    # 1) Multiplication correctness against Python’s exact integers
    tests_mul = [
        (0,0), (0,5), (5,0), (1,999999), (-1,999999),
        (7, 13), (238,13), (123456, 654321),
        (-12345, 6789), (222, -333), (-222, -333),
    ]
    for a,b in tests_mul:
        assert peasant_multiply(a,b) == a*b, f"multiply mismatch for {a}×{b}"

    # 2) Commutativity & sign rules
    for a,b in [(2,3), (-2,3), (2,-3), (-2,-3), (123456, -78901)]:
        p1 = peasant_multiply(a,b); p2 = peasant_multiply(b,a)
        assert p1 == p2 == a*b
    assert peasant_multiply(0, 1234567) == 0
    assert peasant_multiply(1, -42) == -42
    assert peasant_multiply(-1, 42) == -42

    # 3) Exponentiation equivalence with Python’s pow (for non-negative exponents)
    tests_pow = [
        (3,0), (3,1), (5,6), (2,20), (-2,11), (12,7)
    ]
    for base, exp in tests_pow:
        assert peasant_power(base, exp) == pow(base, exp), f"power mismatch for {base}^{exp}"

    # 4) Exponent negative should raise
    try:
        peasant_power(2, -1)
        raise AssertionError("negative exponent did not raise")
    except ValueError:
        pass

    print("OK: multiplication/power equal to Python's math, signs/edge cases verified.\n")

# ─────────────────────────────────────────────────────────────
# CLI / demo
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    arc_answer()
    arc_reason()
    arc_check()

