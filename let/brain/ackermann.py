#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Ackermann A₂ via hyper-operations — ARC (Answer / Reason / Check), self-contained

Definition (same as the original script):
  A₂(x, y) = H(x, y+3, 2) − 3

Hyper-operation H implemented without deep recursion:
  H(0, y, z) = y + 1                 (successor)
  H(1, y, z) = y + z                 (addition)
  H(2, y, z) = y · z                 (multiplication)
  H(x≥3, 0, z) = 1                   (neutral)
  H(x≥3, y>0, z) = repeat y times: result = H(x−1, result, z), start result=1
    └─ For x=3 this yields exponentiation: H(3, y, z) = z^y
       For x=4 this yields tetration:      H(4, y, z) = z^^y (power-tower of height y)

This mirrors the original EYE translation and prints the same 12 demo queries.
"""

from typing import List, Tuple

# Let very large integers print without artificial limits (Python 3.11+)
# If that's not available (older Python), we just skip it.
try:
    import sys
    sys.set_int_max_str_digits(0)  # or e.g., 50_000
except Exception:
    pass

# ───────────────────────────────── Hyper-operations ──────────────────────────
def hyper(x: int, y: int, z: int) -> int:
    """Iterative H to avoid deep Python recursion; recursion depth is at most x."""
    if x == 0:                 # successor
        return y + 1
    if x == 1:                 # addition
        return y + z
    if x == 2:                 # multiplication
        return y * z
    if y == 0:                 # identity element for x ≥ 3
        return 1
    # For x ≥ 3 and y > 0: loop in y, recurse only on x-1
    result = 1
    for _ in range(y):
        result = hyper(x - 1, result, z)
    return result

def ackermann2(x: int, y: int) -> int:
    return hyper(x, y + 3, 2) - 3

# The exact 12 queries from the original script
QUERIES: List[Tuple[int, int]] = [
    (0, 0), (0, 6),
    (1, 2), (1, 7),
    (2, 2), (2, 9),
    (3, 4), (3, 14),
    (4, 0), (4, 1), (4, 2),
    (5, 0),
]

# ───────────────────────────────────── Answer ────────────────────────────────
def print_answer():
    print("Answer")
    print("======")
    print("Definition:  A₂(x, y) = H(x, y+3, 2) − 3\n")
    print("Values for the 12 demo queries:")
    for idx, (x, y) in enumerate(QUERIES):
        v = ackermann2(x, y)
        # Show digit count too, since some are huge (e.g., A(4,2) has 19,729 digits)
        d = len(str(v))
        print(f"A{idx:02} = A₂({x},{y}) = {v}")
        print(f"      digits: {d}")
    print()

# ────────────────────────────────── Reason why ───────────────────────────────
def print_reason():
    print("\nReason why")
    print("==========")
    print("We compute A₂ by reducing it to the hyper-operations H:")
    print("  A₂(x, y) = H(x, y+3, 2) − 3.")
    print("For x ≤ 2, H coincides with successor/addition/multiplication, giving the closed forms:")
    print("  A₂(0, y) = y + 1")
    print("  A₂(1, y) = y + 2")
    print("  A₂(2, y) = 2y + 3")
    print("For x = 3, H(3, y, 2) = 2^y, so:")
    print("  A₂(3, y) = 2^(y+3) − 3")
    print("For x = 4, H(4, y, 2) is tetration (power towers of 2).")
    print("The implementation uses a loop on y for x ≥ 3 (and only recurses on x−1),")
    print("so we avoid deep call stacks while matching the mathematical definition.")

# ───────────────────────────────── Check (harness) ───────────────────────────
def print_check():
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    # 1) Closed forms for small x
    ok = all(ackermann2(0, y) == y + 1 for y in range(0, 51))
    print(f"A₂(0, y) = y + 1  on y∈[0,50]? {ok}"); ok_all &= ok

    ok = all(ackermann2(1, y) == y + 2 for y in range(0, 51))
    print(f"A₂(1, y) = y + 2  on y∈[0,50]? {ok}"); ok_all &= ok

    ok = all(ackermann2(2, y) == 2*y + 3 for y in range(0, 1001))
    print(f"A₂(2, y) = 2y + 3 on y∈[0,1000]? {ok}"); ok_all &= ok

    ok = all(ackermann2(3, y) == (1 << (y + 3)) - 3 for y in range(0, 21))
    print(f"A₂(3, y) = 2^(y+3) − 3 on y∈[0,20]? {ok}"); ok_all &= ok

    # 2) Standard two-argument Ackermann recurrences (small grid to stay fast)
    #    A(m+1, 0) = A(m, 1)
    #    A(m+1, n+1) = A(m, A(m+1, n))
    def A(m, n): return ackermann2(m, n)
    ok = True
    for m in range(0, 3):
        ok &= (A(m+1, 0) == A(m, 1))
        for n in range(0, 4):
            ok &= (A(m+1, n+1) == A(m, A(m+1, n)))
    print(f"Recurrences hold on m∈[0,2], n∈[0,3]? {ok}"); ok_all &= ok

    # 3) Query spot-checks (exact where small, structural where huge)
    expected = {
        (0,0): 1, (0,6): 7,
        (1,2): 4, (1,7): 9,
        (2,2): 7, (2,9): 21,
        (3,4): 125, (3,14): 131072 - 3,
        (4,0): 13, (4,1): 65533, (5,0): 65533,
    }
    ok = True
    for (x, y), want in expected.items():
        got = ackermann2(x, y)
        if got != want:
            ok = False
            print(f"  MISMATCH at A₂({x},{y}): got {got}, want {want}")
    print(f"Exact checks for 11/12 queries OK? {ok}"); ok_all &= ok

    # The remaining big one: A₂(4,2) = 2^^5 − 3 = 2^(65536) − 3.
    v = ackermann2(4, 2)
    is_pow2 = (v + 3) & (v + 2) == 0  # power-of-two test
    bitlen  = (v + 3).bit_length()    # should be 65537 bits for 2^65536
    digits  = len(str(v))             # should be 19,729 decimal digits
    ok = (is_pow2 and bitlen == 65537 and digits == 19729)
    print(f"A₂(4,2)+3 is a power of two, with bit_length=65537 and 19,729 digits? {ok}")
    ok_all &= ok

    # 4) Monotonicity in y for small x (sanity)
    ok = True
    for x in range(0, 4):
        prev = None
        for y in range(0, 11):
            val = ackermann2(x, y)
            if prev is not None and not (val > prev):
                ok = False
                break
            prev = val
    print(f"Monotone in y for x∈[0,3], y∈[0,10]? {ok}"); ok_all &= ok

    print(f"\nAll checks passed? {ok_all}")

# ───────────────────────────────────── Main ────────────────────────────────
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

