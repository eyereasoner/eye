#!/usr/bin/env python3
# padovan.py  –  prints Padovan values *and* backward-style proofs
# ================================================================

from functools import lru_cache

# -------------------------------------------------------------------
# 1 ▸  Efficient numeric evaluator  (iterative O(n) and memoised O(1) reuse)
# -------------------------------------------------------------------
@lru_cache(maxsize=None)
def pad(n: int) -> int:
    """Padovan number with bases (0, 1, 1) and P(n)=P(n-2)+P(n-3)."""
    if n < 0:
        raise ValueError("Padovan is only defined for n ≥ 0")
    if n == 0:
        return 0
    if n in (1, 2):
        return 1

    # iterative loop avoids Python-recursion limits even for huge n
    p0, p1, p2 = 0, 1, 1           # holds P(i-3), P(i-2), P(i-1)
    for _ in range(3, n + 1):
        p0, p1, p2 = p1, p2, p0 + p1   # slide window and compute next
    return p2


# -------------------------------------------------------------------
# 2 ▸  Pretty backward-chaining proof printer
# -------------------------------------------------------------------
def prove_pad(n: int, indent=0, step=[0], summarise_above=20):
    """
    Print a backward proof that pad(n) = … down to the three facts
    pad(0)=0, pad(1)=1, pad(2)=1.

    `summarise_above` lets you keep very large traces short.
    """
    ind = "  " * indent
    step[0] += 1
    print(f"{ind}Step {step[0]:02d}: prove pad({n})")

    # ── base facts ────────────────────────────────────────────────
    if n == 0:
        print(f"{ind}  ✓ fact   (pad(0) = 0)")
        return
    if n == 1:
        print(f"{ind}  ✓ fact   (pad(1) = 1)")
        return
    if n == 2:
        print(f"{ind}  ✓ fact   (pad(2) = 1)")
        return

    # ── large n: print compactly ─────────────────────────────────
    if n > summarise_above:
        print(f"{ind}  → via recurrence  pad({n}) = pad({n-2}) + pad({n-3})")
        print(f"{ind}    (omitting sub-proofs for n > {summarise_above})")
        step[0] += 1; print(f"{ind}  Step {step[0]:02d}: assume pad({n-2}) proven")
        step[0] += 1; print(f"{ind}  Step {step[0]:02d}: assume pad({n-3}) proven")
        return

    # ── normal detailed expansion ────────────────────────────────
    print(f"{ind}  → via recurrence  pad({n}) = pad({n-2}) + pad({n-3})")
    prove_pad(n - 2, indent + 1, step, summarise_above)
    prove_pad(n - 3, indent + 1, step, summarise_above)


# -------------------------------------------------------------------
# 3 ▸  Demo – matches the original N3 query list
# -------------------------------------------------------------------
if __name__ == "__main__":
    cases = [1, 2, 3, 4, 5, 20, 91, 283, 3674]

    for n in cases:
        val = pad(n)
        print(f"\npadovan({n}) = {val}")
        print("=== Proof ===============================================")
        prove_pad(n)

