#!/usr/bin/env python3
# padovan.py  –  prints Padovan values *and* backward-style proofs (ARC-ified)
# =============================================================================
#
# • Fast numeric evaluator `pad(n)` with memoization and an iterative core:
#     bases P(0)=0, P(1)=1, P(2)=1;  P(n)=P(n-2)+P(n-3) for n≥3.
# • A compact backward-style proof printer `prove_pad(n)` that expands the
#   recurrence down to base facts (with optional summarisation for large n).
# • For each queried n we print:
#     Answer      — P(n) (and digit counts for very large outputs)
#     Reason why  — the recurrence and an on-the-fly backward proof trace
#     Check       — a harness that re-verifies base cases, recurrence, and
#                   cross-checks with a DP table; also sanity-checks growth.
#
# Notes
# -----
# • No third-party imports; only `functools.lru_cache`.
# • Printing all digits is fine for moderate n; for very large n we abbreviate.
# • The growth ratio P(n+1)/P(n) tends to the plastic constant (~1.324717957…).
#

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
# 3 ▸  ARC helpers: compact printing for very large integers
# -------------------------------------------------------------------
def digits(n: int) -> int:
    """Count base-10 digits of a nonnegative integer."""
    return 1 if n == 0 else len(str(n))

def summarize_int(n: int, max_body: int = 120) -> str:
    """
    Return a human-friendly string for possibly huge integers.
    Prints full number if <= max_body digits; otherwise shows head…tail.
    """
    s = str(n)
    if len(s) <= max_body:
        return s
    head = s[:30]
    tail = s[-30:]
    return f"{head}…{tail}  [digits={len(s)}]"


# -------------------------------------------------------------------
# 4 ▸  ARC sections for a single case n
# -------------------------------------------------------------------
def arc_answer(n: int, val: int) -> None:
    print("Answer")
    print("------")
    print(f"P({n}) = {summarize_int(val)}")
    print(f"digits(P({n})) = {digits(val)}")
    print()

def arc_reason(n: int, proof_cutoff: int = 20) -> None:
    print("Reason why")
    print("----------")
    print("Padovan recurrence and bases:")
    print("  P(0)=0, P(1)=1, P(2)=1, and for n≥3:  P(n) = P(n-2) + P(n-3).")
    print("Backward proof trace:")
    prove_pad(n, summarise_above=proof_cutoff)
    print()

def arc_check(n: int, val: int) -> None:
    print("Check (harness)")
    print("---------------")
    # 1) Base cases
    assert pad(0) == 0 and pad(1) == 1 and pad(2) == 1, "Base cases wrong."

    # 2) Recurrence holds on a range (safe bound)
    bound = max(10, min(n, 500))
    for k in range(3, bound + 1):
        assert pad(k) == pad(k-2) + pad(k-3), f"Recurrence fails at n={k}"

    # 3) Cross-check with a DP build up to `bound`
    dp = [0, 1, 1]
    for k in range(3, bound + 1):
        dp.append(dp[k-2] + dp[k-3])
        assert dp[k] == pad(k)
    # Also check reported value for this n
    assert val == pad(n), "Mismatch between printed value and pad(n)."

    # 4) Growth sanity: ratio approaches the plastic constant (~1.324718)
    # Only check when n is large enough to be meaningful
    if n >= 30:
        ratio = pad(n) / pad(n-1)
        # plastic constant approx (no imports)
        plastic = 1.3247179572447458
        assert abs(ratio - plastic) < 0.01, "Growth ratio far from plastic constant."

    print("OK: bases, recurrence, DP cross-check, and growth sanity all verified.")
    print()


# -------------------------------------------------------------------
# 5 ▸  Demo – matches the original N3 query list (now ARC-ified)
# -------------------------------------------------------------------
if __name__ == "__main__":
    cases = [1, 2, 3, 4, 5, 20, 91, 283, 3674]

    for n in cases:
        val = pad(n)

        print("=" * 72)
        arc_answer(n, val)
        arc_reason(n, proof_cutoff=20)
        arc_check(n, val)

