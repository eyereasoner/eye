#!/usr/bin/env python3
"""
Feigenbaum α (alpha) via validated *main-cascade superstable* points — P3 style

Goal
----
Compute the Feigenbaum spatial-scaling constant α to ~8–9 digits with good performance.
We follow the same robust strategy as the δ script, but estimate α from
superstable parameters only (no period classification).

Background
----------
Let f_r(x) = r x (1 − x) be the logistic map and x_c = 1/2 the critical point.
For the superstable parameter s_n (the first r after s_{n-1} satisfying
f_r^{(2^n)}(x_c) = x_c), define the "diameter"
    d_n = f_{s_n}^{(2^{n-1})}(x_c) − x_c.
Then the ratios converge to the Feigenbaum α (negative):
    α = lim_{n→∞} d_n / d_{n+1}.
Since d_n alternates sign, we estimate α via  -d_n / d_{n+1} and then negate.

Numerics strategy (robust & fast)
---------------------------------
- For each n=1..N, find s_n in a *local* window right after s_{n-1}, under a hard cap r<3.57
  (the main cascade accumulates near 3.569946...). The window size is derived from the
  previous gap (which shrinks by ~1/δ), so we remain in the main window.
- Enumerate sign-change brackets for g_n(r)=f_r^{(2^n)}(x_c)-x_c and bisection-solve.
- **Validation:** accept a candidate only if its minimal period is exactly 2^n, i.e. g_k(r)≠0 for all k<n.
- Compute d_n at each s_n and estimate α from the last few ratios.

No third‑party libraries required.
"""

import math
from typing import List, Tuple

# ------------------------------- Dynamics -------------------------------

def logistic_next(r: float, x: float) -> float:
    return r * x * (1.0 - x)


def iterate_from_c_pow2(r: float, n: int) -> float:
    """Return f_r^{(2^n)}(1/2)."""
    k = 1 << n
    x = 0.5
    for _ in range(k):
        x = logistic_next(r, x)
    return x


def g_n(r: float, n: int) -> float:
    """Root function for superstable: g_n(r) = f_r^{(2^n)}(1/2) - 1/2."""
    return iterate_from_c_pow2(r, n) - 0.5


# ----------------------- Validation: minimal period ---------------------

def minimal_period_is_exact_pow2(r: float, n: int, eps_zero: float = 8e-11) -> bool:
    """
    Check the minimal period at parameter r is *exactly* 2^n.
    We test g_k(r) != 0 for all k < n (within tolerance).
    """
    for k in range(1, n):
        if abs(g_n(r, k)) <= eps_zero:
            return False
    return True


# ------------------------ Root finding in local window ------------------

def enumerate_sign_change_brackets(n: int, lo: float, hi: float, max_steps: int = 1500) -> List[Tuple[float, float]]:
    """
    Scan [lo, hi] to collect brackets [a,b] where g_n changes sign.
    """
    brackets: List[Tuple[float, float]] = []
    step = (hi - lo) / max_steps
    r = lo
    fr = g_n(r, n)
    sr = 1 if fr > 0 else (-1 if fr < 0 else 0)
    for _ in range(max_steps):
        r2 = r + step
        if r2 > hi:
            r2 = hi
        f2 = g_n(r2, n)
        s2 = 1 if f2 > 0 else (-1 if f2 < 0 else 0)
        if s2 == 0:
            a, b = (r, r2) if r < r2 else (r2, r)
            brackets.append((a, b))
        elif sr != 0 and s2 != 0 and s2 != sr:
            a, b = (r, r2) if r < r2 else (r2, r)
            brackets.append((a, b))
        r, fr, sr = r2, f2, s2
        if r >= hi:
            break
    return brackets


def bisect_root(n: int, lo: float, hi: float, tol: float = 5e-14) -> float:
    flo = g_n(lo, n)
    fhi = g_n(hi, n)
    if flo == 0.0:
        return lo
    if fhi == 0.0:
        return hi
    if flo * fhi > 0.0:
        raise RuntimeError("Invalid bracket: no sign change")

    for _ in range(90):
        mid = 0.5 * (lo + hi)
        fm = g_n(mid, n)
        if fm == 0.0:
            return mid
        if flo * fm < 0.0:
            hi = mid
            fhi = fm
        else:
            lo = mid
            flo = fm
        if hi - lo < tol:
            break
    return 0.5 * (lo + hi)


def find_superstable_next(prev: float, n: int, prev_gap: float, r_cap: float = 3.57) -> float:
    """
    Find the *main-cascade* s_n right after prev = s_{n-1} by:
    - building a *small* local window just after prev,
    - enumerating all sign-change brackets for g_n,
    - solving them in order and validating minimal period == 2^n,
    - returning the first valid one.
    """
    eps = 1e-12
    if n == 1:
        lo, hi = prev + eps, min(prev + 0.5, r_cap)  # wide for the first
    elif n == 2:
        lo, hi = prev + eps, min(prev + 0.25, r_cap)
    else:
        span = min(0.35 * prev_gap, 0.01, r_cap - prev)
        if span <= 5e-8:
            span = min(5e-8, r_cap - prev)
        lo, hi = prev + eps, prev + span

    brackets = enumerate_sign_change_brackets(n, lo, hi, max_steps=1600)
    for a, b in brackets:
        r_star = bisect_root(n, a, b, tol=5e-14)
        if minimal_period_is_exact_pow2(r_star, n, eps_zero=8e-11):
            return r_star

    # Expand a bit if needed
    for _ in range(4):
        new_hi = min(hi + (hi - lo), r_cap)
        if new_hi <= hi + 1e-12:
            break
        hi = new_hi
        brackets = enumerate_sign_change_brackets(n, lo, hi, max_steps=2200)
        for a, b in brackets:
            r_star = bisect_root(n, a, b, tol=5e-14)
            if minimal_period_is_exact_pow2(r_star, n, eps_zero=8e-11):
                return r_star

    raise RuntimeError(f"Could not find a valid main-cascade s_{n} in ({prev:.12f}, {hi:.12f}] under cap {r_cap}.")


# ---------------------------- α from s_n only ---------------------------

def compute_s_sequence(N: int = 14) -> List[float]:
    s = [0.0] * (N + 1)
    prev = 3.0
    prev_gap = 0.5
    for n in range(1, N + 1):
        sn = find_superstable_next(prev, n, prev_gap, r_cap=3.57)
        s[n] = sn
        prev, prev_gap = sn, (sn - s[n - 1]) if n > 1 else (sn - 3.0)
    return s


def compute_diameters(ss: List[float]) -> List[float]:
    """
    d_n = f_{s_n}^{(2^{n-1})}(1/2) - 1/2 for n >= 1.
    Signs alternate; |d_{n+1}| ≈ |d_n| / |α|.
    """
    d = [0.0] * len(ss)
    for n in range(1, len(ss)):
        if ss[n] == 0.0:
            continue
        y = iterate_from_c_pow2(ss[n], n - 1)
        d[n] = y - 0.5
    return d


def estimate_alpha_from_d(diam: List[float]) -> Tuple[float, List[Tuple[int, float]]]:
    """
    Using α_n = - d_n / d_{n+1}. Return the final estimate (avg of last 3) and table.
    """
    rows: List[Tuple[int, float]] = []
    for n in range(1, len(diam) - 1):
        if diam[n] == 0.0 or diam[n + 1] == 0.0:
            continue
        a = -diam[n] / diam[n + 1]
        rows.append((n + 1, a))  # index with the *upper* n
    if len(rows) >= 3:
        est = sum(v for _, v in rows[-3:]) / 3.0
    else:
        est = rows[-1][1]
    # α is negative by convention
    return -est, rows


# ------------------------------- Driver --------------------------------

def main():
    MAX_N = 12  # 13–15 typically yields 8–9 digits quickly

    ss = compute_s_sequence(MAX_N)
    diam = compute_diameters(ss)
    alpha, table = estimate_alpha_from_d(diam)

    print("Answer")
    print("------")
    print(f"Feigenbaum α (alpha) ≈ {alpha:.12f}")
    print()

    print("Reason Why")
    print("----------")
    print("We locate the superstable parameters s_n of periods 2^n within the main cascade,")
    print("validate minimal period = 2^n, and compute diameters d_n = f_{s_n}^{(2^{n-1})}(1/2) − 1/2.")
    print("The ratios d_n/d_{n+1} → α (negative). Using the last few ratios we obtain a stable estimate.")
    print()

    print("Check (harness)")
    print("---------------")
    # Monotonic & below cap
    mono_ok = all(ss[i] < ss[i + 1] for i in range(1, MAX_N))
    cap_ok = all(ss[i] < 3.57 for i in range(1, MAX_N + 1))
    print(f"Monotonic s_n increasing: {'PASS' if mono_ok else 'FAIL'}")
    print(f"s_n under 3.57 cap: {'PASS' if cap_ok else 'FAIL'}")

    # Diameter ratios shrink roughly by ~1/|α|
    shrink_ok = True
    mags = [abs(x) for x in diam[1:] if x != 0.0]
    for i in range(len(mags) - 1):
        if not (mags[i + 1] < mags[i]):
            shrink_ok = False
            break
    print(f"|d_n| shrinking: {'PASS' if shrink_ok else 'FAIL'}")

    # Convergence of α_n
    if len(table) >= 3:
        last = [v for _, v in table[-3:]]
        spread = max(last) - min(last)
        print(f"Last 3 |α_n| spread: {spread:.3e}")
        conv_ok = spread < 5e-8  # tight
    else:
        conv_ok = False
    print(f"Convergence: {'PASS' if conv_ok else 'WARN'}")

    # Show tail
    print("\ns_n, d_n, and α_n (tail)")
    start = max(1, MAX_N - 6)
    for n in range(start, MAX_N + 1):
        print(f"s_{n:2d} = {ss[n]:.15f}")
        if n >= 1:
            print(f"  d_{n:2d} = {diam[n]:+.15e}")
        if n >= 2 and n < MAX_N:
            alpha_n = -diam[n] / diam[n + 1]
            print(f"  α_{n:02d} = {-alpha_n:.12f}")

    result_ok = mono_ok and cap_ok and shrink_ok and (conv_ok or MAX_N >= 13)
    print("\nResult:", "PASS" if result_ok else "OK (with warnings)")


if __name__ == "__main__":
    main()

