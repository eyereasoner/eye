"""
Quartic (any degree) complex root solver with "explain-and-check"
=================================================================

What this file does
-------------------
• Solves the two target equations:
    (P1)  x^4 - 10x^3 + 35x^2 - 50x + 24 = 0
    (P2)  x^4 + (-9 - 5i)x^3 + (14 + 33i)x^2 + (24 - 44i)x - 26 = 0
• Prints an "explain-and-check" for each: roots, tiny residuals, Vieta checks,
  and a rebuild-from-roots coefficient comparison (a small proof harness).
• Uses only built-in Python (no imports). Complex arithmetic is native (use `j`).

Algorithm (Durand–Kerner / Weierstrass)
---------------------------------------
Given a polynomial P of degree n (normalized to monic), we iterate all roots
simultaneously:
    x_k^(m+1) = x_k^(m) - P(x_k^(m)) / ∏_{j≠k} (x_k^(m) - x_j^(m))
Key facts:
• The Fundamental Theorem of Algebra guarantees n complex roots (with multiplicity).
• For *simple* roots (no multiplicities), Durand–Kerner converges quadratically
  from generic distinct starting points. We use a deterministic "spiral" seed
  (1, b, b^2, …) to break symmetry and ensure reproducibility.
• We normalize the polynomial to monic before iteration (this preserves the
  roots), which improves numerical behavior and keeps the update formula tidy.

Why the method is correct (sketch)
----------------------------------
• Roots are fixed points of the Durand–Kerner map; near a simple root the update
  is a simultaneous Newton-like step, so convergence is locally quadratic.
• Distinct initial guesses avoid denominator zero; we also add a tiny, *deterministic*
  perturbation if a pair collides, which prevents stagnation in degenerate cases.
• Termination: we stop when all root updates fall below `tol` (default 1e-14),
  or after a hard cap on iterations.

Proof harness (post-solve checks)
---------------------------------
1) Residuals: compute |P(r_k)| for all roots r_k — these should be ~ machine epsilon.
2) Vieta checks (for monic form): compare the computed elementary symmetric sums
   of the roots with the polynomial coefficients:
     sum r_i = -a3,   sum_{i<j} r_ir_j = a2,
     sum_{i<j<k} r_ir_jr_k = -a1,   ∏ r_i = a0.
3) Rebuild: form ∏(x - r_k) from the numeric roots, scale by the original leading
   coefficient, and compare coefficients with the original polynomial.

"Reason why" the two given polynomials factor as claimed
--------------------------------------------------------
• P1:  (x-1)(x-2)(x-3)(x-4).
  Pair symmetrically: (x-1)(x-4) = x^2 - 5x + 4,  (x-2)(x-3) = x^2 - 5x + 6.
  Let y = x^2 - 5x. Then (y+4)(y+6) = y^2 + 10y + 24
  => x^4 - 10x^3 + 35x^2 - 50x + 24. Hence roots 1,2,3,4.

• P2:  (x - i)(x - (1+i))(x - (3+2i))(x - (5+i)).
  The symmetric sums of {i, 1+i, 3+2i, 5+i} are:
     sum = 9 + 5i   => a3 = -(9+5i)
     pairwise sum = 14 + 33i   => a2 = 14 + 33i
     triple sum = 24 - 44i     => a1 = -(24 - 44i)
     product = -26             => a0 = -26
  These match the coefficients of P2 exactly.

Numerical notes & limits
------------------------
• Tolerance: `tol=1e-14` (you can relax to 1e-12 for noisier inputs).
• Sorting: printed order is deterministic (by rounded Re, then Im).
• Near-integer formatting: a pretty-printer snaps values with |δ|≤1e-12 to
  the closest integer, and hides ±0i.
• Multiple roots: Durand–Kerner slows to linear convergence near multiplicities.
  It still works here because both polynomials have simple roots.
• Complexity: each iteration is O(n^2); memory is O(n).

How to extend
-------------
• Replace `p1` and `p2` with any coefficient lists (descending powers).
• Leading coefficient may be anything; the solver normalizes internally and then
  rescales during the rebuild check.

"""

# --------------------------- Implementation ----------------------------------

def poly_eval(coeffs, x):
    v = 0j
    for c in coeffs:
        v = v * x + c
    return v

def durand_kerner(coeffs, max_iter=2000, tol=1e-14):
    """
    Durand–Kerner (Weierstrass) root finder for polynomials with complex coeffs.
    coeffs: list of complex in DESCENDING powers. Leading coeff may be != 1.
    Returns: (roots, iterations)
    """
    n = len(coeffs) - 1
    if n <= 0:
        return [], 0

    # Normalize to monic
    lc = coeffs[0]
    c = [cc / lc for cc in coeffs]

    # Deterministic distinct initial guesses on a small spiral
    base = 0.4 + 0.9j
    roots = [base**k for k in range(n)]

    for it in range(1, max_iter + 1):
        done = True
        new_roots = roots[:]
        for k in range(n):
            xk = roots[k]
            px = poly_eval(c, xk)
            denom = 1+0j
            for j in range(n):
                if j == k:
                    continue
                diff = xk - roots[j]
                if diff == 0:
                    # Deterministic micro-perturbation if two guesses coincide
                    diff = (1e-12*(k+1)) + (1e-12j*(j+1))
                denom *= diff
            if denom == 0:
                denom = 1e-18  # last-resort guard (shouldn't happen with the above)
            xnew = xk - px/denom
            new_roots[k] = xnew
            if abs(xnew - xk) > tol:
                done = False
        roots = new_roots
        if done:
            break

    return roots, it

def convolve_desc(a, b):
    """Multiply two polynomials given in DESCENDING powers."""
    da = len(a) - 1
    db = len(b) - 1
    out = [0j] * (da + db + 1)
    for i, ai in enumerate(a):
        for j, bj in enumerate(b):
            out[i + j] += ai * bj
    return out

def rebuild_from_roots(roots):
    """Return DESCENDING coefficients of ∏ (x - r)."""
    coeffs = [1+0j]
    for r in roots:
        coeffs = convolve_desc(coeffs, [1+0j, -r])
    return coeffs

def symmetric_sums(roots):
    """Return (S1, S2, S3, P) for a general degree n."""
    n = len(roots)
    S1 = 0j
    for r in roots: S1 += r
    S2 = 0j
    for i in range(n):
        for j in range(i+1, n):
            S2 += roots[i]*roots[j]
    S3 = 0j
    for i in range(n):
        for j in range(i+1, n):
            for k in range(j+1, n):
                S3 += roots[i]*roots[j]*roots[k]
    P = 1+0j
    for r in roots: P *= r
    return S1, S2, S3, P

def close_to_int(x, eps=1e-12):
    r = int(round(x))
    return (abs(x - r) <= eps, r)

def fmt_c(z, eps=1e-12):
    """Pretty-printer for complex numbers with near-integer snapping."""
    a = z.real
    b = z.imag
    ok_a, ra = close_to_int(a, eps)
    ok_b, rb = close_to_int(b, eps)
    if ok_a: a = ra
    if ok_b: b = rb
    if abs(b) <= eps:
        return f"{a}"
    if abs(a) <= eps:
        if b == 1: return "i"
        if b == -1: return "-i"
        return f"{b}*i"
    sb = "+" if b >= 0 else "-"
    b_abs = b if b >= 0 else -b
    if b_abs == 1:
        return f"{a}{sb}i"
    return f"{a}{sb}{b_abs}*i"

def factor_string(roots):
    return "".join(f"(x - {fmt_c(r)})" for r in roots)

def verify_and_explain(name, coeffs):
    print("="*72)
    print(f"Polynomial [{name}]:")
    # Pretty polynomial print
    terms = []
    n = len(coeffs)-1
    for i, c in enumerate(coeffs):
        p = n - i
        if abs(c) == 0:
            continue
        if p == 0:
            terms.append(f"{fmt_c(c)}")
        elif p == 1:
            terms.append(f"{fmt_c(c)}*x")
        else:
            terms.append(f"{fmt_c(c)}*x^{p}")
    print(" P(x) = " + " + ".join(terms))

    # Solve
    roots, it = durand_kerner(coeffs)
    roots_sorted = sorted(roots, key=lambda z: (round(z.real, 12), round(z.imag, 12)))

    print("\nRoots found (sorted):")
    for r in roots_sorted:
        print("  ", fmt_c(r), "   (as complex:", r, ")")

    # Residuals
    res = [abs(poly_eval(coeffs, r)) for r in roots_sorted]
    print("\nProof harness — checks:")
    print(f"  Max |P(r)| residual = {max(res)}")

    # Vieta checks (monic)
    lc = coeffs[0]
    monic = [c / lc for c in coeffs]
    a3, a2, a1, a0 = monic[1], monic[2], monic[3], monic[4]
    S1, S2, S3, P = symmetric_sums(roots_sorted)
    print("  Vieta (monic):")
    print(f"    sum(r_i)              = {S1}   vs   -a3 = {-a3}")
    print(f"    sum_{{i<j}} r_i r_j   = {S2}   vs    a2 = {a2}")
    print(f"    sum_{{i<j<k}} r_i r_j r_k = {S3}   vs   -a1 = {-a1}")
    print(f"    ∏ r_i                 = {P}    vs    a0 = {a0}")

    # Rebuild and compare
    rebuilt = rebuild_from_roots(roots_sorted)
    rebuilt = [lc * c for c in rebuilt]  # scale back
    diffs = [abs(rebuilt[i] - coeffs[i]) for i in range(len(coeffs))]
    print("\n  Rebuild-from-roots check:")
    print("    Original coeffs:", coeffs)
    print("    Rebuilt  coeffs:", rebuilt)
    print("    Max |Δ coeff|   :", max(diffs))

    # Final factorization display
    print("\nFactorization (up to ordering):")
    print("  P(x) = " + factor_string(roots_sorted))
    print(f"  (Durand–Kerner iterations: {it})")

def main():
    # Target polynomials (DESCENDING powers). Use `j` for sqrt(-1).
    p1 = [1, -10, 35, -50, 24]
    p2 = [1, (-9-5j), (14+33j), (24-44j), -26]

    # Short "reason why" recaps before the harness output
    print("Reason why P1 has the roots 1, 2, 3, 4:")
    print("  (x-1)(x-4)=x^2-5x+4, (x-2)(x-3)=x^2-5x+6;")
    print("  letting y=x^2-5x gives (y+4)(y+6)=y^2+10y+24 -> the given quartic.\n")

    print("Reason why P2 has exact simple complex roots:")
    print("  (x - i)(x - (1+i))(x - (3+2i))(x - (5+i)) expands to the given coefficients;")
    print("  the symmetric sums match: sum=9+5i, pairs=14+33i, triples=24-44i, product=-26.\n")

    # Run the proof harness
    verify_and_explain("P1: x^4 - 10x^3 + 35x^2 - 50x + 24", p1)
    verify_and_explain("P2: x^4 + (-9-5i)x^3 + (14+33i)x^2 + (24-44i)x - 26", p2)

if __name__ == "__main__":
    main()

