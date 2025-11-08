#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P3 — Self-contained, self-checking script (Lie groups)
Case: One-parameter subgroups via the matrix exponential

Puzzle / Claim
--------------
Let A be a real 2×2 matrix and define X(t) = exp(tA) using the matrix exponential.
Then:

(1) (One-parameter subgroup)  X(t+s) = X(t) X(s)  for all real s,t.
(2) (Derivative at the identity)  d/dt|_{t=0} X(t) = A.
(3) If A is skew-symmetric (A^T = -A), then X(t) ∈ SO(2):  X(t)^T X(t) = I and det X(t) = 1.
(4) det X(t) = exp(t · tr(A))  (in particular, if tr(A)=0 then det X(t)=1).
(5) If A^2 = 0 (nilpotent of index 2), then  X(t) = I + tA  for all t.

Intuition: the map t ↦ exp(tA) gives a smooth group homomorphism R → GL_2(R).
For “rotations” A (skew-symmetric), you get the familiar SO(2) rotations; for
trace-zero A you land in SL_2(R); and nilpotent A generate unipotent one-parameter
subgroups.

Contract (P3 style)
-----------------------------
- Answer: the five statements above.
- Reason Why: short proof outline using series, BCH for commuting case, and basic
  identities (det∘exp = exp∘tr; orthogonality from skew-symmetry).
- Check (harness): numerically verifies all five bullets on many concrete matrices.
  We implement a small, robust expm with scaling & squaring + Taylor (no numpy).

No external packages. Pure Python 3.
"""

import math
import random
random.seed(11)

# ----------------------------
# Tiny 2×2 linear algebra
# ----------------------------

def mat_add(A,B):
    return [[A[0][0]+B[0][0], A[0][1]+B[0][1]],
            [A[1][0]+B[1][0], A[1][1]+B[1][1]]]

def mat_sub(A,B):
    return [[A[0][0]-B[0][0], A[0][1]-B[0][1]],
            [A[1][0]-B[1][0], A[1][1]-B[1][1]]]

def mat_mul(A,B):
    return [[A[0][0]*B[0][0] + A[0][1]*B[1][0], A[0][0]*B[0][1] + A[0][1]*B[1][1]],
            [A[1][0]*B[0][0] + A[1][1]*B[1][0], A[1][0]*B[0][1] + A[1][1]*B[1][1]]]

def mat_scalar(A,s):
    return [[s*A[0][0], s*A[0][1]],[s*A[1][0], s*A[1][1]]]

def mat_eye():
    return [[0.0+1.0, 0.0],[0.0, 0.0+1.0]]  # force float

def mat_T(A):
    return [[A[0][0], A[1][0]],[A[0][1], A[1][1]]]

def mat_trace(A):
    return A[0][0] + A[1][1]

def mat_det(A):
    return A[0][0]*A[1][1] - A[0][1]*A[1][0]

def mat_norm1(A):
    # matrix 1-norm (max column sum of abs)
    c1 = abs(A[0][0]) + abs(A[1][0])
    c2 = abs(A[0][1]) + abs(A[1][1])
    return max(c1, c2)

def mat_normF(A):
    return math.sqrt(sum(a*a for row in A for a in row))

def is_skew(A, tol=1e-12):
    return (abs(A[0][0]) <= tol and abs(A[1][1]) <= tol and
            abs(A[0][1] + A[1][0]) <= tol)

def is_zero(A, tol=1e-12):
    return all(abs(a) <= tol for row in A for a in row)

def mat_pow(A, k):
    if k == 0:
        return mat_eye()
    if k == 1:
        return [[A[0][0],A[0][1]],[A[1][0],A[1][1]]]
    if k % 2 == 0:
        H = mat_pow(A, k//2)
        return mat_mul(H, H)
    else:
        return mat_mul(A, mat_pow(A, k-1))

# ----------------------------
# expm for 2×2: scaling & squaring + Taylor
# ----------------------------

def expm_scaled(A, t):
    """
    Compute exp(tA) by scaling and squaring:
      - choose s so ||(tA)/2^s||_1 <= theta (theta ~ 0.5),
      - compute exp(B) with B = (t/2^s)A via Taylor (20 terms),
      - square s times.
    This is small & robust for 2×2 without numpy.
    """
    theta = 0.5
    Anorm = mat_norm1(A) * abs(t)
    s = 0
    if Anorm > theta and Anorm > 0:
        s = int(math.ceil(math.log(Anorm/theta, 2)))
    B = mat_scalar(A, t / (2**s if s>0 else 1.0))

    # Taylor series exp(B) ≈ sum_{k=0}^{N} B^k / k!
    N = 20
    term = mat_eye()
    S = [[term[0][0], term[0][1]],[term[1][0], term[1][1]]]
    for k in range(1, N+1):
        term = mat_mul(term, B)
        coeff = 1.0 / math.factorial(k)
        S = mat_add(S, mat_scalar(term, coeff))

    # square back
    for _ in range(s):
        S = mat_mul(S, S)
    return S

# ----------------------------
# Reason (proof sketch)
# ----------------------------

def build_reason() -> str:
    lines = []
    lines.append("Why the claims hold (sketch):")
    lines.append("1) One-parameter subgroup: For any square matrix A, exp(tA) exp(sA) = exp((t+s)A)")
    lines.append("   because the series for exp commutes with itself when the exponents are multiples of A;")
    lines.append("   equivalently, this is a special case of Baker–Campbell–Hausdorff when [tA,sA]=0.")
    lines.append("2) Derivative at 0: From exp(tA) = I + tA + O(t^2), we get d/dt|_0 exp(tA) = A.")
    lines.append("3) Skew-symmetric A yields orthogonal exp(tA): (exp(tA))^T exp(tA) = exp(tA^T) exp(tA)")
    lines.append("   = exp(-tA) exp(tA) = I; continuity forces det = +1 in 2D, so exp(tA) ∈ SO(2).")
    lines.append("4) Determinant identity: det(exp(tA)) = exp(tr(tA)) = exp(t·tr(A)) (general fact for any size).")
    lines.append("5) Nilpotent A with A^2=0 has exp(tA) = I + tA (all higher powers vanish).")
    lines.append("")
    lines.append("This shows t ↦ exp(tA) is a Lie group homomorphism R → GL_2(R) with tangent A at t=0.")
    return "\n".join(lines)

# ----------------------------
# Harness
# ----------------------------

EPS = 2e-7

def close(A,B, tol=EPS):
    return mat_normF(mat_sub(A,B)) <= tol

def rand_matrix(scale=0.7):
    return [[random.uniform(-scale, scale), random.uniform(-scale, scale)],
            [random.uniform(-scale, scale), random.uniform(-scale, scale)]]

def test_group_law(trials=30):
    for _ in range(trials):
        A = rand_matrix(scale=0.8)
        s = random.uniform(-1.0, 1.0)
        t = random.uniform(-1.0, 1.0)
        L = expm_scaled(A, t+s)
        R = mat_mul(expm_scaled(A, t), expm_scaled(A, s))
        assert close(L, R, tol=1e-6), "Group law failed: exp((t+s)A) ≠ exp(tA)exp(sA)."
    return "✓ One-parameter subgroup property holds on random tests."

def test_derivative_at_zero(trials=20):
    for _ in range(trials):
        A = rand_matrix(scale=0.8)
        h = 1e-7
        E = expm_scaled(A, h)
        D = mat_scalar(mat_sub(E, mat_eye()), 1.0/h)
        assert close(D, A, tol=5e-5), "Derivative at 0 failed."
    return "✓ Derivative at identity matches A."

def test_skew_symmetric(trials=20):
    for _ in range(trials):
        w = random.uniform(-2.0, 2.0)
        A = [[0.0, -w],[w, 0.0]]
        t = random.uniform(-2.0, 2.0)
        X = expm_scaled(A, t)
        XT_X = mat_mul(mat_T(X), X)
        assert close(XT_X, mat_eye(), tol=2e-6), "(exp(tA))^T exp(tA) ≠ I for skew A."
        detX = mat_det(X)
        assert abs(detX - 1.0) <= 2e-6, "det exp(tA) ≠ 1 for skew A."
    return "✓ Skew-symmetric case lands in SO(2)."

def test_det_trace(trials=25):
    for _ in range(trials):
        A = rand_matrix(scale=0.9)
        t = random.uniform(-1.0, 1.0)
        X = expm_scaled(A, t)
        lhs = mat_det(X)
        rhs = math.exp(t * mat_trace(A))
        assert abs(lhs - rhs) <= 3e-6 * max(1.0, abs(rhs)), "det(exp(tA)) ≠ exp(t tr A)."
    return "✓ det(exp(tA)) = exp(t·tr(A)) verified."

def test_nilpotent(trials=10):
    # Use simple nilpotent: [[0, a],[0, 0]] has A^2 = 0
    for _ in range(trials):
        a = random.uniform(-2.0, 2.0)
        A = [[0.0, a],[0.0, 0.0]]
        t = random.uniform(-3.0, 3.0)
        X = expm_scaled(A, t)
        Y = mat_add(mat_eye(), mat_scalar(A, t))
        assert close(X, Y, tol=1e-8), "Nilpotent formula exp(tA)=I+tA failed."
    return "✓ Nilpotent case exp(tA) = I + tA holds exactly."

def run_checks():
    res = []
    res.append(test_group_law())
    res.append(test_derivative_at_zero())
    res.append(test_skew_symmetric())
    res.append(test_det_trace())
    res.append(test_nilpotent())
    return res

# ----------------------------
# Presentation
# ----------------------------

def print_header(title):
    print("="*72)
    print(title)
    print("="*72)

def main():
    print_header("ANSWER")
    print("For X(t) = exp(tA) with A ∈ M₂(ℝ):")
    print("(1) X(t+s)=X(t)X(s).  (2) X'(0)=A.  (3) If A^T=-A then X(t)∈SO(2).")
    print("(4) det X(t) = exp(t·tr A).  (5) If A²=0 then X(t)=I+tA.")

    print_header("REASON WHY")
    print(build_reason())

    print_header("CHECK (HARNESS)")
    try:
        for line in run_checks():
            print(line)
        print("All checks PASSED ✅")
    except AssertionError as e:
        print(f"Check FAILED ❌: {e}")
        raise

if __name__ == "__main__":
    main()

