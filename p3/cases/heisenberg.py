#!/usr/bin/env python3
"""
heisenberg.py  —  matrix-mechanics demo of three uncertainty relations
======================================================================

Goal
----
Demonstrate the Heisenberg–Robertson uncertainty relation
    ΔA · ΔB ≥ (1/2) |⟨[A,B]⟩|
for three observable pairs on a single electron in 1-D, using **matrix mechanics**
in a finite basis — entirely in pure Python.

Observable pairs
----------------
1) Position X  vs Momentum P          (canonical pair)
2) Spin Sx     vs Spin Sz             (orthogonal spin components)
3) Position X  vs Spin Sz             (commuting, bound should be 0)

Hilbert space and units
-----------------------
We work in the direct product
    H_total = H_space ⊗ H_spin,
where H_space is the first N harmonic‐oscillator (HO) eigenstates and H_spin is
the 2-dimensional spin-½ space. We fix  ħ = 1,  m = 1,  ω = 1.

Operators
---------
In the HO basis, with ladder operators a|n⟩ = √n |n-1⟩ and a†|n⟩ = √(n+1)|n+1⟩,
we set
    X = (a† + a)/√2,     P = i(a† − a)/√2,     H = (P^2 + X^2)/2 .
For spin we use S = σ/2 with Pauli matrices.

State we test
-------------
Spatial part: ground state |0⟩.    Spin: (|↑⟩ + i |↓⟩)/√2   (eigenstate of Sy with +1/2).
The total state is |ψ⟩ = |0⟩ ⊗ (|↑⟩ + i|↓⟩)/√2.

What you should see (analytic expectations)
-------------------------------------------
• Ground HO has  ΔX = ΔP = 1/√2, so  ΔX·ΔP = 1/2 and **saturates** the bound
  since ⟨[X,P]⟩ = i ⇒ (1/2)|⟨[X,P]⟩| = 1/2.
• For spin, [Sx,Sz] = i Sy and ⟨Sy⟩ = +1/2 in our spinor, hence the bound is
  (1/2)|⟨i Sy⟩| = (1/2)·|⟨Sy⟩| = 1/4, and ΔSx = ΔSz = 1/2, so product = 1/4:
  **also saturates**.
• X and Sz act on different tensor factors ⇒ they **commute**, bound = 0 while
  ΔX and ΔSz are finite.

Proof harness (what we verify)
------------------------------
• Normalization of |ψ⟩ and Hermiticity of X, P, Sx, Sz.
• Commutators: [X,P] ≈ i·I (exact on states supported below the truncation edge)
  and [X,Sz] = 0.
• Numeric inequalities  ΔA·ΔB ≥ (1/2)|⟨[A,B]⟩|  for the three pairs, plus
  “how tight” the first two are (saturation gap).
• Everything is pure Python lists of complex numbers (no imports).

Notes on truncation
-------------------
We use a finite HO basis of size N. For our ground‐state tests, the top edge
is never populated, so the canonical relations are **exact** in expectation.
Raising N improves robustness for more excited states.
"""

# --------------------------- tiny linear algebra -----------------------------

def zeros(n, m):
    return [[0+0j for _ in range(m)] for __ in range(n)]

def eye(n):
    M = zeros(n, n)
    for i in range(n):
        M[i][i] = 1+0j
    return M

def dag(A):
    n, m = len(A), len(A[0])
    B = zeros(m, n)
    for i in range(n):
        for j in range(m):
            B[j][i] = A[i][j].conjugate()
    return B

def mat_add(A, B):
    n, m = len(A), len(A[0])
    C = zeros(n, m)
    for i in range(n):
        for j in range(m):
            C[i][j] = A[i][j] + B[i][j]
    return C

def mat_sub(A, B):
    n, m = len(A), len(A[0])
    C = zeros(n, m)
    for i in range(n):
        for j in range(m):
            C[i][j] = A[i][j] - B[i][j]
    return C

def mat_scalar(A, s):
    n, m = len(A), len(A[0])
    C = zeros(n, m)
    for i in range(n):
        for j in range(m):
            C[i][j] = s * A[i][j]
    return C

def matmul(A, B):
    n, k = len(A), len(A[0])
    k2, m = len(B), len(B[0])
    if k != k2:
        raise ValueError("shape mismatch in matmul")
    C = zeros(n, m)
    for i in range(n):
        Ai = A[i]
        for t in range(k):
            a = Ai[t]
            if a == 0: continue
            Bt = B[t]
            for j in range(m):
                C[i][j] += a * Bt[j]
    return C

def matvec(A, v):
    n, m = len(A), len(A[0])
    if len(v) != m:
        raise ValueError("shape mismatch in matvec")
    out = [0+0j for _ in range(n)]
    for i in range(n):
        s = 0+0j
        Ai = A[i]
        for j in range(m):
            if Ai[j] != 0:
                s += Ai[j] * v[j]
        out[i] = s
    return out

def vec_inner(u, v):
    if len(u) != len(v):
        raise ValueError("shape mismatch in inner")
    s = 0+0j
    for i in range(len(u)):
        s += u[i].conjugate() * v[i]
    return s

def kron(A, B):
    """Kronecker product for matrices A ⊗ B."""
    n, m = len(A), len(A[0])
    p, q = len(B), len(B[0])
    C = zeros(n*p, m*q)
    for i in range(n):
        for j in range(m):
            a = A[i][j]
            if a == 0: continue
            for r in range(p):
                row = i*p + r
                Br = B[r]
                for c in range(q):
                    C[row][j*q + c] += a * Br[c]
    return C

def kron_vec(u, v):
    """Kronecker product for vectors u ⊗ v."""
    out = []
    for a in u:
        for b in v:
            out.append(a*b)
    return out

def comm(A, B):
    return mat_sub(matmul(A, B), matmul(B, A))

# --------------------------- HO ladder & observables -------------------------

def creation_annihilation(N):
    """Return (a, a†) for N-level HO."""
    a = zeros(N, N)
    for n in range(1, N):
        a[n-1][n] = (n ** 0.5)
    adag = dag(a)
    return a, adag

def ho_operators(N):
    """Position X, momentum P, Hamiltonian H for ħ=m=ω=1."""
    a, adag = creation_annihilation(N)
    invsqrt2 = 2 ** -0.5
    X = mat_scalar(mat_add(adag, a), invsqrt2)
    A = mat_sub(adag, a)
    P = mat_scalar(A, 1j * invsqrt2)
    H = mat_scalar(mat_add(matmul(P, P), matmul(X, X)), 0.5)
    return X, P, H

# --------------------------- Spin operators ----------------------------------

def pauli():
    sx = [[0+0j, 1+0j],
          [1+0j, 0+0j]]
    sy = [[0+0j, -1j],
          [1j,    0+0j]]
    sz = [[1+0j,  0+0j],
          [0+0j, -1+0j]]
    return sx, sy, sz

# --------------------------- Uncertainty -------------------------------------

def uncertainty(state, A):
    """ΔA = sqrt(⟨A^2⟩ − ⟨A⟩^2) for normalized |ψ⟩ (as a vector)."""
    Av = matvec(A, state)
    mean = vec_inner(state, Av)
    A2v = matvec(matmul(A, A), state)
    mean2 = vec_inner(state, A2v)
    var = mean2 - (mean * mean)
    # numerical noise can leave ε·i on var
    return (var.real ** 0.5)

# --------------------------- Main demo ---------------------------------------

def main():
    # 1) Build operators in H_space ⊗ H_spin
    N = 12  # HO truncation; increase if you explore excited states
    X, P, _H = ho_operators(N)

    sx, sy, sz = pauli()
    half = 0.5
    Sx = mat_scalar(sx, half)
    Sy = mat_scalar(sy, half)
    Sz = mat_scalar(sz, half)

    I_space = eye(N)
    I_spin  = eye(2)

    X_full  = kron(X, I_spin)
    P_full  = kron(P, I_spin)
    Sx_full = kron(I_space, Sx)
    Sy_full = kron(I_space, Sy)
    Sz_full = kron(I_space, Sz)

    # 2) Build |ψ⟩ = |0⟩ ⊗ (|↑⟩ + i|↓⟩)/√2
    ground = [0+0j for _ in range(N)]
    ground[0] = 1+0j
    invsqrt2 = 2 ** -0.5
    spinor = [1*invsqrt2 + 0j, 1j*invsqrt2]
    psi = kron_vec(ground, spinor)
    # normalize defensively
    nrm = (vec_inner(psi, psi).real) ** 0.5
    psi = [x / nrm for x in psi]

    # 3) Uncertainties and bounds
    dX  = uncertainty(psi, X_full)
    dP  = uncertainty(psi, P_full)
    dSx = uncertainty(psi, Sx_full)
    dSz = uncertainty(psi, Sz_full)

    def bound(A, B):
        return 0.5 * abs(vec_inner(psi, matvec(comm(A, B), psi)))

    bXP   = bound(X_full,  P_full)
    bSxSz = bound(Sx_full, Sz_full)
    bXSz  = bound(X_full,  Sz_full)

    # -------------------- Answer (numbers first) --------------------
    print("Answer")
    print("======")
    dim = len(X_full)
    print(f"Hilbert-space dimension : {dim} = N × 2 = {N} × 2\n")
    header = "{:<15} {:>10} {:>15}".format("Observable pair", "ΔA·ΔB", "½|⟨[A,B]⟩|")
    print(header)
    print("-" * len(header))
    print("{:<15} {:10.4f} {:15.4f}".format("X  &  P",  dX*dP,  bXP))
    print("{:<15} {:10.4f} {:15.4f}".format("Sx & Sz", dSx*dSz, bSxSz))
    print("{:<15} {:10.4f} {:15.4f}".format("X  & Sz", dX*dSz,  bXSz))

    # -------------------- Reason why --------------------
    print("\nReason why")
    print("==========")
    print("• Canonical pair: in units ħ=m=ω=1 the HO ground state has ΔX=ΔP=1/√2,")
    print("  and ⟨[X,P]⟩=i ⇒ the bound is 1/2, so ΔX·ΔP=1/2 saturates the inequality.")
    print("• Spin: [Sx,Sz]=i Sy and our spinor has ⟨Sy⟩=+1/2, giving bound 1/4;")
    print("  for a spin-½ eigenstate of Sy we also have ΔSx=ΔSz=1/2, so product=1/4 — saturated.")
    print("• X vs Sz act on different tensor factors, so they commute → bound=0 while")
    print("  ΔX and ΔSz are finite and independent.\n")

    # -------------------- Check (harness) --------------------
    print("Check (harness)")
    print("===============")
    # normalization
    norm_psi = vec_inner(psi, psi).real
    print(f"Norm ⟨ψ|ψ⟩ = {norm_psi:.12f}  (should be 1)")

    # Hermiticity (max entrywise deviation from A†)
    def herm_gap(A):
        M = mat_sub(A, dag(A))
        mx = 0.0
        for row in M:
            for z in row:
                mx = max(mx, abs(z))
        return mx
    print(f"Hermiticity gaps:  ‖X−X†‖_max={herm_gap(X_full):.3e}, "
          f"‖P−P†‖_max={herm_gap(P_full):.3e}, "
          f"‖Sx−Sx†‖_max={herm_gap(Sx_full):.3e}, "
          f"‖Sz−Sz†‖_max={herm_gap(Sz_full):.3e}")

    # Commutators
    def max_abs_mat(M):
        m = 0.0
        for r in M:
            for z in r:
                if abs(z) > m:
                    m = abs(z)
        return m
    I_full = eye(dim)
    XP_err  = max_abs_mat(mat_sub(comm(X_full, P_full), mat_scalar(I_full, 1j)))
    XSz_err = max_abs_mat(comm(X_full, Sz_full))  # should be exactly zero
    print(f"Commutators:  max|[X,P]−iI| = {XP_err:.3e},   max|[X,Sz]| = {XSz_err:.3e}")

    # Inequalities and saturation gaps
    def sat_gap(prod, bnd):
        return abs(prod - bnd)
    print(f"Inequality checks (should be ≥):")
    print(f"  X & P : ΔX·ΔP = {dX*dP:.12f} ≥ {bXP:.12f}  (gap {sat_gap(dX*dP,bXP):.3e})")
    print(f"  Sx&Sz : ΔSx·ΔSz = {dSx*dSz:.12f} ≥ {bSxSz:.12f}  (gap {sat_gap(dSx*dSz,bSxSz):.3e})")
    print(f"  X &Sz : ΔX·ΔSz = {dX*dSz:.12f} ≥ {bXSz:.12f}  (gap {sat_gap(dX*dSz,bXSz):.3e})")

if __name__ == "__main__":
    main()

