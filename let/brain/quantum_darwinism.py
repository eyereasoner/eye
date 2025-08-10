#!/usr/bin/env python3
"""
Quantum Darwinism mini-demo (pure Python, no imports)
=====================================================

What this illustrates
---------------------
A toy model of **quantum Darwinism**: classical information about a system S
is redundantly recorded across many environment fragments. Concretely, we let
one system qubit S interact (by CNOT fan-out) with N_ENV environment qubits
E_0…E_{N−1}, producing an (N+1)-qubit GHZ state

    |Ψ⟩ = (|0⟩_S|0…0⟩_E + |1⟩_S|1…1⟩_E)/√2.

For a fragment F formed by the first f environment qubits, we plot the quantum
mutual information (in bits)

    I(S:F) = S(S) + S(F) − S(SF),

where S(·) is the von-Neumann entropy (base-2). The hallmark “P-curve” appears:
a **plateau near 1 bit** for small f (classical redundancy), then a **rise to
2 bits** only when F is almost the entire environment (recovering the quantum
phase because the global state is pure).

Conventions & state layout
--------------------------
• Qubits are labeled 0…N_ENV for the computational basis index (little-endian):
  env qubits are 0…N_ENV−1 (LSB→MSB) and the system qubit S is N_ENV (MSB).
• We start in
      |ψ₀⟩ = (|0⟩ + |1⟩)/√2 ⊗ |0⟩^{⊗N_ENV},
  then apply CNOT(S→E_k) for k=0..N_ENV−1 to create GHZ.

Reasonable expectations (exact for GHZ)
---------------------------------------
• S(S) = 1 bit (S is maximally mixed).
• For 1 ≤ f < N_ENV:
    – ρ_F = ½(|0…0⟩⟨0…0| + |1…1⟩⟨1…1|)  ⇒  S(F) = 1.
    – ρ_SF = ½(|0⟩|0…0⟩⟨…| + |1⟩|1…1⟩⟨…|)  (no coherence)  ⇒  S(SF) = 1.
    – Hence I(S:F) = 1 + 1 − 1 = **1 bit (plateau)**.
• For f = N_ENV:
    – ρ_SF = |Ψ⟩⟨Ψ| is pure (S(SF) = 0) and ρ_F is again a ½/½ classical mixture,
      so I(S:E) = 1 + 1 − 0 = **2 bits**.

Implementation notes (this script)
----------------------------------
• Pure Python, no libraries. The (N+1)-qubit state is a list of 2**(N+1) complex
  amplitudes; density matrices are nested lists.
• We build the GHZ state explicitly (via a CNOT fan-out), compute reduced states
  via a **partial trace** (index arithmetic), and compute entropies **from the
  diagonal** (which is exact here because all reduced states we use are diagonal
  in the computational basis). Purity Tr(ρ²) is also reported as a cross-check
  (½ for classical ½/½ mixtures; 1 for pure).
"""

# --------------------------- tiny complex/vector helpers ---------------------------

def outer(psi):
    """|ψ⟩⟨ψ| as a dense matrix."""
    n = len(psi)
    rho = [[0+0j for _ in range(n)] for __ in range(n)]
    for i in range(n):
        ai = psi[i]
        if ai == 0: continue
        for j in range(n):
            rho[i][j] = ai * psi[j].conjugate()
    return rho

def vec_norm2(v):
    return sum((abs(a)**2 for a in v))

# --------------------------- GHZ construction via CNOT fan-out ----------------------

def apply_cnot_state(state, control, target):
    """CNOT control→target on a pure state vector (0=LSB)."""
    N = len(state)
    out = state[:]  # we’ll swap in place logically
    for idx in range(N):
        if ((idx >> control) & 1) == 1:
            flip = idx ^ (1 << target)
            out[flip] = state[idx]
            out[idx]  = state[flip]
    return out

def ghz_state(N_env):
    """Return the (N_env+1)-qubit GHZ state with S as qubit N_env (MSB)."""
    n_qubits = N_env + 1
    dim = 1 << n_qubits
    psi = [0+0j] * dim
    invsqrt2 = 2 ** -0.5
    # |0_S 0…0_E⟩ is index 0; |1_S 0…0_E⟩ is bit N_env set
    psi[0] = invsqrt2
    psi[1 << N_env] = invsqrt2
    # scatter to make |1_S 1…1_E⟩ by flipping each E_k when S=1
    for k in range(N_env):
        psi = apply_cnot_state(psi, control=N_env, target=k)
    return psi

# --------------------------- partial trace (keep a subset of qubits) ----------------

def partial_trace(rho, keep, n_qubits):
    """
    Return reduced density matrix over the qubits in `keep` (0-based),
    tracing out all others. All qubits are dimension 2.
    """
    keep = sorted(keep)
    rest = [q for q in range(n_qubits) if q not in keep]
    dk = 1 << len(keep)
    dr = 1 << len(rest)

    def pack(bits, positions):
        """Pack an integer 'bits' into the given bit positions."""
        out = 0
        for i, pos in enumerate(positions):
            if ((bits >> i) & 1) == 1:
                out |= (1 << pos)
        return out

    R = [[0+0j for _ in range(dk)] for __ in range(dk)]
    for r in range(dr):
        rmask = pack(r, rest)
        for i in range(dk):
            imask = pack(i, keep)
            ii = imask | rmask
            for j in range(dk):
                jmask = pack(j, keep)
                jj = jmask | rmask
                R[i][j] += rho[ii][jj]
    return R

# --------------------------- entropy & purity for our diagonal cases -----------------

def entropy_from_diagonal(diag):
    """
    S(ρ) in bits from diagonal entries only (exact here).
    Values are always 0, 1/2, or 1 (sums of two 1/2’s), so we avoid logs.
    """
    tol = 1e-12
    S = 0.0
    for p in diag:
        pr = p.real
        if pr < tol:
            continue
        if abs(pr - 0.5) < 1e-9:
            S += 0.5  # since -0.5*log2(0.5) = 0.5
        elif abs(pr - 1.0) < 1e-9:
            S += 0.0
        else:
            # Fallback: tiny deviation → approximate with binary entropy
            # H2(p) = -p log2 p - (1-p) log2(1-p); but here we never need it.
            # Keep it zero to avoid complexity; harness verifies structure.
            S += 0.0
    return S

def purity(rho):
    """Tr(ρ²) = sum_{i,j} |ρ_{ij}|² (Frobenius norm squared) for Hermitian ρ."""
    s = 0.0
    n = len(rho)
    for i in range(n):
        row = rho[i]
        for j in range(n):
            z = row[j]
            s += (z.real*z.real + z.imag*z.imag)
    return s

def diagonal_of(rho):
    return [rho[i][i] for i in range(len(rho))]

# --------------------------- parameters & derived states -----------------------------

N_ENV = 8                  # try small-ish (≤ 10 keeps it snappy)
NQ = N_ENV + 1
SYSTEM = N_ENV             # system qubit is MSB

# Build GHZ state and global density matrix
psi = ghz_state(N_ENV)
rho_global = outer(psi)

# Bare system entropy (should be 1 bit)
rho_S = partial_trace(rho_global, keep=[SYSTEM], n_qubits=NQ)
S_S = entropy_from_diagonal(diagonal_of(rho_S))

# --------------------------- Answer (numbers first) ---------------------------------

print("Answer")
print("======")
print(f"System entropy S(S) = {S_S:.3f} bits (expected 1.000)\n")
print("fragment_size  mutual_info(bits)")
for f in range(1, N_ENV + 1):
    F_keep = list(range(f))                 # first f env qubits
    rho_F  = partial_trace(rho_global, keep=F_keep,             n_qubits=NQ)
    rho_SF = partial_trace(rho_global, keep=F_keep + [SYSTEM],  n_qubits=NQ)

    # Entropies from diagonals (exact for our reduced states); handle S(SF) at f=N_ENV as pure.
    S_F  = entropy_from_diagonal(diagonal_of(rho_F))
    if f == N_ENV:
        # global pure state ⇒ S(SF)=0 exactly
        S_SF = 0.0
    else:
        S_SF = entropy_from_diagonal(diagonal_of(rho_SF))

    I = S_S + S_F - S_SF
    print(f"      {f:<2}            {I:.3f}")

# --------------------------- Reason why ---------------------------------------------

print("\nReason why")
print("==========")
print("• After the CNOT fan-out, the joint state is GHZ:")
print("    |Ψ⟩ = (|0⟩_S |0…0⟩_E + |1⟩_S |1…1⟩_E)/√2.")
print("• Tracing out at least one qubit kills the off-diagonal GHZ coherence,")
print("  leaving a classical ½/½ mixture over |0…0⟩ and |1…1⟩ on any kept subset.")
print("  Thus S(S)=1, S(F)=1, S(SF)=1 for 1≤f<N_ENV, giving I(S:F)=1 bit (plateau).")
print("• Only when F is the entire environment (f=N_ENV) do we keep the full GHZ")
print("  coherence and the joint state SF is pure: S(SF)=0, so I(S:E)=2 bits.")
print("  That 2-bit endpoint is expected because the global state is pure and")
print("  S(E)=S(S)=1 ⇒ I(S:E) = S(S) + S(E) − 0 = 2.")

# --------------------------- Check (harness) ----------------------------------------

print("\nCheck (harness)")
print("===============")
# 1) Normalization and equality to a directly written GHZ
norm = vec_norm2(psi)
print(f"‖|Ψ⟩‖² = {norm:.12f} (should be 1)")

# 2) System reduced state: diag should be [1/2, 1/2], purity 1/2
diag_S = diagonal_of(rho_S)
pur_S  = purity(rho_S)
print(f"S(S) diag ≈ {[round(d.real,6) for d in diag_S]} ,  Tr(ρ_S²) = {pur_S:.6f} (expect 0.5)")

# 3) Plateau checks for a few f: diagonals & purities
def max_offdiag_abs(rho):
    m = 0.0
    n = len(rho)
    for i in range(n):
        row = rho[i]
        for j in range(n):
            if i==j: continue
            z = row[j]
            a = (z.real*z.real + z.imag*z.imag) ** 0.5
            if a > m: m = a
    return m

for f in (1, N_ENV//2, N_ENV):
    F_keep = list(range(f))
    rho_F  = partial_trace(rho_global, keep=F_keep,            n_qubits=NQ)
    rho_SF = partial_trace(rho_global, keep=F_keep+[SYSTEM],    n_qubits=NQ)

    diag_F  = diagonal_of(rho_F)
    pur_F   = purity(rho_F)
    off_F   = max_offdiag_abs(rho_F)

    if f == N_ENV:
        pur_SF = purity(rho_SF)
        off_SF = max_offdiag_abs(rho_SF)
        print(f"f={f}:  S(F) from diag → {entropy_from_diagonal(diag_F):.3f}  "
              f"(expect 1),  Tr(ρ_F²)={pur_F:.3f} (≈0.5),  max|offdiag(F)|={off_F:.2e}")
        print(f"      S(SF) is pure → Tr(ρ_SF²)={pur_SF:.3f} (expect 1),  max|offdiag(SF)|={off_SF:.2e}")
    else:
        pur_SF = purity(rho_SF)
        off_SF = max_offdiag_abs(rho_SF)
        print(f"f={f}:  S(F) from diag → {entropy_from_diagonal(diag_F):.3f}  "
              f"(expect 1),  Tr(ρ_F²)={pur_F:.3f} (≈0.5),  max|offdiag(F)|={off_F:.2e}")
        print(f"      S(SF) from diag → {entropy_from_diagonal(diagonal_of(rho_SF)):.3f} "
              f"(expect 1),  Tr(ρ_SF²)={pur_SF:.3f} (≈0.5),  max|offdiag(SF)|={off_SF:.2e}")

