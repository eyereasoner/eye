#!/usr/bin/env python3
"""
Quantum teleportation demo (pure Python, no imports)
====================================================

Protocol (three qubits)
-----------------------
        ┌──────────┐
 ψ  ───►   Alice   │        Bob
        └──────────┘

1. Shared entanglement: Alice and Bob pre-share the Bell state
       |Φ⁺⟩ = (|00⟩ + |11⟩)/√2   on qubits A₂ (Alice) and B (Bob).

2. Unknown state: Alice holds an extra qubit A₁ in some state
       |ψ⟩ = α|0⟩ + β|1⟩      (we set α=cosθ, β=sinθ for a chosen θ).

3. Bell measurement: Alice performs a CNOT(A₁→A₂) followed by H on A₁,
   then measures both A₁ and A₂ in the computational basis, yielding
   two classical bits m₂ m₁ ∈ {00,01,10,11}.

4. Classical communication: Alice sends m₂ m₁ to Bob.

5. Conditional correction: Bob applies
        m₂ m₁ = 00 →  I
                01 →  X
                10 →  Z
                11 →  XZ              (order doesn’t matter up to phase)
   on his qubit B.

Result: Bob’s qubit is now |ψ⟩.  Teleportation is complete; the original
qubit A₁ has been destroyed by the measurement, preserving the no-cloning
theorem.

Implementation notes (this script)
----------------------------------
• The 3-qubit state is an 8-entry Python list of complex numbers.
• Qubit order in the basis index (most→least significant bit):  A₁ (bit 2), A₂ (bit 1), B (bit 0).
• We implement H on any qubit by pairwise mixing amplitudes; CNOT is a permutation of amplitudes.
• Measurement is simulated by projectors; Bob’s reduced 2×2 density matrix is obtained by a partial trace.
• We print:
    - Reason why: a short argument that the mapping is X^{m₁} Z^{m₂} up to phase.
    - Check (harness): state norms, outcome probabilities, and fidelity after Bob’s correction.
"""

# --------------------------- tiny math (sin/cos) -----------------------------

PI = 3.1415926535897932384626433832795028841971
TWO_PI = 2.0 * PI

def _reduce_to_pi(x):
    y = x % TWO_PI
    if y > PI:
        y -= TWO_PI
    return y

def cos_t(x):
    y = _reduce_to_pi(x)
    term = 1.0
    s = term
    y2 = y*y
    k = 1
    while True:
        term *= -y2 / ((2*k-1)*(2*k))
        s_old = s
        s += term
        if abs(s - s_old) < 1e-16 or k > 200:
            break
        k += 1
    return s

def sin_t(x):
    y = _reduce_to_pi(x)
    term = y
    s = term
    y2 = y*y
    k = 1
    while True:
        term *= -y2 / ((2*k)*(2*k+1))
        s_old = s
        s += term
        if abs(s - s_old) < 1e-16 or k > 200:
            break
        k += 1
    return s

INV_SQRT2 = 1.0 / (2.0**0.5)

# --------------------------- single-qubit 2x2s ------------------------------

I2 = ((1+0j, 0+0j),
      (0+0j, 1+0j))

X2 = ((0+0j, 1+0j),
      (1+0j, 0+0j))

Z2 = ((1+0j, 0+0j),
      (0+0j,-1+0j))

H2 = ((INV_SQRT2+0j, INV_SQRT2+0j),
      (INV_SQRT2+0j,-INV_SQRT2+0j))

# --------------------------- vector helpers ---------------------------------

def vec_norm2(v):
    return sum((abs(a)**2 for a in v))

def apply_single_qubit_gate(state, gate2, pos):
    """
    Apply a 2x2 gate to qubit at bit-position 'pos' (0=LSB=B, 1=A2, 2=A1).
    Returns a new 8-vector.
    """
    out = state[:]  # will overwrite in pairs
    mask = 1 << pos
    for i in range(8):
        if (i & mask) == 0:
            j = i | mask
            a0 = state[i]
            a1 = state[j]
            out[i] = gate2[0][0]*a0 + gate2[0][1]*a1
            out[j] = gate2[1][0]*a0 + gate2[1][1]*a1
    return out

def apply_cnot(state, control_pos, target_pos):
    """CNOT: if control bit is 1, flip target bit."""
    out = [0j]*8
    for i in range(8):
        if ((i >> control_pos) & 1) == 1:
            j = i ^ (1 << target_pos)
        else:
            j = i
        out[j] = out[j] + state[i]
    return out

# --------------------------- density-matrix helpers -------------------------

def matmul2(A, B):
    return [[A[0][0]*B[0][0] + A[0][1]*B[1][0],  A[0][0]*B[0][1] + A[0][1]*B[1][1]],
            [A[1][0]*B[0][0] + A[1][1]*B[1][0],  A[1][0]*B[0][1] + A[1][1]*B[1][1]]]

def dagger2(A):
    return [[A[0][0].conjugate(), A[1][0].conjugate()],
            [A[0][1].conjugate(), A[1][1].conjugate()]]

def apply_unitary_on_rho(rho, U):
    Ud = dagger2(U)
    return matmul2(matmul2(U, rho), Ud)

def fidelity_pure(rho, psi):
    # <psi| rho |psi>
    v0 = rho[0][0]*psi[0] + rho[0][1]*psi[1]
    v1 = rho[1][0]*psi[0] + rho[1][1]*psi[1]
    return (psi[0].conjugate()*v0 + psi[1].conjugate()*v1).real

# --------------------------- build initial state ----------------------------

# Choose an arbitrary state |ψ⟩ = cos(θ)|0⟩ + sin(θ)|1⟩ with θ = 0.42π
theta = 0.42 * PI
alpha = cos_t(theta)
beta  = sin_t(theta)
# normalize defensively
norm = (alpha*alpha + beta*beta)**0.5
alpha /= norm; beta /= norm

# |Φ+> on A2,B : (|00>+|11>)/√2 with basis bits (A2, B) = (1,0)
phi_plus = [INV_SQRT2+0j, 0+0j, 0+0j, INV_SQRT2+0j]  # length 4

# Tensor |ψ> (A1) with |Φ+> (A2,B) → 3-qubit state in order (A1,A2,B)
state = []
for a1 in [0,1]:
    amp1 = alpha if a1==0 else beta
    for v in phi_plus:
        state.append(amp1 * v)  # length 8

print(f"Unknown state to teleport: |ψ⟩ = {alpha:.6f}|0⟩ + {beta:.6f}|1⟩")
print()

# --------------------------- Reason why -------------------------------------

print("Reason why:")
print("  Start with A₂–B in |Φ⁺⟩ and an unknown A₁ = α|0⟩+β|1⟩.")
print("  After CNOT(A₁→A₂) and H on A₁, measuring (A₂, A₁) yields bits m₂ m₁,")
print("  and Bob’s qubit becomes Z^{m₂} X^{m₁} |ψ⟩ (up to global phase).")
print("  Upon receiving (m₂, m₁), Bob applies X^{m₁} Z^{m₂} and recovers |ψ⟩.")
print("  This uses only local operations and two classical bits; the original")
print("  A₁ is destroyed by measurement, consistent with no-cloning.")
print()

# --------------------------- Step 3: Alice's circuit ------------------------

norm_before = vec_norm2(state)
state = apply_cnot(state, control_pos=2, target_pos=1)     # CNOT A1→A2 (bits 2→1)
state = apply_single_qubit_gate(state, H2, pos=2)         # H on A1 (bit 2)
norm_after = vec_norm2(state)

# --------------------------- Check (harness) --------------------------------

print("Check (harness):")
print(f"  Norms: before = {norm_before:.12f}, after gates = {norm_after:.12f}")

# Outcome probabilities and post-measurement states
# Bits in index i: A1=(i>>2)&1, A2=(i>>1)&1, B=(i>>0)&1
def outcome_prob(state, m2, m1):
    prob = 0.0
    for i, amp in enumerate(state):
        a1 = (i >> 2) & 1
        a2 = (i >> 1) & 1
        if (a2 == m2) and (a1 == m1):
            prob += (abs(amp)**2)
    return prob

# Build post-measurement normalized vector for outcome (m2,m1)
def collapse(state, m2, m1):
    prob = outcome_prob(state, m2, m1)
    if prob == 0.0:
        return prob, [0j]*8
    out = [0j]*8
    inv = (prob**0.5)
    for i, amp in enumerate(state):
        a1 = (i >> 2) & 1
        a2 = (i >> 1) & 1
        if (a2 == m2) and (a1 == m1):
            out[i] = amp / inv
    return prob, out

# Partial trace over A1,A2 → 2×2 rho_B
def reduced_rho_B(state8):
    # state8 indexed by (a1,a2,b)
    rho = [[0+0j, 0+0j],
           [0+0j, 0+0j]]
    for b in (0,1):
        for bp in (0,1):
            s = 0+0j
            for a1 in (0,1):
                for a2 in (0,1):
                    i = (a1<<2) | (a2<<1) | b
                    j = (a1<<2) | (a2<<1) | bp
                    s += state8[i] * state8[j].conjugate()
            rho[b][bp] = s
    return rho

def fmt_c(x):
    return f"{x:.12f}"

# Corrections for Bob: keyed by (m2,m1)
Ucorr = {
    (0,0): I2,
    (0,1): X2,
    (1,0): Z2,
    (1,1): matmul2(X2, Z2)  # XZ
}

# Original |ψ⟩ as a 2-vector
psi = (alpha+0j, beta+0j)

# Collect and print per-outcome checks
probs = []
for m2 in (0,1):
    for m1 in (0,1):
        prob, post = collapse(state, m2, m1)
        probs.append(prob)
        if prob < 1e-15:
            continue
        rhoB = reduced_rho_B(post)
        # Apply Bob's conditional correction
        rhoB_corr = apply_unitary_on_rho(rhoB, Ucorr[(m2,m1)])
        fid = fidelity_pure(rhoB_corr, psi)
        print(f"  Outcome m₂m₁ = {m2}{m1}:  Pr = {prob:.12f}  →  fidelity after correction = {fid:.12f}")

print(f"  Sum of probabilities = {sum(probs):.12f} (should be 1)")

