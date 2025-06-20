"""
Quantum teleportation demo (pure Python + NumPy)
================================================

Protocol (three qubits)
-----------------------
        ┌──────────┐
 ψ  ───►   Alice   │        Bob
        └──────────┘

1. Shared entanglement: Alice and Bob pre-share the Bell state
       |Φ⁺⟩ = (|00⟩ + |11⟩)/√2   on qubits A₂ (Alice) and B (Bob).

2. Unknown state: Alice holds an extra qubit A₁ in some state
       |ψ⟩ = α|0⟩ + β|1⟩      (here we choose α = cosθ, β = sinθ).

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
"""

import numpy as np
import itertools

# ── single-qubit gates ─────────────────────────────────────────────────────────
I = np.array([[1, 0],
              [0, 1]], dtype=complex)
X = np.array([[0, 1],
              [1, 0]], dtype=complex)
Z = np.array([[1, 0],
              [0,-1]], dtype=complex)
H = (1/np.sqrt(2))*np.array([[1, 1],
                             [1,-1]], dtype=complex)

# ── Kronecker helper ───────────────────────────────────────────────────────────
def kron(*mats):
    out = mats[0]
    for m in mats[1:]:
        out = np.kron(out, m)
    return out

# two-qubit CNOT (control = 0, target = 1) in the computational basis
CNOT = np.array([[1,0,0,0],
                 [0,1,0,0],
                 [0,0,0,1],
                 [0,0,1,0]], dtype=complex)

# ── choose an arbitrary |ψ⟩ to teleport (parametrised by θ) ───────────────────
θ   = 0.42*np.pi
α   = np.cos(θ)
β   = np.sin(θ)
psi = np.array([α, β], dtype=complex)          # |ψ⟩ on qubit A₁

# ── prepare joint 3-qubit state: |ψ⟩ ⊗ |Φ⁺⟩ ──────────────────────────────────
phi_plus = (1/np.sqrt(2))*np.array([1,0,0,1], dtype=complex)  # Bell on A₂,B
state = kron(psi, phi_plus)                                   # order: A₁ A₂ B

# indices: qubit0 = A₁, qubit1 = A₂, qubit2 = B
def apply_2q_gate(state, gate, q1, q2):
    """
    Insert a two-qubit gate acting on qubits (q1,q2) into a 3-qubit register.
    Very small and brute-force but fine for demonstration.
    """
    # reshape-permute so that (q1,q2) become the leading two axes
    order = [q1, q2] + [k for k in (0,1,2) if k not in (q1, q2)]
    state4 = state.reshape([2]*3).transpose(order).reshape(4, 2)

    # apply the 4×4 gate to those two qubits
    state4 = gate @ state4

    # undo the reshapes
    state = state4.reshape(2,2,2).transpose(np.argsort(order)).reshape(8)
    return state

# Step 3: Alice’s Bell-basis measurement circuit
state = apply_2q_gate(state, CNOT, q1=0, q2=1)      # CNOT A₁→A₂
state = (kron(H, I, I) @ state)                     # H on A₁

# ── outcome probabilities & post-measurement states ───────────────────────────
probs = np.abs(state)**2

# map computational-basis index → (m₂, m₁, b) bit tuple
bitstrings = [(idx>>2 & 1, idx>>1 & 1, idx & 1) for idx in range(8)]

# measurement projectors for Alice’s two bits
projectors = {}
for m2, m1 in itertools.product([0,1], [0,1]):
    mask = [idx for idx, (b2, b1, _) in enumerate(bitstrings)
            if (b2, b1) == (m2, m1)]
    P = np.zeros((8, 8), complex)
    for idx in mask:
        P[idx, idx] = 1
    projectors[(m2, m1)] = P

# Pauli corrections Bob applies
corr = {(0,0): I,
        (0,1): X,
        (1,0): Z,
        (1,1): X @ Z}

print(f"Unknown state to teleport: |ψ⟩ = {α:.3f}|0⟩ + {β:.3f}|1⟩\n")

for (m2, m1), P in projectors.items():
    prob = np.real_if_close(state.conj() @ (P @ state))
    if prob < 1e-12:    # outcome impossible for this |ψ⟩ (rare), skip
        continue
    post = P @ state / np.sqrt(prob)                # post-measurement state

    # partial trace over A₁, A₂ to get Bob’s qubit density matrix ρ_B
    post = post.reshape(2,2,2)                      # [A₁][A₂][B]
    rho_B = np.tensordot(post, post.conj(), axes=([0,1],[0,1]))  # 2×2

    # Bob’s conditional correction
    rho_B = corr[(m2, m1)] @ rho_B @ corr[(m2, m1)].conj().T

    # fidelity with the original |ψ⟩
    fidelity = np.real_if_close(np.conjugate(psi) @ (rho_B @ psi))

    print(f"Outcome m₂m₁ = {m2}{m1}  (prob {prob:.3f})  ⇒  fidelity = {fidelity:.6f}")
