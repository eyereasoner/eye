"""
Three-qubit bit-flip error-correction code  (pure NumPy, little-endian order)

Order convention
----------------
State vector basis |q2 q1 q0⟩, with
    idx = b0 + 2 b1 + 4 b2 ,
so qubit 0 is the *least* significant bit (right-most in the ket).

Steps
-----
1.  Encode |ψ⟩ into |ψ̄⟩ = α|000⟩ + β|111⟩.
2.  Apply an X flip to **one** chosen qubit (or no flip).
3.  Measure stabilisers
        S₁ = Z₀ Z₁   ,   S₂ = Z₁ Z₂     (eigenvalues ±1).
4.  Look the syndrome up in the table and apply the corresponding X.
5.  Decode and verify the recovered single-qubit state.
"""

import numpy as np
import itertools

# ── elementary single-qubit operators ─────────────────────────────────────────
I = np.eye(2, dtype=complex)
X = np.array([[0, 1],
              [1, 0]], dtype=complex)
Z = np.array([[1, 0],
              [0,-1]], dtype=complex)

def kron(*ops):
    out = ops[0]
    for op in ops[1:]:
        out = np.kron(out, op)
    return out

# ── little-endian CNOT (control → target) acting on an n-qubit vector ─────────
def apply_cnot(vec: np.ndarray, control: int, target: int) -> np.ndarray:
    """Return (CNOT_{control→target}) |vec⟩ .  Qubit 0 is **LSB**."""
    n = int(np.log2(vec.size))
    res = np.zeros_like(vec)
    for idx, amp in enumerate(vec):
        if (idx >> control) & 1:          # control = 1 → flip target bit
            idx ^= 1 << target
        res[idx] = amp
    return res

# ── encode |ψ⟩ → |ψ̄⟩ = α|000⟩ + β|111⟩ (q0 = data, q1,q2 = ancillae) ─────────
def encode(psi):
    vec = kron(np.array([1,0]), np.array([1,0]), psi)   # |00ψ⟩  (q2 q1 q0)
    vec = apply_cnot(vec, 0, 1)   # CNOT q0 → q1
    vec = apply_cnot(vec, 0, 2)   # CNOT q0 → q2
    return vec

# ── Pauli-X acting on a chosen qubit index (little-endian) ────────────────────
def apply_X(vec, qubit):
    ops = [X if k == qubit else I for k in reversed(range(3))]  # q2 q1 q0 order
    return kron(*ops) @ vec

# ── build stabiliser projectors P_{s1,s2}  (eigenvalues ±1) ───────────────────
P_S = {}
for s1, s2 in itertools.product([+1, -1], repeat=2):
    keep = []
    for b2, b1, b0 in itertools.product([0, 1], repeat=3):
        if (-1)**(b0 ^ b1) == s1 and (-1)**(b1 ^ b2) == s2:
            idx = b0 + 2*b1 + 4*b2        # little-endian
            keep.append(idx)
    P = np.zeros((8, 8), complex)
    for idx in keep:
        P[idx, idx] = 1
    P_S[(s1, s2)] = P

# syndrome → correction operator (X on the appropriate qubit)
CORR = {
    (+1, +1): kron(I, I, I),        # no error
    (-1, +1): kron(I, I, X),        # flip qubit 0 (LSB)
    (-1, -1): kron(I, X, I),        # flip qubit 1
    (+1, -1): kron(X, I, I)         # flip qubit 2 (MSB)
}

# ── choose a test qubit |ψ⟩ = α|0⟩ + β|1⟩ ─────────────────────────────────────
theta  = 0.37 * np.pi
alpha, beta = np.cos(theta), np.sin(theta)
psi1   = np.array([alpha, beta], dtype=complex)

print(f"Logical qubit to protect : |ψ⟩ = {alpha:.3f}|0⟩ + {beta:.3f}|1⟩\n")

logical = encode(psi1)

for error_qubit in [None, 0, 1, 2]:
    state = logical.copy()
    if error_qubit is not None:
        state = apply_X(state, error_qubit)

    # ── syndrome measurement (find the one projector with weight 1/2) ─────────
    for (s1, s2), P in P_S.items():
        p = np.real_if_close(state.conj() @ (P @ state))
        if p > 1e-12:
            state = (P @ state) / np.sqrt(p)
            syndrome = (s1, s2)
            break

    # ── correction and decode (inverse of the two CNOTs) ─────────────────────
    state = CORR[syndrome] @ state
    state = apply_cnot(state, 0, 2)      # undo q0 → q2
    state = apply_cnot(state, 0, 1)      # undo q0 → q1

    # keep amplitudes where q2=q1=0  ⇒ indices 0 and 1
    decoded = state[[0, 1]]
    decoded /= np.linalg.norm(decoded)

    fidelity = abs(np.vdot(psi1, decoded))**2
    label = "none" if error_qubit is None else f"X on qubit {error_qubit}"
    print(f"{label:<14}  →  fidelity = {fidelity:.6f}")
