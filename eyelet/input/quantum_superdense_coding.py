"""
Super-dense coding demo (pure Python + NumPy)

Protocol recap
--------------
1. Alice & Bob share the Bell state |Φ⁺⟩ = (|00⟩ + |11⟩)/√2.
2. To encode two classical bits b1 b0, Alice applies on *her* qubit:
      00 →  I   (identity)
      01 →  X
      10 →  Z
      11 →  Y
3. Alice ships that single qubit to Bob.
4. Bob performs CNOT (Alice=control, Bob=target) then H on Alice’s qubit,
   and finally measures both qubits in the computational basis.
   The outcome is exactly b1 b0.
"""

import numpy as np
import itertools

# ── single-qubit gates ─────────────────────────────────────────────────────────
I = np.array([[1, 0],
              [0, 1]], dtype=complex)

X = np.array([[0, 1],
              [1, 0]], dtype=complex)

Y = np.array([[0, -1j],
              [1j,  0]], dtype=complex)

Z = np.array([[1,  0],
              [0, -1]], dtype=complex)

H = (1/np.sqrt(2)) * np.array([[1,  1],
                               [1, -1]], dtype=complex)

# ── two-qubit helpers ──────────────────────────────────────────────────────────
def kron(*matrices):
    """Kronecker product of a sequence of matrices."""
    result = matrices[0]
    for m in matrices[1:]:
        result = np.kron(result, m)
    return result

# CNOT (control qubit 0, target qubit 1) in computational basis
CNOT = np.array([[1, 0, 0, 0],
                 [0, 1, 0, 0],
                 [0, 0, 0, 1],
                 [0, 0, 1, 0]], dtype=complex)

# |Φ⁺⟩ Bell state |00⟩ + |11⟩, normalised
phi_plus = (1/np.sqrt(2)) * np.array([1, 0, 0, 1], dtype=complex)

# Map 2 classical bits → Alice’s unitary
BIT_TO_OP = {
    (0, 0): I,
    (0, 1): X,
    (1, 0): Z,
    (1, 1): Y,
}

# ── protocol steps as functions ────────────────────────────────────────────────
def alice_encode(state: np.ndarray, bits: tuple[int, int]) -> np.ndarray:
    """Apply the appropriate single-qubit Pauli to Alice’s qubit (qubit 0)."""
    UA = kron(BIT_TO_OP[bits], I)   # operator on the 2-qubit space
    return UA @ state

def bob_decode(state: np.ndarray) -> tuple[int, int]:
    """Bob’s reversal: CNOT → H → measure; returns the recovered bits."""
    state = CNOT @ state
    state = kron(H, I) @ state
    # Probabilities for |00>, |01>, |10>, |11>
    probs = np.abs(state)**2
    outcome = np.argmax(probs)
    # Convert basis index back to bit pair
    return ((outcome >> 1) & 1, outcome & 1)

def superdense(bits: tuple[int, int]) -> tuple[int, int]:
    """End-to-end: start from Bell state, run Alice+Bob, return Bob’s bits."""
    encoded = alice_encode(phi_plus, bits)
    return bob_decode(encoded)

# ── demo / sanity check ────────────────────────────────────────────────────────
if __name__ == "__main__":
    print("Alice sends  Bob gets")
    print("------------ --------")
    for bits in itertools.product([0, 1], repeat=2):
        print(f"   {bits}     {superdense(bits)}")
