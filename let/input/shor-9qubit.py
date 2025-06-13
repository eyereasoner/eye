"""
Shor 9-qubit code demo (pure Python + NumPy, little-endian convention)
=====================================================================

Logical code words
------------------
Let
    |GHZ+⟩ = (|000⟩ + |111⟩)/√2
    |GHZ–⟩ = (|000⟩ – |111⟩)/√2
Blocks: 0 = qubits 0-2, 1 = 3-5, 2 = 6-8  (qubit 0 = LSB).

    |0̄⟩ = |GHZ+⟩ ⊗ |GHZ+⟩ ⊗ |GHZ+⟩
    |1̄⟩ = |GHZ–⟩ ⊗ |GHZ–⟩ ⊗ |GHZ–⟩
for any |ψ⟩ = α|0⟩ + β|1⟩, the encoded state is |ψ̄⟩ = α|0̄⟩ + β|1̄⟩.

Error-correction strategy
-------------------------
1. **Bit-flip stage (X errors)** – treat each 3-qubit block as the
   standard repetition code.  
   Measure the two Z-parities inside every block  
   (Z₀Z₁ & Z₁Z₂, Z₃Z₄ & Z₄Z₅, …) and apply X to the offending
   qubit determined by the ±1 syndrome.

2. **Phase-flip stage (Z errors)** – after bit-flip cleanup each block is
   effectively a single “logical” qubit in a phase-repetition code.  
   Measure
       M₁ = X₀X₁X₂X₃X₄X₅  (blocks 0⊕1)
       M₂ = X₃X₄X₅X₆X₇X₈  (blocks 1⊕2)
   The ±1 pair (M₁,M₂) identifies which block carries a Z phase,
   then a single Z on (say) the first qubit of that block repairs it.

The script:
* builds |ψ̄⟩ for a generic α,β,
* loops over **all 27 single-qubit Pauli errors** (X,Y,Z on each of 9 qubits),
* performs both correction stages, and
* prints the fidelity ⟨ψ̄|ρ|ψ̄⟩, which is always 1.000000.
"""

import numpy as np
import itertools

# ─────────────────── single-qubit operators ───────────────────────────────────
I = np.eye(2, dtype=complex)
X = np.array([[0, 1], [1, 0]],            dtype=complex)
Y = np.array([[0, -1j], [1j, 0]],         dtype=complex)
Z = np.array([[1, 0], [0, -1]],           dtype=complex)
PAULI = {'I': I, 'X': X, 'Y': Y, 'Z': Z}

def kron(*ops):
    out = ops[0]
    for op in ops[1:]:
        out = np.kron(out, op)
    return out

# ─────────────────── utility: apply operators ─────────────────────────────────
def apply_pauli(vec: np.ndarray, qubit: int, label: str) -> np.ndarray:
    """Apply a Pauli {X,Y,Z} to `qubit` (0 = LSB) and return new vector."""
    ops = [PAULI[label] if k == qubit else I for k in reversed(range(9))]
    return kron(*ops) @ vec

def apply_cnot(vec: np.ndarray, control: int, target: int) -> np.ndarray:
    """Little-endian CNOT(control→target) acting on a 9-qubit state vector."""
    res = np.zeros_like(vec)
    for idx, amp in enumerate(vec):
        j = idx ^ (1 << target) if ((idx >> control) & 1) else idx
        res[j] = amp
    return res

# ─────────────────── logical basis |0̄⟩ and |1̄⟩ ───────────────────────────────
def ghz(sign=+1):
    """Return |000⟩ + sign·|111⟩ (8-component, little-endian)."""
    v = np.zeros(8, complex)
    v[0] = 1
    v[7] = sign
    return v / np.sqrt(2)

GHZ_plus  = ghz(+1)
GHZ_minus = ghz(-1)

# blocks: 0 = qubits 0-2,  1 = 3-5,  2 = 6-8  (block0 is the LSB trio)
ket0 = kron(GHZ_plus,  GHZ_plus,  GHZ_plus)   # |0̄⟩
ket1 = kron(GHZ_minus, GHZ_minus, GHZ_minus)  # |1̄⟩

def logical_state(alpha: float, beta: float) -> np.ndarray:
    """Return α|0̄⟩ + β|1̄⟩ (normalisation assumed |α|²+|β|²=1)."""
    return alpha * ket0 + beta * ket1

# ─────────────────── bit-flip (X) syndrome helpers ────────────────────────────
def zz_parity(state: np.ndarray, i: int, j: int) -> int:
    """Expectation of Z_i Z_j → ±1 (little-endian)."""
    phase = 0.0
    for idx, amp in enumerate(state):
        if amp == 0: continue
        parity = ((idx >> i) & 1) ^ ((idx >> j) & 1)
        phase += (1 if parity == 0 else -1) * (abs(amp) ** 2)
    return 1 if phase >= 0 else -1

def correct_bit_flip(state: np.ndarray, base: int) -> np.ndarray:
    """
    Correct a single X/Y in the 3-qubit block {base,base+1,base+2}.
    """
    s1 = zz_parity(state, base,   base+1)
    s2 = zz_parity(state, base+1, base+2)

    if   (s1, s2) == (-1, +1): q = base       # left qubit flipped
    elif (s1, s2) == (+1, -1): q = base+2     # right qubit flipped
    elif (s1, s2) == (-1, -1): q = base+1     # middle qubit flipped
    else:                                     # (+1,+1) → no X error
        return state
    return apply_pauli(state, q, 'X')

# ─────────────────── phase-flip (Z) syndrome helpers ──────────────────────────
def build_bigX(indices):
    """Tensor product X…X on the specified physical qubit indices."""
    ops = [X if k in indices else I for k in reversed(range(9))]
    return kron(*ops)

X01 = build_bigX(range(0, 6))   # X on blocks 0 ⊕ 1   (qubits 0-5)
X12 = build_bigX(range(3, 9))   # X on blocks 1 ⊕ 2   (qubits 3-8)

TOL = 1e-10
def sgn(x):             # robust sign with tolerance
    return 1 if x > -TOL else -1

def phase_block(state: np.ndarray) -> np.ndarray:
    """Detect which 3-qubit block carries a Z phase and fix it."""
    m1 = np.real_if_close(state.conj() @ (X01 @ state))
    m2 = np.real_if_close(state.conj() @ (X12 @ state))
    s1, s2 = sgn(m1), sgn(m2)

    if   (s1, s2) == ( 1, 1): return state        # no phase error
    elif (s1, s2) == (-1, 1): return apply_pauli(state, 0, 'Z')  # block 0
    elif (s1, s2) == (-1,-1): return apply_pauli(state, 3, 'Z')  # block 1
    elif (s1, s2) == ( 1,-1): return apply_pauli(state, 6, 'Z')  # block 2
    return state  # should never reach here

# ─────────────────── demo: all single-qubit Paulis ────────────────────────────
theta = 0.37 * np.pi
alpha, beta = np.cos(theta), np.sin(theta)
psi_bar = logical_state(alpha, beta)

errors = [(label, q) for label in ('X', 'Y', 'Z') for q in range(9)]

print(f"Encoded logical qubit : α={alpha:.3f}, β={beta:.3f}\n")
print("error             fidelity")

for label, q in errors:
    # 1) apply the chosen Pauli error
    state = apply_pauli(psi_bar, q, label)

    # 2) bit-flip correction on each 3-qubit repetition block
    for base in (0, 3, 6):
        state = correct_bit_flip(state, base)

    # 3) phase-flip correction across the three blocks
    state = phase_block(state)

    # 4) fidelity with the original logical state
    fid = abs(np.vdot(psi_bar, state)) ** 2
    print(f"{label} on q{q:<2}      {fid:.6f}")

