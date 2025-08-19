#!/usr/bin/env python3
"""
Super-dense coding demo (pure Python, no imports)
=================================================

Protocol recap
--------------
1) Alice & Bob share the Bell state |Φ⁺⟩ = (|00⟩ + |11⟩)/√2.
2) To encode two classical bits b1 b0, Alice applies on *her* qubit:
        00 →  I
        01 →  X
        10 →  Z
        11 →  Y           (global phase is irrelevant)
3) Alice ships that single qubit to Bob.
4) Bob performs CNOT (Alice=control, Bob=target) then H on Alice’s qubit,
   and finally measures both qubits in the computational basis.
   The outcome is exactly b1 b0.

Implementation notes (this script)
----------------------------------
• The 2-qubit state is a length-4 Python list of complex numbers.
• Basis order: |00>, |01>, |10>, |11> with the first bit = Alice, second = Bob.
• Gates:
    - I, X, Y, Z, H as 2×2 lists.
    - CNOT as a fixed 4×4.
    - Kronecker product builds 4×4 operators like (U⊗I) for Alice-only ops.
• For each bit pair (b1,b0) we print:
    - Reason why: which Bell state Alice created and how Bob’s decode recovers b1 b0.
    - Check (harness): norms, Bell match (up to global phase), Bob’s decode result,
      and that the post-decode measurement is deterministic.
"""

# ── single-qubit 2×2 gates ────────────────────────────────────────────────────
I2 = [[1+0j, 0+0j],
      [0+0j, 1+0j]]

X2 = [[0+0j, 1+0j],
      [1+0j, 0+0j]]

Y2 = [[0+0j, -1j],
      [1j,   0+0j]]

Z2 = [[1+0j,  0+0j],
      [0+0j, -1+0j]]

INV_SQRT2 = 1 / (2 ** 0.5)
H2 = [[INV_SQRT2+0j,  INV_SQRT2+0j],
      [INV_SQRT2+0j, -INV_SQRT2+0j]]

# ── two-qubit helpers ─────────────────────────────────────────────────────────
def kron2(A, B):
    """Kronecker product of 2×2 with 2×2 → 4×4."""
    return [
        [A[0][0]*B[0][0], A[0][0]*B[0][1], A[0][1]*B[0][0], A[0][1]*B[0][1]],
        [A[0][0]*B[1][0], A[0][0]*B[1][1], A[0][1]*B[1][0], A[0][1]*B[1][1]],
        [A[1][0]*B[0][0], A[1][0]*B[0][1], A[1][1]*B[0][0], A[1][1]*B[0][1]],
        [A[1][0]*B[1][0], A[1][0]*B[1][1], A[1][1]*B[1][0], A[1][1]*B[1][1]],
    ]

def matvec4(M, v):
    """4×4 times 4-vector."""
    return [M[i][0]*v[0] + M[i][1]*v[1] + M[i][2]*v[2] + M[i][3]*v[3] for i in range(4)]

def vec_norm2(v):
    return sum((abs(x)**2 for x in v))

def inner(u, v):
    return sum((u[i].conjugate()*v[i] for i in range(4)))

def global_phase_match(u, v, eps=1e-12):
    """Return True if unit vectors u, v are equal up to a global phase."""
    nu = vec_norm2(u) ** 0.5
    nv = vec_norm2(v) ** 0.5
    if nu == 0 or nv == 0: return False
    s = inner([x/nu for x in u], [y/nv for y in v])  # complex overlap
    return abs(abs(s) - 1.0) < eps

# CNOT (control = Alice, target = Bob) in computational basis
CNOT = [
    [1+0j, 0+0j, 0+0j, 0+0j],
    [0+0j, 1+0j, 0+0j, 0+0j],
    [0+0j, 0+0j, 0+0j, 1+0j],
    [0+0j, 0+0j, 1+0j, 0+0j],
]

# Bell states
PHI_PLUS  = [INV_SQRT2+0j, 0+0j, 0+0j, INV_SQRT2+0j]           # (|00>+|11>)/√2
PHI_MINUS = [INV_SQRT2+0j, 0+0j, 0+0j, -INV_SQRT2+0j]          # (|00>-|11>)/√2
PSI_PLUS  = [0+0j, INV_SQRT2+0j, INV_SQRT2+0j, 0+0j]           # (|01>+|10>)/√2
PSI_MINUS = [0+0j, INV_SQRT2+0j, -INV_SQRT2+0j, 0+0j]          # (|01>-|10>)/√2

# Alice’s encoding map: bits → unitary on her qubit (⊗ I on Bob)
BIT_TO_U = {
    (0,0): I2,
    (0,1): X2,
    (1,0): Z2,
    (1,1): Y2,  # up to a global phase, this maps to |Ψ−>
}

# Expected Bell state after Alice’s op (up to global phase)
EXPECTED_BELL = {
    (0,0): PHI_PLUS,
    (0,1): PSI_PLUS,
    (1,0): PHI_MINUS,
    (1,1): PSI_MINUS,
}

# Bob’s decode operator: CNOT then H on Alice → (H⊗I)·CNOT when applied to state
H_on_Alice = kron2(H2, I2)

# Computational basis vectors
def basis(b1, b0):
    idx = (b1 << 1) | b0
    e = [0j, 0j, 0j, 0j]
    e[idx] = 1+0j
    return e

# ── End-to-end helpers ────────────────────────────────────────────────────────
def alice_encode(state, bits):
    UA = kron2(BIT_TO_U[bits], I2)
    return matvec4(UA, state)

def bob_decode(state):
    s = matvec4(CNOT, state)
    s = matvec4(H_on_Alice, s)
    # outcome is deterministic; pick argmax of probabilities to get (b1,b0)
    probs = [abs(x)**2 for x in s]
    idx = max(range(4), key=lambda i: probs[i])
    return ((idx >> 1) & 1, idx & 1), s, probs

# ── Demo: show mapping and per-case explain+check ─────────────────────────────
if __name__ == "__main__":
    print("Alice sends  Bob gets")
    print("------------ --------")
    for b1 in (0,1):
        for b0 in (0,1):
            bits = (b1, b0)
            enc = alice_encode(PHI_PLUS, bits)
            (r1, r0), dec_state, probs = bob_decode(enc)
            print(f"   {bits}       ({r1}, {r0})")

    # Per-case Reason why + Check (harness)
    print("\nDetailed cases")
    print("==============")
    for b1 in (0,1):
        for b0 in (0,1):
            bits = (b1, b0)
            print("\n" + "-"*72)
            print(f"Case bits b1b0 = {b1}{b0}")

            # Reason why
            print("Reason why:")
            print("  Start in |Φ⁺⟩ = (|00⟩+|11⟩)/√2.")
            if bits == (0,0):
                bell_name = "|Φ⁺⟩"
                print("  Alice applies I → state stays |Φ⁺⟩.")
            elif bits == (0,1):
                bell_name = "|Ψ⁺⟩"
                print("  Alice applies X on her qubit: |Φ⁺⟩ → |Ψ⁺⟩ (flips Alice’s bit).")
            elif bits == (1,0):
                bell_name = "|Φ⁻⟩"
                print("  Alice applies Z on her qubit: |Φ⁺⟩ → |Φ⁻⟩ (phase on |11⟩).")
            else:  # (1,1)
                bell_name = "|Ψ⁻⟩"
                print("  Alice applies Y=iXZ: |Φ⁺⟩ → |Ψ⁻⟩ up to a global phase.")
            print("  Bob’s decode CNOT→H maps Bell basis to computational basis:")
            print("     |Φ⁺⟩→|00⟩,  |Ψ⁺⟩→|01⟩,  |Φ⁻⟩→|10⟩,  |Ψ⁻⟩→|11⟩.")
            print("  Therefore the measurement yields exactly b1b0.")

            # Check (harness)
            print("Check (harness):")
            encoded = alice_encode(PHI_PLUS, bits)
            norm_enc = vec_norm2(encoded)
            bell_target = EXPECTED_BELL[bits]
            bell_match = global_phase_match(encoded, bell_target)

            (r1, r0), decoded, probs = bob_decode(encoded)
            norm_dec = vec_norm2(decoded)
            deterministic = (abs(max(probs) - 1.0) < 1e-12 and sum(probs) - 1.0 < 1e-12)

            print(f"  Encoded norm = {norm_enc:.12f} (should be 1)")
            print(f"  Matches expected {bell_name} up to global phase? {bell_match}")
            print(f"  Bob decodes to bits = ({r1}, {r0})  (should be ({b1}, {b0}))")
            print(f"  Decoded norm = {norm_dec:.12f} (should be 1)")
            print(f"  Measurement deterministic? {deterministic}  (probs = {[round(p,12) for p in probs]})")

