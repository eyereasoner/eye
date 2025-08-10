#!/usr/bin/env python3
"""
Shor 9-qubit code demo (pure Python, no imports, little-endian convention)
==========================================================================
(Original extensive comments preserved and augmented)

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

This script:
* builds |ψ̄⟩ for a generic α,β,
* prints a **Reason why** for the two-stage correction,
* loops over **all 27 single-qubit Pauli errors** (X,Y,Z on each of 9 qubits),
* performs both correction stages, and
* prints a **Check (harness)** table with the final fidelity ⟨ψ̄|ρ|ψ̄⟩
  (which is always 1.000000 up to rounding).

Implementation notes
--------------------
• The 9-qubit pure state is a length-512 Python list of complex numbers.
• Little-endian basis: index bit k is qubit k (qubit 0 is the LSB).
• Single-qubit Paulis act by in-place permutations (X) and phases (Z, Y=iXZ).
• Z-parities ⟨Z_i Z_j⟩ are computed from probabilities; big-X stabilizers are
  applied via a bitmask flip and inner product for the expectation.
• All unitaries preserve the ℓ² norm; we print norms for sanity.
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

# --------------------------- vector helpers ---------------------------------

def vec_norm2(v):
    return sum((abs(a)**2 for a in v))

def inner(u, v):
    return sum((u[i].conjugate()*v[i] for i in range(len(u))))

def kron_vec(u, v):
    """Kronecker product of state vectors u⊗v."""
    out = []
    for a in u:
        for b in v:
            out.append(a*b)
    return out

# --------------------------- GHZ blocks & logical states ---------------------

def ghz(sign=+1):
    """Return |000⟩ + sign·|111⟩ (length 8, little-endian)."""
    v = [0j]*8
    v[0] = 1+0j
    v[7] = (sign+0j)
    # normalize
    s = (abs(v[0])**2 + abs(v[7])**2)**0.5
    return [x / (s) for x in v]

GHZ_plus  = ghz(+1)
GHZ_minus = ghz(-1)

# blocks: 0 = qubits 0-2, 1 = 3-5, 2 = 6-8 (block0 is LSB trio)
ket0 = kron_vec(kron_vec(GHZ_plus,  GHZ_plus),  GHZ_plus)   # |0̄⟩
ket1 = kron_vec(kron_vec(GHZ_minus, GHZ_minus), GHZ_minus)  # |1̄⟩

def logical_state(alpha: float, beta: float):
    """Return α|0̄⟩ + β|1̄⟩ (assumes |α|²+|β|²=1)."""
    return [alpha*ket0[i] + beta*ket1[i] for i in range(len(ket0))]

# --------------------------- Pauli operations -------------------------------

def apply_pauli(state, q, label):
    """Apply a single-qubit Pauli {X,Y,Z} to qubit q (0 = LSB)."""
    N = len(state)
    mask = 1 << q
    if label == 'X':
        out = [0j]*N
        for i, amp in enumerate(state):
            out[i ^ mask] = out[i ^ mask] + amp
        return out
    elif label == 'Z':
        out = state[:]
        for i in range(N):
            if (i >> q) & 1:
                out[i] = -out[i]
        return out
    elif label == 'Y':
        # Y|0> = i|1>, Y|1> = -i|0>
        out = [0j]*N
        for i, amp in enumerate(state):
            if ((i >> q) & 1) == 0:
                out[i ^ mask] = out[i ^ mask] + (1j * amp)
            else:
                out[i ^ mask] = out[i ^ mask] + (-1j * amp)
        return out
    else:
        return state[:]  # identity

# --------------------------- Bit-flip (X) correction ------------------------

def zz_parity(state, i, j):
    """Expectation of Z_i Z_j → returns ±1 using probabilities."""
    s = 0.0
    for idx, amp in enumerate(state):
        if amp == 0:
            continue
        parity = ((idx >> i) & 1) ^ ((idx >> j) & 1)
        s += (1.0 if parity == 0 else -1.0) * (abs(amp)**2)
    return 1 if s >= 0 else -1

def correct_bit_flip(state, base):
    """
    Correct a single X/Y in the 3-qubit block {base, base+1, base+2}
    using Z-parity syndromes (Z_base Z_{base+1}, Z_{base+1} Z_{base+2}).
    """
    s1 = zz_parity(state, base,   base+1)
    s2 = zz_parity(state, base+1, base+2)
    if   (s1, s2) == (-1, +1): q = base       # left
    elif (s1, s2) == (+1, -1): q = base+2     # right
    elif (s1, s2) == (-1, -1): q = base+1     # middle
    else:                                     # (+1, +1): no X error detected
        return state
    return apply_pauli(state, q, 'X')

# --------------------------- Phase-flip (Z) correction ----------------------

def apply_bigX(state, indices):
    """Apply X on all given qubit indices at once (one bitmask flip)."""
    mask = 0
    for k in indices:
        mask |= (1 << k)
    out = [0j]*len(state)
    for i, amp in enumerate(state):
        out[i ^ mask] = out[i ^ mask] + amp
    return out

def phase_block(state):
    """
    Detect which block carries a Z phase using big-X stabilizers and fix it.
    M1 = X on qubits 0..5 (blocks 0⊕1),  M2 = X on qubits 3..8 (blocks 1⊕2).
    """
    X01 = apply_bigX(state, range(0,6))
    X12 = apply_bigX(state, range(3,9))
    m1 = inner(state, X01).real
    m2 = inner(state, X12).real
    s1 = 1 if m1 >= 0 else -1
    s2 = 1 if m2 >= 0 else -1

    if   (s1, s2) == ( 1, 1): return state                # no phase error
    elif (s1, s2) == (-1, 1): return apply_pauli(state, 0, 'Z')  # block 0
    elif (s1, s2) == (-1,-1): return apply_pauli(state, 3, 'Z')  # block 1
    elif (s1, s2) == ( 1,-1): return apply_pauli(state, 6, 'Z')  # block 2
    return state

# --------------------------- Reason why -------------------------------------

def print_reason():
    print("Reason why:")
    print("  Shor’s code = repetition-in-repetition:")
    print("    • Bit-flip protection: each block of 3 is a classical repetition code;")
    print("      measuring (Z₀Z₁, Z₁Z₂) in each block identifies a single X/Y location,")
    print("      then an X on that qubit repairs it.")
    print("    • Phase-flip protection: after bit-flip cleanup each 3-qubit block")
    print("      encodes a logical |±⟩; a Z on one block flips its phase.")
    print("      Big-X stabilizers M₁=X⁰…X⁵ and M₂=X³…X⁸ tell which block is phased,")
    print("      then one Z (e.g., on the first qubit of that block) restores it.")
    print("  Any single-qubit Pauli error (X, Y=iXZ, or Z) reduces to at most one X and")
    print("  one Z across the two stages, so the final state equals the original |ψ̄⟩.")

# --------------------------- Check (harness) ---------------------------------

def check_harness(theta=0.37*PI):
    alpha, beta = cos_t(theta), sin_t(theta)
    # normalize defensively
    nrm = (alpha*alpha + beta*beta)**0.5
    alpha /= nrm; beta /= nrm

    psi_bar = logical_state(alpha, beta)
    norm0 = vec_norm2(psi_bar)

    print("Check (harness):")
    print(f"  Encoded logical qubit: α={alpha:.6f}, β={beta:.6f}")
    print(f"  |||ψ̄⟩||² = {norm0:.12f} (should be 1)")
    print()
    print("  error            fidelity")

    labels = ('X','Y','Z')
    worst = 1.0
    for label in labels:
        for q in range(9):
            # 1) apply error
            state = apply_pauli(psi_bar, q, label)
            # 2) correct bit flips per block
            for base in (0,3,6):
                state = correct_bit_flip(state, base)
            # 3) correct phase flip across blocks
            state = phase_block(state)
            # 4) fidelity with original
            fid = abs(inner(psi_bar, state))**2
            worst = min(worst, fid)
            print(f"  {label} on q{q:<2}      {fid:.6f}")
    print(f"\n  Worst-case fidelity over all 27 single-qubit Paulis: {worst:.12f}")

# --------------------------- main -------------------------------------------

if __name__ == "__main__":
    print_reason()
    print()
    check_harness()

