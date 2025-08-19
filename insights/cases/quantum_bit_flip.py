#!/usr/bin/env python3
"""
Three-qubit bit-flip error-correction code  (pure Python, little-endian order)
==============================================================================

Problem
-------
Protect a single qubit |ψ⟩ = α|0⟩ + β|1⟩ from a **single bit-flip (X) error** on
one of three physical qubits using the smallest repetition code. Detect the
error with stabilizers and correct it, then decode and compare to the original.

Order convention (little-endian)
--------------------------------
State vector basis |q2 q1 q0⟩, with integer index
    idx = b0 + 2 b1 + 4 b2,
so **qubit 0 is the least significant bit (right-most in the ket)**.

Encoding circuit (repetition of the data qubit)
-----------------------------------------------
Place |ψ⟩ on q0 and ancillae |0⟩ on q1,q2, then copy by CNOTs:
    |00ψ⟩  --CNOT(q0→q1)-->  --CNOT(q0→q2)-->  |ψ̄⟩ = α|000⟩ + β|111⟩.

Stabilizers and syndrome table
------------------------------
We measure commuting Pauli operators
    S₁ = Z₀ Z₁,    S₂ = Z₁ Z₂,         eigenvalues in {+1, −1}.
A single X on one qubit flips exactly one of the equalities among bits and
produces a unique syndrome:

    (S₁, S₂) = (+1, +1)  →  no error
                (−1, +1)  →  X on q0
                (−1, −1)  →  X on q1
                (+1, −1)  →  X on q2

Correction = apply X to the qubit indicated by the syndrome.

Correctness sketch
------------------
• |ψ̄⟩ resides in the +1 eigenspace of both S₁ and S₂.
• Any single bit-flip X_i anticommutes with exactly the stabilizers that touch
  that qubit and flips their eigenvalues, yielding the table above.
• Applying that same X_i again restores |ψ̄⟩.  Decoding (inverse CNOTs) recovers
  |ψ⟩ (global phase is irrelevant), so fidelity is 1.

Implementation notes (this script)
----------------------------------
• The full 3-qubit state is a Python list of 8 complex amplitudes.
• Single-qubit X is implemented by permuting amplitudes whose index differs in
  the target bit; Z multiplies by −1 where that bit is 1.
• “Measurement” is done by computing ⟨Z_i Z_j⟩ from probabilities and taking
  its sign (±1). That’s appropriate here because |ψ̄⟩ and any single-X error
  are stabilized (up to the flipped signs).
• The harness reports the measured syndrome, the applied correction, and the
  **fidelity** with the original |ψ⟩ after decoding.

Limitations
-----------
This 3-qubit code corrects **bit flips (X)** only. Phase flips (Z) and Y=iXZ
are outside its correction capability (they require the 3-qubit phase-flip code
or a concatenation like Shor’s 9-qubit code).
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

# --------------------------- vector helpers ---------------------------------

def vec_norm2(v):
    return sum((abs(a)**2 for a in v))

def kron_vec(u, v):
    """Kronecker product u ⊗ v (u supplies the high-order bits)."""
    out = []
    for a in u:
        for b in v:
            out.append(a*b)
    return out

# --------------------------- single-qubit Paulis -----------------------------

def apply_X(state, q):
    """Apply X on qubit q (0=LSB): flip bit q of the basis index."""
    N = len(state)
    mask = 1 << q
    out = [0j]*N
    for i, amp in enumerate(state):
        out[i ^ mask] = out[i ^ mask] + amp
    return out

def apply_Z(state, q):
    """Apply Z on qubit q (0=LSB): multiply by −1 whenever bit q is 1."""
    out = state[:]
    for i in range(len(state)):
        if (i >> q) & 1:
            out[i] = -out[i]
    return out

# --------------------------- little-endian CNOT ------------------------------

def apply_cnot(state, control, target):
    """CNOT control→target on an n-qubit state (0=LSB)."""
    out = [0j]*len(state)
    flip = 1 << target
    for i, amp in enumerate(state):
        j = (i ^ flip) if (((i >> control) & 1) == 1) else i
        out[j] = out[j] + amp
    return out

# --------------------------- encode / decode --------------------------------

def encode(psi1):
    """
    Start from |00> (q2,q1) ⊗ |ψ> (q0), then copy q0 to q1 and q2:
    |00ψ> --CNOT(q0→q1)--> --CNOT(q0→q2)--> α|000> + β|111>.
    """
    base = kron_vec([1+0j, 0+0j], [1+0j, 0+0j])   # |q2 q1> = |00>
    vec  = kron_vec(base, psi1)                   # |q2 q1 q0>
    vec  = apply_cnot(vec, 0, 1)
    vec  = apply_cnot(vec, 0, 2)
    return vec

def decode(state):
    """Inverse the encode CNOTs and return the (normalized) amplitudes on q0."""
    s = apply_cnot(state, 0, 2)
    s = apply_cnot(s,     0, 1)
    decoded = [s[0], s[1]]  # only basis with q2=q1=0 survive ideal path
    n = (abs(decoded[0])**2 + abs(decoded[1])**2)**0.5
    if n != 0:
        decoded = [decoded[0]/n, decoded[1]/n]
    return decoded

# --------------------------- stabilizer syndromes ----------------------------

def zz_parity_expectation(state, i, j):
    """
    ⟨Z_i Z_j⟩ from probabilities: sum ±|amp|^2 with sign + if bits i,j equal.
    """
    s = 0.0
    for idx, amp in enumerate(state):
        if amp == 0: continue
        parity = ((idx >> i) & 1) ^ ((idx >> j) & 1)
        s += (1.0 if parity == 0 else -1.0) * (abs(amp)**2)
    return s

def measure_syndrome(state):
    """Return (S1, S2) ∈ {+1, −1}² for S1=Z0Z1, S2=Z1Z2."""
    s1 = 1 if zz_parity_expectation(state, 0, 1) >= 0 else -1
    s2 = 1 if zz_parity_expectation(state, 1, 2) >= 0 else -1
    return (s1, s2)

SYNDROME_TO_FLIP = {
    (+1, +1): None,  # no error
    (-1, +1): 0,     # flip q0 (LSB)
    (-1, -1): 1,     # flip q1
    (+1, -1): 2,     # flip q2 (MSB)
}

# --------------------------- run a single case -------------------------------

def run_case(psi1, err_q):
    """Return (syndrome, flip_label, fidelity) after correcting and decoding."""
    logical = encode(psi1)
    state = logical[:] if err_q is None else apply_X(logical, err_q)

    syn = measure_syndrome(state)
    flip = SYNDROME_TO_FLIP[syn]
    flip_label = "none" if flip is None else f"X(q{flip})"

    if flip is not None:
        state = apply_X(state, flip)

    decoded = decode(state)
    # overlap ⟨ψ|decoded⟩
    fid = abs(psi1[0].conjugate()*decoded[0] + psi1[1].conjugate()*decoded[1])**2
    return syn, flip_label, fid, logical

# --------------------------- main -------------------------------------------

if __name__ == "__main__":
    # Choose a concrete test state |ψ⟩ = α|0⟩ + β|1⟩ with θ = 0.37π
    theta = 0.37 * PI
    alpha, beta = cos_t(theta), sin_t(theta)
    # normalize defensively
    nrm = (alpha*alpha + beta*beta)**0.5
    alpha /= nrm; beta /= nrm
    psi1 = [alpha+0j, beta+0j]

    # ---------- Answer (compact results first) ----------
    print("Answer:")
    cases = [("none", None), ("X on q0", 0), ("X on q1", 1), ("X on q2", 2)]
    for label, eq in cases:
        syn, corr, fid, _ = run_case(psi1, eq)
        print(f"  {label:<9} → fidelity = {fid:.6f}")

    # ---------- Reason why ----------
    print("\nReason why:")
    print("  Encode by copying the data qubit to two ancillae: |ψ̄⟩ = α|000⟩+β|111⟩.")
    print("  A single X flips one physical bit and flips the signs of stabilizers that")
    print("  touch it. Measuring S₁=Z₀Z₁ and S₂=Z₁Z₂ yields a unique ±1 pair that")
    print("  pinpoints the flipped qubit (see syndrome table above). Applying that X")
    print("  restores |ψ̄⟩, and inverse CNOTs decode back to |ψ⟩, so the final fidelity")
    print("  with the original is 1 (up to rounding).")

    # ---------- Check (harness) ----------
    print("\nCheck (harness):")
    logical = encode(psi1)
    print(f"  Test state            : |ψ⟩ = {alpha:.6f}|0⟩ + {beta:.6f}|1⟩")
    print(f"  |||ψ̄⟩||² before errors = {vec_norm2(logical):.12f} (should be 1)")
    print("\n  case       syndrome  correction  fidelity")

    for label, eq in cases:
        syn, corr, fid, _ = run_case(psi1, eq)
        print(f"  {label:<9} {syn}    {corr:<10} {fid:.6f}")

