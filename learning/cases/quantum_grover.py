#!/usr/bin/env python3
"""
Grover’s algorithm — ARC output, pure Python (no imports)
=========================================================

We model n-qubit states as length-2^n complex vectors in the computational basis.
The oracle O flips the phase of one **marked** basis state |m>, and the diffusion
operator is D = 2|s><s| − I where |s> is the **uniform superposition**.
A single Grover iteration is G = D·O.

What we show
------------
1) Oracle on |m> itself (n=2, m=2) → phase flip:  −|m>.
2) Oracle on an unmarked basis state → unchanged.
3) One Grover iteration from |s> (n=2) → exactly |m> (success prob = 1).
4) One Grover iteration from |s> (n=4) → marked amplitude becomes 11/16,
   others 3/16 (success prob = (11/16)^2).

Reason why (one-iteration formulas, N=2^n)
------------------------------------------
Start with a_i = 1/√N for all i. After oracle: a_m = −1/√N, others +1/√N.
Let μ be the average amplitude after the oracle:
  μ = ((N−1)(1/√N) − 1/√N)/N = (N−2)/(N√N).
Diffusion reflects amplitudes about μ:
  a_m'    = 2μ − (−1/√N) = (3N − 4)/(N√N),
  a_other'= 2μ − ( 1/√N) = (N − 4)/(N√N).
For N=4 these give a_m'=1, a_other'=0. For N=16 they give 11/16 and 3/16.

We print:
• Answer  — the resulting state (compact form when it’s a basis vector).
• Reason why — as above.
• Check (harness) — norm=1, phase-flip test, diffusion-as-reflection test,
  and exact amplitude matches for the analytic formulas.
"""

# --------------------------- tiny statevector toolbox ---------------------------

def dim(n): return 1 << n

def ket_int(n, k):
    """|k> in n qubits."""
    v = [0j]*dim(n)
    v[k] = 1+0j
    return v

def superposition(n):
    """Uniform superposition |s> = (1/√N) Σ_i |i>."""
    N = dim(n)
    amp = 1.0 / (N ** 0.5)
    return [amp+0j]*N

def oracle(n, marked, state):
    """Phase-flip oracle: multiply amplitude of |marked> by −1."""
    out = state[:]
    out[marked] = -out[marked]
    return out

def diffusion(state):
    """D = 2|s><s| − I, applied by reflecting amplitudes about their mean."""
    N = len(state)
    mu = sum(state)/N
    return [2*mu - a for a in state]

def grover_iteration(n, marked, state):
    """G = D·O."""
    return diffusion(oracle(n, marked, state))

def inner(u, v):
    return sum((u[i].conjugate()*v[i] for i in range(len(u))))

def norm2(v):
    return (inner(v, v).real)

def pretty_ket(n, k):
    return "|" + format(k, f"0{n}b") + ">"

def pretty_state(n, v, cutoff=1e-12, max_terms=8):
    """Render Σ c_i |i> . If it’s exactly a basis state, print that compactly."""
    N = len(v)
    # basis state?
    nz = [(i, v[i]) for i in range(N) if abs(v[i]) > cutoff]
    if len(nz) == 1 and abs(nz[0][1]-1) < 1e-12:
        return pretty_ket(n, nz[0][0])
    # otherwise print a few leading terms (all will be real here)
    parts = []
    for i, a in nz[:max_terms]:
        parts.append(f"{a.real:.6f}{'+' if a.imag>=0 else ''}{a.imag:.6f}i·{pretty_ket(n,i)}")
    if len(nz) > max_terms:
        parts.append("…")
    return " + ".join(parts)

# --------------------------- analytic amplitudes after 1 iteration ------------

def analytic_after_one_iter(N):
    """Return (a_marked, a_other) = ((3N−4)/(N√N), (N−4)/(N√N))."""
    import_den = (N * (N ** 0.5))
    return ( (3*N - 4)/import_den, (N - 4)/import_den )

# --------------------------- cases --------------------------------------------

def case1_oracle_on_marked():
    # qapply(OracleGate(2, …)*IntQubit(2))
    n, m = 2, 2
    inp = ket_int(n, m)
    out = oracle(n, m, inp)
    print("Case 1")
    print("======")
    print("Answer:")
    print(f"  Oracle on {pretty_ket(n,m)} → −{pretty_ket(n,m)}")
    print("\nReason why:")
    print("  The oracle flips the phase of the marked basis state and acts as identity on others.")
    print("\nCheck (harness):")
    print(f"  ‖state‖² = {norm2(out):.12f} (should be 1)")
    print(f"  amplitude[{pretty_ket(n,m)}] = {out[m].real:.1f} (should be −1)")
    print()

def case2_oracle_on_unmarked():
    # qapply(OracleGate(2, …)*IntQubit(3))
    n, m, k = 2, 2, 3
    inp = ket_int(n, k)
    out = oracle(n, m, inp)
    print("Case 2")
    print("======")
    print("Answer:")
    print(f"  Oracle on {pretty_ket(n,k)} (unmarked) → {pretty_ket(n,k)}")
    print("\nReason why:")
    print("  The oracle only flips the marked ket; all unmarked basis states are fixed.")
    print("\nCheck (harness):")
    print(f"  identical? {all(abs(out[i]-inp[i])<1e-15 for i in range(len(inp)))}")
    print()

def case3_one_iter_on_superposition_n2():
    # qapply(grover_iteration(superposition_basis(2), OracleGate(2, …)))
    n, m = 2, 2
    s = superposition(n)
    out = grover_iteration(n, m, s)
    print("Case 3")
    print("======")
    print("Answer:")
    print(f"  One Grover iteration from |s> (n=2) → {pretty_state(n, out)}  (i.e., {pretty_ket(n,m)})")
    print("\nReason why:")
    print("  For N=4, θ = arcsin(1/√N) = 30°. After one iteration the success prob is sin²(3θ)=1,")
    print("  so the state becomes exactly the marked basis state.")
    print("\nCheck (harness):")
    print(f"  ‖state‖² = {norm2(out):.12f} (should be 1)")
    # exact match
    basis = ket_int(n, m)
    eq = all(abs(out[i]-basis[i])<1e-12 for i in range(len(out)))
    print(f"  equals {pretty_ket(n,m)} exactly? {eq}")
    print()

def case4_one_iter_on_superposition_n4():
    # qapply(grover_iteration(superposition_basis(4), OracleGate(4, …)))
    n, m = 4, 2
    N = dim(n)
    s = superposition(n)
    out = grover_iteration(n, m, s)
    a_m_theory, a_o_theory = analytic_after_one_iter(N)  # 11/16 and 3/16
    # collect statistics
    amps = [out[i].real for i in range(N)]
    am_marked = amps[m]
    am_others = [amps[i] for i in range(N) if i != m]
    uniq_others = len({round(a,12) for a in am_others}) == 1
    print("Case 4")
    print("======")
    print("Answer:")
    print(f"  One Grover iteration from |s> (n=4, N=16).")
    print(f"  Marked amplitude   = {am_marked:.6f}  (expected 11/16 = 0.687500)")
    print(f"  Unmarked amplitude = {am_others[0]:.6f}  (expected  3/16 = 0.187500)")
    print("  (All unmarked amplitudes are equal.)")
    print("\nReason why:")
    print("  After oracle the mean amplitude is μ=(N−2)/(N√N). Diffusion reflects every amplitude")
    print("  about μ, yielding a'_m=(3N−4)/(N√N)=11/16 and a'_(other)=(N−4)/(N√N)=3/16 for N=16.")
    print("\nCheck (harness):")
    print(f"  ‖state‖² = {norm2(out):.12f} (should be 1)")
    print(f"  all unmarked equal? {uniq_others}")
    print(f"  a_marked  ≈ theory? {abs(am_marked - a_m_theory) < 1e-15}  (num={am_marked:.12f}, th={a_m_theory:.12f})")
    print(f"  a_unmarked≈ theory? {abs(am_others[0] - a_o_theory) < 1e-15}  (num={am_others[0]:.12f}, th={a_o_theory:.12f})")
    # Diffusion as reflection about mean
    after_oracle = oracle(n, m, s)
    mu = sum(after_oracle)/len(after_oracle)
    reflected = [2*mu - a for a in after_oracle]
    refl_ok = all(abs(reflected[i]-out[i])<1e-15 for i in range(len(out)))
    print(f"  diffusion = reflection about mean? {refl_ok}")
    print()

# --------------------------- driver -------------------------------------------

if __name__ == "__main__":
    case1_oracle_on_marked()
    case2_oracle_on_unmarked()
    case3_one_iter_on_superposition_n2()
    case4_one_iter_on_superposition_n4()

