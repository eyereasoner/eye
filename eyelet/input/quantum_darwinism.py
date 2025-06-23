"""
Quantum Darwinism mini‑demo                     (pure Python + NumPy)
=====================================================================
This tiny script illustrates the **redundant storage of classical
information** that characterises *quantum Darwinism*.

Model
-----
* **Qubits** : 1 system qubit **S**  +  `N_ENV` environment qubits
  `E₀ … E_{N−1}`.
* **Initial state** :
    |ψ₀⟩  =  (|0⟩ + |1⟩)/√2   ⊗   |0⟩^{⊗N}.
* **Interaction** : each environment qubit scatters exactly once via a CNOT
  whose **control is S** and **target is Eₖ**:

        U_int  =  ∏_{k=0}^{N−1}  CNOT(S → Eₖ).

  The final state is the (N+1)‑qubit GHZ state

        |Ψ⟩ = (|0⟩_S |0…0⟩_E  +  |1⟩_S |1…1⟩_E) / √2 .

Observable
----------
For a fragment **F** containing the first `f` environment qubits we compute the
*quantum mutual information*

    I(S : F) = S(S) + S(F) − S(SF),

where S(·) is the von‑Neumann entropy in **bits**.

Expected “P‑shaped” curve
-------------------------
* **Plateau (classical redundancy)**  –  A *single* environment qubit already
  carries the full classical bit held by S, so

        I(S:F) ≈ 1 bit = S(S)   for 1 ≤ f ≪ N.

  Adding further qubits does **not** increase classical information: many
  independent fragments agree on the pointer value.

* **Rise to 2 bits (quantum part)** –  Only when the fragment becomes almost
  the *entire* environment does it begin to capture the *quantum phase*
  information as well.  For a **pure global state** we have S(SF) = 0 when
  F = E, which enforces

        I(S : E) = 2 · S(S) = 2 bits.

Thus the end‑point at 2 bits is a *feature*, not a bug.

Running the script prints a table

    fragment_size  mutual_info(bits)

so you can watch the plateau around 1 bit and the final rise to 2 bits.  Try
changing `N_ENV`, injecting noise, or replacing the CNOT with a partial‑phase
interaction to explore how redundancy and decoherence interplay.
"""

import numpy as np
from numpy.linalg import eigh

# ── small helpers ─────────────────────────────────────────────────────────────

def apply_cnot(state: np.ndarray, control: int, target: int) -> np.ndarray:
    """Return (CNOT_{control→target}) |state⟩.

    The qubits are labelled 0…n‑1 (LSB‑first); `state` is a 2**n vector.
    """
    new = state.copy()
    for idx in range(len(state)):
        if (idx >> control) & 1:                     # control qubit is |1⟩ ?
            flip = idx ^ (1 << target)              # toggle target bit
            new[flip] = state[idx]
            new[idx]  = state[flip]
    return new


def partial_trace(rho: np.ndarray, keep: list[int], dims: list[int]) -> np.ndarray:
    """Trace out all subsystems *except* those in `keep`.

    Args:
        rho  :  density matrix of the full system (square 2**n array)
        keep :  list of subsystem indices to retain (0‑based; order irrelevant)
        dims :  Hilbert‑space dimensions (here always `[2]*n`)
    Returns:
        reduced density matrix ρ_keep.
    """
    keep   = sorted(keep)
    dk     = np.prod([dims[i] for i in keep])       # dimension of kept space
    dr     = rho.shape[0] // dk                    # dimension of the rest

    # Reshape → permute kept blocks to front → reshape back → trace rest
    order  = keep + [i for i in range(len(dims)) if i not in keep]
    rho4d  = rho.reshape([2]*len(dims)*2)
    rho4d  = rho4d.transpose(order + [i+len(dims) for i in order])
    rho4d  = rho4d.reshape(dk, dr, dk, dr)
    return rho4d.trace(axis1=1, axis2=3).reshape(dk, dk)


def von_neumann_entropy(rho: np.ndarray) -> float:
    """Return S(ρ) in **bits** (base‑2 logarithm)."""
    eigs = np.clip(eigh(rho)[0].real, 0, 1)
    nz   = eigs[eigs > 0]
    return float(-(nz * np.log2(nz)).sum())

# ── parameters ────────────────────────────────────────────────────────────────
N_ENV = 8                    # number of environment qubits (≤10 keeps RAM comfy)
dims  = [2] * (N_ENV + 1)    # individual Hilbert‑space dimensions

# ── build initial |ψ₀⟩ = (|0⟩+|1⟩)/√2 ⊗ |0…0⟩ ──────────────────────────────────
psi = np.zeros(2**(N_ENV + 1), dtype=complex)
psi[0]           = 1/np.sqrt(2)        # |0_S 0…0_E⟩
psi[1 << N_ENV]  = 1/np.sqrt(2)        # |1_S 0…0_E⟩  (system is MSB)

# ── scatter S with every env qubit (CNOT chain) ──────────────────────────────
for k in range(N_ENV):
    psi = apply_cnot(psi, control=N_ENV, target=k)

# ── global density matrix ────────────────────────────────────────────────────
rho_global = np.outer(psi, psi.conj())

# entropy of bare system (should be exactly 1 bit)
rho_S = partial_trace(rho_global, keep=[N_ENV], dims=dims)
S_S   = von_neumann_entropy(rho_S)
print(f"System entropy S(S) = {S_S:.3f} bits\n")

# ── Darwinism curve I(S:F) vs |F| ─────────────────────────────────────────────
print("fragment_size  mutual_info(bits)")
for f in range(1, N_ENV + 1):
    keep_F  = list(range(f))                    # first f env qubits
    rho_F   = partial_trace(rho_global, keep=keep_F, dims=dims)
    rho_SF  = partial_trace(rho_global, keep=keep_F + [N_ENV], dims=dims)

    S_F  = von_neumann_entropy(rho_F)
    S_SF = von_neumann_entropy(rho_SF)
    I    = S_S + S_F - S_SF

    print(f"      {f:<2}            {I:.3f}")

