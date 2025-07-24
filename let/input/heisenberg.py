#!/usr/bin/env python3
"""
heisenberg.py
------------------------------------------------
A matrix-mechanics demo of three uncertainty relations
for a single electron in 1-D:

  1.  Position X  vs.  Momentum P        (canonical pair)
  2.  Spin Sx     vs.  Spin Sz           (orthogonal spin components)
  3.  Position X  vs.  Spin Sz           (commuting observables)

We build the direct-product Hilbert space
   H_total = H_space ⊗ H_spin,
where  H_space  is a truncated harmonic-oscillator basis
(N levels) and  H_spin  is the 2-dimensional spin-½ space.

Throughout the script  ℏ = 1,  m = 1,  ω = 1.

Run:
    python heisenberg.py
------------------------------------------------
"""

import numpy as np


# ----------------------------------------------------------------------
#  Matrix-mechanics utilities
# ----------------------------------------------------------------------
def comm(A, B):
    """Matrix commutator  [A, B] = AB − BA."""
    return A @ B - B @ A


def creation_annihilation(N):
    """Return annihilation a and creation a† operators for an N-level HO."""
    a = np.zeros((N, N), dtype=complex)
    for n in range(1, N):
        a[n - 1, n] = np.sqrt(n)
    return a, a.conj().T


def ho_operators(N):
    """Position X, momentum P, and Hamiltonian H for ℏ = m = ω = 1."""
    a, adag = creation_annihilation(N)
    X = (adag + a) / np.sqrt(2)
    P = 1j * (adag - a) / np.sqrt(2)
    H = 0.5 * (P @ P + X @ X)
    return X, P, H


def uncertainty(state, A):
    """
    Return the standard deviation ΔA =
        sqrt(⟨A²⟩ − ⟨A⟩²)
    for a *normalized* state vector |ψ⟩.
    """
    mean = (state.conj().T @ (A @ state)).item()
    mean2 = (state.conj().T @ (A @ A @ state)).item()
    var = mean2 - mean**2
    return np.sqrt(var.real)   # imaginary part ≈ 0 to numerical noise


# ----------------------------------------------------------------------
#  Main demonstration
# ----------------------------------------------------------------------
def main():
    # ----- 1. Build operators -------------------------------------------------
    N = 12                           # oscillator truncation; raise for accuracy
    X, P, _ = ho_operators(N)

    # Pauli matrices → spin-½ operators  S = σ / 2   (ℏ = 1)
    σx = np.array([[0, 1], [1,  0]], dtype=complex)
    σy = np.array([[0, -1j], [1j, 0]], dtype=complex)
    σz = np.array([[1, 0], [0, -1]], dtype=complex)
    Sx, Sy, Sz = 0.5 * σx, 0.5 * σy, 0.5 * σz

    # Kronecker-lift to the combined space
    I_space = np.eye(N)
    I_spin  = np.eye(2)
    X_full  = np.kron(X, I_spin)
    P_full  = np.kron(P, I_spin)
    Sx_full = np.kron(I_space, Sx)
    Sy_full = np.kron(I_space, Sy)
    Sz_full = np.kron(I_space, Sz)

    # ----- 2. Choose a test state |ψ⟩ ----------------------------------------
    #   Spatial part: HO ground state  |0⟩
    #   Spin part   : (|↑⟩ + i|↓⟩) / √2   →  maximal S_y expectation
    ground = np.zeros(N); ground[0] = 1.0
    spinor = np.array([1.0, 1.0j]) / np.sqrt(2)
    ψ = np.kron(ground, spinor)
    ψ = ψ / np.linalg.norm(ψ)        # make sure it's normalized

    # ----- 3. Compute uncertainties and bounds -------------------------------
    ΔX  = uncertainty(ψ, X_full)
    ΔP  = uncertainty(ψ, P_full)
    ΔSx = uncertainty(ψ, Sx_full)
    ΔSz = uncertainty(ψ, Sz_full)

    # General Heisenberg–Robertson bound:  ΔA ΔB ≥ ½|⟨[A,B]⟩|
    def bound(A, B):
        return 0.5 * abs((ψ.conj().T @ (comm(A, B) @ ψ)).item())

    bound_XP   = bound(X_full, P_full)
    bound_SxSz = bound(Sx_full, Sz_full)
    bound_XSz  = bound(X_full, Sz_full)

    # ----- 4. Print results ---------------------------------------------------
    print("Hilbert-space dimension :", X_full.shape[0], "= N × 2 =", N, "× 2")
    print("\nHeisenberg uncertainty checks (ℏ = 1)")
    print("---------------------------------------")
    header = "{:<15} {:>10} {:>15}".format("Observable pair",
                                           "ΔA·ΔB",
                                           "½|⟨[A,B]⟩|")
    print(header)
    print("-" * len(header))
    print("{:<15} {:10.4f} {:15.4f}".format("X  &  P",
                                            ΔX * ΔP, bound_XP))
    print("{:<15} {:10.4f} {:15.4f}".format("Sx & Sz",
                                            ΔSx * ΔSz, bound_SxSz))
    print("{:<15} {:10.4f} {:15.4f}".format("X  & Sz",
                                            ΔX * ΔSz, bound_XSz))
    # ------ 5. Interpret ------------------------------------------------------
    print("\nInterpretation:")
    print("  •  X & P   → ground-state oscillator *saturates* the canonical bound.")
    print("  •  Sx & Sz → orthogonal spin components also saturate the bound.")
    print("  •  X & Sz  → they commute, so the lower bound is zero;")
    print("               ΔX and ΔSz are finite but *independent*.")


# ----------------------------------------------------------------------
if __name__ == "__main__":
    main()

