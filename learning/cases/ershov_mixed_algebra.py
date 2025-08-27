#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Ershov's mixed computation — algebra edition (solving Ax = b with static A).

What this program shows
-----------------------
Ershov’s *mixed computation* (partial evaluation) lets us pre-compute all work
that depends only on *static* inputs and generate a smaller, faster *residual
program* that awaits only the *dynamic* inputs.

Algebra example:
  - Generic problem: solve the 3×3 linear system A x = b.
  - Mixed computation: if A is *static* (known now) and only b is *dynamic*
    (arrives later), we can decompose A = P·L·U today (with partial pivoting)
    and emit a *residual solver* `solve_A(b1, b2, b3)` that performs:
        y  = L^{-1} (P b)      (forward substitution)
        x  = U^{-1} y          (back substitution)
    No run-time pivoting, no row swaps, no matrix multiplications left — just a
    tiny sequence of affine ops with baked-in constants.

The program prints three sections:

1) "Answer" — the generated residual code and a sample evaluation,
2) "Reason why" — a concise explanation of the mix-time LU decisions,
3) "Check (harness)" — a test harness that verifies the residual solver
   matches a generic solver on many random right-hand sides.

Run it directly with Python 3.x; no dependencies.
"""

from dataclasses import dataclass
from typing import Callable, List, Tuple
import random

EPS = 1e-12


# ---------- Generic (unspecialized) solver for comparison ----------

def lu_decompose_3x3(A: List[List[float]]) -> Tuple[List[int], List[List[float]], List[List[float]], List[str]]:
    """
    Partial-pivoting LU decomposition of a 3×3 matrix.
    Returns:
      perm: row permutation such that P*A = L*U; row i of P*A came from original row perm[i]
      L: unit-lower-triangular (diag = 1)
      U: upper-triangular
      trace: human-readable steps (pivots, swaps, multipliers)
    """
    # Work on a copy
    a = [row[:] for row in A]
    n = 3
    perm = [0, 1, 2]
    L = [[1.0 if i == j else 0.0 for j in range(n)] for i in range(n)]
    U = [[0.0]*n for _ in range(n)]
    trace: List[str] = []

    # k = 0 pivot
    piv0 = max(range(0, n), key=lambda i: abs(a[i][0]))
    if abs(a[piv0][0]) < EPS:
        raise ValueError("Singular/ill-conditioned matrix at first pivot.")
    if piv0 != 0:
        a[0], a[piv0] = a[piv0], a[0]
        perm[0], perm[piv0] = perm[piv0], perm[0]
        if L[0][0] != 1.0 or L[piv0][piv0] != 1.0:
            pass  # no stored multipliers yet to swap
        trace.append(f"Swap rows 0 and {piv0} for |pivot| maximization.")
    trace.append(f"Pivot at (0,0) = {a[0][0]:.8g}")

    # Eliminate below pivot 0
    L[1][0] = a[1][0] / a[0][0]
    L[2][0] = a[2][0] / a[0][0]
    trace.append(f"l21 = a21/a11 = {L[1][0]:.8g}")
    trace.append(f"l31 = a31/a11 = {L[2][0]:.8g}")
    for j in range(0, n):
        a[1][j] -= L[1][0] * a[0][j]
        a[2][j] -= L[2][0] * a[0][j]

    # k = 1 pivot (among rows 1..2)
    piv1_offset = 1 + max(range(1, n), key=lambda i: abs(a[i][1]))
    if abs(a[piv1_offset][1]) < EPS:
        raise ValueError("Singular/ill-conditioned matrix at second pivot.")
    if piv1_offset != 1:
        a[1], a[piv1_offset] = a[piv1_offset], a[1]
        perm[1], perm[piv1_offset] = perm[piv1_offset], perm[1]
        # Swap corresponding L multipliers below column 0
        L[1][0], L[piv1_offset][0] = L[piv1_offset][0], L[1][0]
        trace.append(f"Swap rows 1 and {piv1_offset} for |pivot| maximization.")
    trace.append(f"Pivot at (1,1) = {a[1][1]:.8g}")

    # Eliminate below pivot 1
    L[2][1] = a[2][1] / a[1][1]
    trace.append(f"l32 = a32/a22 = {L[2][1]:.8g}")
    for j in range(1, n):
        a[2][j] -= L[2][1] * a[1][j]

    # Now 'a' holds U
    for i in range(n):
        for j in range(i, n):
            U[i][j] = a[i][j]

    trace.append("Assemble L (unit lower) and U (upper) such that P·A = L·U.")
    trace.append(f"Permutation (rows in P·A come from original rows): perm = {perm}")
    return perm, L, U, trace


def lu_solve_3x3(perm: List[int], L: List[List[float]], U: List[List[float]], b: Tuple[float, float, float]) -> Tuple[float, float, float]:
    """Solve A x = b given A's LU with permutation described by 'perm'."""
    # Apply permutation: (P b)[i] = b[perm[i]]
    Pb0, Pb1, Pb2 = b[perm[0]], b[perm[1]], b[perm[2]]

    # Forward substitution: L y = P b  (L has unit diagonal)
    y0 = Pb0
    y1 = Pb1 - L[1][0]*y0
    y2 = Pb2 - L[2][0]*y0 - L[2][1]*y1

    # Back substitution: U x = y
    if abs(U[2][2]) < EPS:
        raise ValueError("Singular matrix: zero pivot in U33.")
    x2 = y2 / U[2][2]

    if abs(U[1][1]) < EPS:
        raise ValueError("Singular matrix: zero pivot in U22.")
    x1 = (y1 - U[1][2]*x2) / U[1][1]

    if abs(U[0][0]) < EPS:
        raise ValueError("Singular matrix: zero pivot in U11.")
    x0 = (y0 - U[0][1]*x1 - U[0][2]*x2) / U[0][0]

    return (x0, x1, x2)


def solve_generic(A: List[List[float]], b: Tuple[float, float, float]) -> Tuple[float, float, float]:
    """One-shot generic solve: compute LU at run time, then solve."""
    perm, L, U, _ = lu_decompose_3x3(A)
    return lu_solve_3x3(perm, L, U, b)


# ---------- Specialization result container ----------

@dataclass
class SpecializationResult:
    func: Callable[[float, float, float], Tuple[float, float, float]]  # residual: b → x
    source: str                                                        # generated Python source
    trace: List[str]                                                   # human-readable mix-time reasoning


# ---------- The "mixer": partially evaluate w.r.t. the static matrix A ----------

def specialize_solver_for_A(A: List[List[float]]) -> SpecializationResult:
    """
    Given static 3×3 matrix A, generate residual function solve_A(b1, b2, b3)
    that returns x solving A x = b. We precompute P, L, U at mix time.
    """
    perm, L, U, steps = lu_decompose_3x3(A)

    # Build residual code
    lines: List[str] = []
    func_name = "solve_A"
    lines.append(f"def {func_name}(b0, b1, b2):")
    lines.append(f"    \"\"\"Residual solver for A x = b (A specialized at mix time).\"\"\"")
    # Bake constants
    lines.append(f"    # Permutation: (P b)[i] = b[perm[i]]")
    lines.append(f"    perm0, perm1, perm2 = {perm[0]}, {perm[1]}, {perm[2]}")
    lines.append(f"    # L (unit diag) multipliers")
    lines.append(f"    l21, l31, l32 = {L[1][0]:.16g}, {L[2][0]:.16g}, {L[2][1]:.16g}")
    lines.append(f"    # U upper-triangular entries")
    lines.append(f"    u11, u12, u13 = {U[0][0]:.16g}, {U[0][1]:.16g}, {U[0][2]:.16g}")
    lines.append(f"    u22, u23      = {U[1][1]:.16g}, {U[1][2]:.16g}")
    lines.append(f"    u33           = {U[2][2]:.16g}")
    lines.append("")
    # Permute b
    lines.append(f"    # Apply P to b")
    lines.append(f"    Pb0 = (b0, b1, b2)[perm0]")
    lines.append(f"    Pb1 = (b0, b1, b2)[perm1]")
    lines.append(f"    Pb2 = (b0, b1, b2)[perm2]")
    lines.append("")
    # Forward substitution
    lines.append(f"    # Forward substitution: L y = P b")
    lines.append(f"    y0 = Pb0")
    lines.append(f"    y1 = Pb1 - l21*y0")
    lines.append(f"    y2 = Pb2 - l31*y0 - l32*y1")
    lines.append("")
    # Back substitution
    lines.append(f"    # Back substitution: U x = y")
    lines.append(f"    x2 = y2 / u33")
    lines.append(f"    x1 = (y1 - u23*x2) / u22")
    lines.append(f"    x0 = (y0 - u12*x1 - u13*x2) / u11")
    lines.append(f"    return (x0, x1, x2)")

    source = "\n".join(lines)

    # Materialize the function from the generated source.
    namespace: dict = {}
    exec(source, namespace)  # safe: code generated locally from numeric constants
    residual_func = namespace[func_name]

    # Build reasoning trace
    trace: List[str] = []
    trace.append("We treat A as *static* and b as *dynamic*.")
    trace.append("At mix time we compute the partial-pivoting LU: P·A = L·U (L unit lower, U upper).")
    trace.extend(steps)
    trace.append("The residual solver performs only: P-apply to b, forward-substitution (L), back-substitution (U).")
    trace.append("No run-time pivot search or row swaps remain; their outcomes are baked into the permutation and multipliers.")
    return SpecializationResult(func=residual_func, source=source, trace=trace)


# ---------- Pretty-printers for the three requested sections ----------

def print_answer(spec: SpecializationResult, example_b: Tuple[float, float, float]) -> None:
    print("Answer")
    print("------")
    print("Residual program (generated at mix time):")
    print(spec.source)
    b0, b1, b2 = example_b
    x = spec.func(b0, b1, b2)
    print(f"Example evaluation: for b = {example_b}, solve_A(b) = {tuple(f'{v:.6g}' for v in x)}")
    print()


def print_reason(spec: SpecializationResult) -> None:
    print("Reason why")
    print("----------")
    for line in spec.trace:
        print("•", line)
    print()


def print_check(spec: SpecializationResult, A: List[List[float]], trials: int = 200) -> None:
    print("Check (harness)")
    print("----------------")
    random.seed(4242)

    failures: List[Tuple[Tuple[float, float, float], Tuple[float, float, float], Tuple[float, float, float]]] = []
    for _ in range(trials):
        # Generate a random right-hand side with a range of magnitudes
        b = (random.uniform(-10, 10), random.uniform(-10, 10), random.uniform(-10, 10))
        x_res = spec.func(*b)
        x_gen = solve_generic(A, b)
        # Compare with a small tolerance
        ok = all(abs(x_res[i] - x_gen[i]) <= 1e-9 * (1.0 + abs(x_gen[i])) for i in range(3))
        if not ok:
            failures.append((b, x_gen, x_res))

    if not failures:
        print(f"PASS: residual solver matches the generic solver on {trials} random right-hand sides.")
    else:
        print(f"FAIL: {len(failures)} mismatches found. Showing up to 5 examples:")
        for (b, want, got) in failures[:5]:
            print(f"  b={tuple(f'{v:.6g}' for v in b)}")
            print(f"    expected x={tuple(f'{v:.6g}' for v in want)}, got x={tuple(f'{v:.6g}' for v in got)}")
    print()


# ---------- Main demo ----------

def main() -> None:
    # Choose the static and dynamic parts:
    # - Matrix A is *static* (we know it now),
    # - Right-hand side b is *dynamic* (unknown until run time).
    A = [
        [4.0,  2.0,  1.0],
        [1.0,  3.0,  2.0],
        [0.5, -1.0,  2.0],
    ]
    # Build residual solver specialized to A
    spec = specialize_solver_for_A(A)

    # 1) Show the residual code and a sample evaluation
    example_b = (7.0, 4.0, 1.5)
    print_answer(spec, example_b)

    # 2) Explain the mix-time LU decisions
    print_reason(spec)

    # 3) Verify equivalence vs. the generic implementation
    print_check(spec, A, trials=500)


if __name__ == "__main__":
    main()

