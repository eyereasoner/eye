#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Ershov's mixed computation — biological evolution edition
(Felsenstein pruning specialized to a fixed tree and JC69 model)

What this program shows
-----------------------
Ershov’s mixed computation (partial evaluation) lets us pre-compute all work
that depends only on static inputs and generate a smaller, faster residual
program that awaits only the dynamic inputs.

Evolution example:
  - Model: JC69 nucleotide substitution with rate μ (we'll use μ = 1).
  - Static today (mix time):
      • a fixed unrooted tree on four taxa with branch lengths:
            ((A:0.10,B:0.20):0.30,(C:0.15,D:0.25))
        We root at the internal node joining A and B (this is OK for JC69
        because the model is time-reversible).
      • the JC69 transition matrices P_e for each branch length.
      • the structure of the Felsenstein pruning recurrences.
  - Dynamic tomorrow (run time):
      • a single alignment column: the nucleotides at tips A,B,C,D.

The residual program `site_ll(a,b,c,d)` contains no tree recursion, no matrix
construction, and no loops over states. It just picks a few precomputed numbers
based on the observed nucleotides and multiplies/adds them.

This script prints:
  1) "Answer"          — the generated residual code and a sample evaluation,
  2) "Reason why"      — what was decided at mix time (model, matrices, unroll),
  3) "Check (harness)" — a verifier comparing residual vs. a generic evaluator
                         on many site patterns.

Run with Python 3.x; no dependencies.
"""

from dataclasses import dataclass
from typing import Callable, List, Tuple
import math
import random


# ---------- Generic (unspecialized) JC69 + pruning for comparison ----------

NUC_TO_ID = {"A":0, "C":1, "G":2, "T":3}
ID_TO_NUC = "ACGT"

def jc69_P(t: float, mu: float = 1.0) -> List[List[float]]:
    """
    JC69 transition matrix for branch length t and rate mu.
      P_ii(t) = 1/4 + 3/4 * e^{-4 mu t / 3}
      P_ij(t) = 1/4 - 1/4 * e^{-4 mu t / 3}  (i != j)
    """
    e = math.exp(-4.0 * mu * t / 3.0)
    same = 0.25 + 0.75 * e
    diff = 0.25 - 0.25 * e
    P = [[diff]*4 for _ in range(4)]
    for i in range(4):
        P[i][i] = same
    return P

# Fixed tree and branch lengths (static):
# Root at internal node X; children: A (0.10), B (0.20), and Y (0.30),
# and Y has children: C (0.15) and D (0.25).
BLEN = {
    "A-X": 0.10,
    "B-X": 0.20,
    "C-Y": 0.15,
    "D-Y": 0.25,
    "Y-X": 0.30,
}

def site_likelihood_generic(a: int, b: int, c: int, d: int, mu: float = 1.0) -> float:
    """
    Generic Felsenstein pruning for our fixed 4-tip tree (computes per-site likelihood).
    Arguments a,b,c,d are 0..3 for A,C,G,T.
    """
    P_AX = jc69_P(BLEN["A-X"], mu)
    P_BX = jc69_P(BLEN["B-X"], mu)
    P_CY = jc69_P(BLEN["C-Y"], mu)
    P_DY = jc69_P(BLEN["D-Y"], mu)
    P_YX = jc69_P(BLEN["Y-X"], mu)

    # Tip -> parent partials
    AX = [P_AX[a][k] for k in range(4)]   # k is state at X
    BX = [P_BX[b][k] for k in range(4)]
    CY = [P_CY[c][r] for r in range(4)]   # r is state at Y
    DY = [P_DY[d][r] for r in range(4)]
    Y  = [CY[r] * DY[r] for r in range(4)]

    # Move Y partials across branch Y->X
    YtoX = [sum(P_YX[r][k] * Y[r] for r in range(4)) for k in range(4)]

    # Combine at root X and sum with uniform stationary π = 1/4
    X = [AX[k] * BX[k] * YtoX[k] for k in range(4)]
    return 0.25 * sum(X)


# ---------- Specialization artifact ----------

@dataclass
class SpecializationResult:
    func: Callable[[str, str, str, str], float]  # residual site_ll(a,b,c,d) → likelihood
    source: str
    trace: List[str]


def specialize_jc69_tree(mu: float = 1.0) -> SpecializationResult:
    """
    Mix time:
      - compute the five JC69 P-matrices we need,
      - emit a tiny, loop-free residual function for the fixed topology.
    """
    # Precompute transition matrices for each branch
    P_AX = jc69_P(BLEN["A-X"], mu)
    P_BX = jc69_P(BLEN["B-X"], mu)
    P_CY = jc69_P(BLEN["C-Y"], mu)
    P_DY = jc69_P(BLEN["D-Y"], mu)
    P_YX = jc69_P(BLEN["Y-X"], mu)

    # Helper to format a 4x4 matrix literal nicely
    def mat_literal(M: List[List[float]]) -> str:
        rows = [", ".join(f"{x:.16g}" for x in row) for row in M]
        return "[" + ", ".join("[" + r + "]" for r in rows) + "]"

    # Emit residual code (no loops over states; just indexed picks and a few sums)
    lines: List[str] = []
    lines.append("def site_ll(a, b, c, d):")
    lines.append('    """Residual JC69 per-site likelihood for ((A:0.10,B:0.20):0.30,(C:0.15,D:0.25)).')
    lines.append('    Inputs can be nucleotides "A/C/G/T" (case-insensitive) or integers 0..3."""')
    lines.append("    MAP = {'A':0,'C':1,'G':2,'T':3}")
    lines.append("    def to_id(x):")
    lines.append("        if isinstance(x, int):")
    lines.append("            i = x")
    lines.append("        else:")
    lines.append("            s = str(x).strip().upper()")
    lines.append("            if s and s[0] in MAP: i = MAP[s[0]]")
    lines.append("            else: raise ValueError('bad nucleotide: %r' % (x,))")
    lines.append("        if not (0 <= i < 4): raise ValueError('nuc id out of range')")
    lines.append("        return i")
    lines.append("    a = to_id(a); b = to_id(b); c = to_id(c); d = to_id(d)")
    lines.append(f"    P_AX = {mat_literal(P_AX)}")
    lines.append(f"    P_BX = {mat_literal(P_BX)}")
    lines.append(f"    P_CY = {mat_literal(P_CY)}")
    lines.append(f"    P_DY = {mat_literal(P_DY)}")
    lines.append(f"    P_YX = {mat_literal(P_YX)}")
    lines.append("    # Tip -> parent partials")
    lines.append("    AX0,AX1,AX2,AX3 = P_AX[a][0],P_AX[a][1],P_AX[a][2],P_AX[a][3]")
    lines.append("    BX0,BX1,BX2,BX3 = P_BX[b][0],P_BX[b][1],P_BX[b][2],P_BX[b][3]")
    lines.append("    CY0,CY1,CY2,CY3 = P_CY[c][0],P_CY[c][1],P_CY[c][2],P_CY[c][3]")
    lines.append("    DY0,DY1,DY2,DY3 = P_DY[d][0],P_DY[d][1],P_DY[d][2],P_DY[d][3]")
    lines.append("    Y0 = CY0*DY0; Y1 = CY1*DY1; Y2 = CY2*DY2; Y3 = CY3*DY3")
    lines.append("    # Move Y across Y->X")
    lines.append("    YX0 = P_YX[0][0]*Y0 + P_YX[1][0]*Y1 + P_YX[2][0]*Y2 + P_YX[3][0]*Y3")
    lines.append("    YX1 = P_YX[0][1]*Y0 + P_YX[1][1]*Y1 + P_YX[2][1]*Y2 + P_YX[3][1]*Y3")
    lines.append("    YX2 = P_YX[0][2]*Y0 + P_YX[1][2]*Y1 + P_YX[2][2]*Y2 + P_YX[3][2]*Y3")
    lines.append("    YX3 = P_YX[0][3]*Y0 + P_YX[1][3]*Y1 + P_YX[2][3]*Y2 + P_YX[3][3]*Y3")
    lines.append("    # Combine at root X, then weight by uniform stationary distribution π=1/4")
    lines.append("    X0 = AX0*BX0*YX0")
    lines.append("    X1 = AX1*BX1*YX1")
    lines.append("    X2 = AX2*BX2*YX2")
    lines.append("    X3 = AX3*BX3*YX3")
    lines.append("    return 0.25*(X0+X1+X2+X3)")
    source = "\n".join(lines)

    # Materialize the residual function
    ns = {}
    exec(source, ns)  # safe: code generated locally from numeric constants
    fn = ns["site_ll"]

    # Build a human-readable reasoning trace
    trace: List[str] = []
    trace.append("Model fixed at mix time: JC69 with μ=1 and stationary π=(1/4,1/4,1/4,1/4).")
    trace.append("Tree fixed at mix time: ((A:0.10,B:0.20):0.30,(C:0.15,D:0.25)). Rooted at the internal node joining A and B.")
    trace.append("For each branch length t we precompute the 4x4 transition matrix P(t).")
    trace.append("Pruning unrolled at mix time:")
    trace.append("  - From tips to X: AX[k]=P_AX[a,k], BX[k]=P_BX[b,k].")
    trace.append("  - From tips to Y: CY[r]=P_CY[c,r], DY[r]=P_DY[d,r], then Y[r]=CY[r]*DY[r].")
    trace.append("  - Move Y across Y->X: YX[k]=Σ_r P_YX[r,k]*Y[r].")
    trace.append("  - Root combine: likelihood = (1/4)*Σ_k AX[k]*BX[k]*YX[k].")
    trace.append("All sums/products above are emitted as straight-line arithmetic in the residual program.")
    return SpecializationResult(func=fn, source=source, trace=trace)


# ---------- Pretty-printers for the requested sections ----------

def print_answer(spec: SpecializationResult) -> None:
    print("Answer")
    print("------")
    print("Residual program (generated at mix time):")
    print(spec.source)
    # Sample site (choose one somewhat asymmetric pattern)
    ex = ("A","C","G","T")
    val = spec.func(*ex)
    print(f"Example evaluation: for states A={ex[0]}, B={ex[1]}, C={ex[2]}, D={ex[3]} → site_ll = {val:.12g}")
    print()

def print_reason(spec: SpecializationResult) -> None:
    print("Reason why")
    print("----------")
    for line in spec.trace:
        print("•", line)
    print()

def print_check(spec: SpecializationResult, trials: int = 500) -> None:
    print("Check (harness)")
    print("----------------")
    random.seed(20250827)
    # Exhaustive over all 256 patterns is small; do both a few randoms and a full sweep.
    # Full sweep:
    max_err = 0.0
    for a in range(4):
        for b in range(4):
            for c in range(4):
                for d in range(4):
                    want = site_likelihood_generic(a,b,c,d, mu=1.0)
                    got  = spec.func(ID_TO_NUC[a], ID_TO_NUC[b], ID_TO_NUC[c], ID_TO_NUC[d])
                    err = abs(want - got)
                    if err > max_err:
                        max_err = err
                    if err > 1e-12:
                        print("FAIL (exhaustive): mismatch at pattern",
                              ID_TO_NUC[a]+ID_TO_NUC[b]+ID_TO_NUC[c]+ID_TO_NUC[d],
                              f"expected {want:.16g}, got {got:.16g}")
                        print()
                        return
    # Spot a few random strings too (just to show usage)
    for _ in range(trials):
        ex = tuple(random.choice("ACGT") for _ in range(4))
        want = site_likelihood_generic(*(NUC_TO_ID[x] for x in ex), mu=1.0)
        got  = spec.func(*ex)
        if abs(want - got) > 1e-12:
            print("FAIL (random):", ex, "expected", want, "got", got)
            print()
            return
    print(f"PASS: residual matches generic pruning on all 256 patterns (max abs error {max_err:.2e}) and {trials} random checks.")
    print()


# ---------- Main demo ----------

def main() -> None:
    spec = specialize_jc69_tree(mu=1.0)
    print_answer(spec)
    print_reason(spec)
    print_check(spec, trials=300)


if __name__ == "__main__":
    main()

