#!/usr/bin/env python3
"""
bbn.py
A backward-chaining “proof” in a Bayesian Belief Network.

Variables (binary):
    I  – Industry     {high, low}
    A  – Awareness    {high, low}
    P  – Pollution    {high, low}
    E  – EnvQual      {neg, ok}

Structure:
    I ─▶ P
    A ─▶ P
    P ─▶ E

We compute P(E = neg  |  I=high , A=low) and show every
variable-elimination step as proof evidence.
"""

from collections import defaultdict
from itertools import product
from typing import Dict, Tuple

# ─────────────────────────────────────────────────────────────
# 1.  Conditional-probability tables (CPTs)
#     All numbers are invented but plausible.
# ─────────────────────────────────────────────────────────────
P_I: Dict[bool, float] = {True: 0.8, False: 0.2}            # P(I = high)
P_A: Dict[bool, float] = {True: 0.3, False: 0.7}            # P(A = high)

# P(Pollution = high | I, A)
P_P: Dict[Tuple[bool, bool], float] = {
    (True,  True ): 0.9,
    (True,  False): 0.8,
    (False, True ): 0.4,
    (False, False): 0.2,
}

# P(EnvQual = negative | Pollution)
P_E: Dict[bool, float] = {
    True : 0.95,
    False: 0.10,
}

# ─────────────────────────────────────────────────────────────
# 2.  Evidence  (I = high,  A = low)
# ─────────────────────────────────────────────────────────────
evidence = dict(I=True, A=False)

# ─────────────────────────────────────────────────────────────
# 3.  Joint-probability evaluator
# ─────────────────────────────────────────────────────────────
def joint(assign: Dict[str, bool]) -> float:
    """Return joint probability of a complete assignment."""
    p  = P_I[assign['I']] if assign['I'] else 1 - P_I[True]
    p *= P_A[assign['A']] if assign['A'] else 1 - P_A[True]
    p *= P_P[(assign['I'], assign['A'])] if assign['P'] else 1 - P_P[(assign['I'], assign['A'])]
    p *= P_E[assign['P']]               if assign['E'] else 1 - P_E[assign['P']]
    return p

# ─────────────────────────────────────────────────────────────
# 4.  Backward elimination proof  P(target=True | evidence)
# ─────────────────────────────────────────────────────────────
def eliminate(target: str, evidence: Dict[str, bool]) -> float:
    hidden = [v for v in ['I','A','P','E'] if v not in evidence and v != target]
    factor_false = factor_true = 0.0

    print("\nBackward-elimination proof:")
    for assignment in product([False, True], repeat=len(hidden)):
        world = dict(zip(hidden, assignment), **evidence)

        # accumulate for target = False
        world[target] = False
        pr_false = joint(world)
        factor_false += pr_false

        # accumulate for target = True
        world[target] = True
        pr_true  = joint(world)
        factor_true  += pr_true

        # show partial evidence
        print(f"  Σ  P(world)  with  {target}=False: {pr_false:.6f}   "
              f"|  {target}=True: {pr_true:.6f}")

    total = factor_true + factor_false
    print(f"\nNormalisation:  True={factor_true:.6f} , False={factor_false:.6f} , "
          f"total={total:.6f}")
    return factor_true / total

# ─────────────────────────────────────────────────────────────
# 5.  Run proof for sign(EnvQual, negative)
# ─────────────────────────────────────────────────────────────
posterior = eliminate('E', evidence)

print(f"\nPosterior  P(E = negative | I=high , A=low) = {posterior:.4f}")

if posterior > 0.5:
    print("Verdict:  sign(E, negative)  is PROVED (posterior > 0.5).")
else:
    print("Verdict:  sign(E, negative)  is NOT proved (posterior ≤ 0.5).")

