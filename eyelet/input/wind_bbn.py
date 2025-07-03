#!/usr/bin/env python3
"""
wind_bbn_backward.py
──────────────────────────────────────────────────────────────
Goal-driven (backward) proof in a Bayesian Belief Network (BBN).

Scenario
========
A service centre monitors a single on-shore wind turbine.
We want to know whether we should *schedule maintenance soon* (R).

Network variables (all Boolean, True = high / present):
    A  AgeHigh           — turbine age ≥ 15 y
    M  MaintRecent       — maintenance done in last 6 months
    W  WindHigh          — current wind-speed high
    G  GearboxWearHigh   — gearbox wear above threshold
    V  VibrationHigh     — nacelle vibration alarm
    F  FaultPresent      — SCADA fault code active
    R  MaintainSoon      — **goal variable** (“needs maintenance”)

Graph structure / causal arcs:
    A ─┐
       ▼
       G ─▶ V ─▶ F ─▶ R
    M ─┘      ▲
              │
    W ─────────

Evidence (observations):
    A = True    (old turbine)
    M = False   (no recent maintenance)
    W = True    (wind is high)

We compute posterior
    P(R=True | evidence)
by *variable elimination*, i.e. “backward chaining” in the probabilistic
sense, and print every elimination step as proof evidence.
"""

# ─────────────────────────────────────────────────────────────
# 0.  Imports
# ─────────────────────────────────────────────────────────────
from collections import defaultdict
from itertools import product
from typing import Dict, Tuple

# ─────────────────────────────────────────────────────────────
# 1.  Conditional-probability tables (CPTs)
#     • Probabilities are illustrative—not taken from real data.
#     • All tables give P(variable = True | parents).
#       P(False | parents) = 1 − that value.
# ─────────────────────────────────────────────────────────────

# Priors (no parents)
P_A = {True: 0.4,  False: 0.6}    # older turbines 40 %
P_M = {True: 0.3,  False: 0.7}    # recent maintenance 30 %
P_W = {True: 0.5,  False: 0.5}    # high wind half the time

# Gearbox wear  P(G=True | A,M)
# Key order:  (AgeHigh , MaintRecent)
P_G = {
    (True,  True ): 0.30,
    (True,  False): 0.70,
    (False, True ): 0.05,
    (False, False): 0.20,
}

# Vibration  P(V=True | W,G)
P_V = {
    (True,  True ): 0.90,
    (True,  False): 0.50,
    (False, True ): 0.60,
    (False, False): 0.10,
}

# Fault  P(F=True | V,G)
P_F = {
    (True,  True ): 0.95,
    (True,  False): 0.70,
    (False, True ): 0.80,
    (False, False): 0.05,
}

# Maintenance decision  P(R=True | F)
P_R = {
    True : 0.9,     # If fault present, 90 % chance we schedule soon
    False: 0.1,
}

# ─────────────────────────────────────────────────────────────
# 2.  Evidence  (observed values)
# ─────────────────────────────────────────────────────────────
evidence = dict(A=True, M=False, W=True)

# ─────────────────────────────────────────────────────────────
# 3.  Joint-probability evaluator  P(complete assignment)
#     This is the “Bayesian logic” knowledge base.
# ─────────────────────────────────────────────────────────────
def joint(assign: Dict[str, bool]) -> float:
    """
    Return the joint probability of a *complete* world assignment.

    Parameters
    ----------
    assign : dict
        Maps every variable in {A,M,W,G,V,F,R} to a Boolean value.
    """
    p  = P_A[assign['A']] if assign['A'] else 1 - P_A[True]
    p *= P_M[assign['M']] if assign['M'] else 1 - P_M[True]
    p *= P_W[assign['W']] if assign['W'] else 1 - P_W[True]

    p *= P_G[(assign['A'], assign['M'])] if assign['G'] else \
         1 - P_G[(assign['A'], assign['M'])]

    p *= P_V[(assign['W'], assign['G'])] if assign['V'] else \
         1 - P_V[(assign['W'], assign['G'])]

    p *= P_F[(assign['V'], assign['G'])] if assign['F'] else \
         1 - P_F[(assign['V'], assign['G'])]

    p *= P_R[assign['F']]               if assign['R'] else 1 - P_R[assign['F']]
    return p

# ─────────────────────────────────────────────────────────────
# 4.  Variable-elimination proof  P(target=True | evidence)
# ─────────────────────────────────────────────────────────────
order = ['A', 'M', 'W', 'G', 'V', 'F', 'R']  # topological order

def eliminate(target: str, evidence: Dict[str, bool]) -> float:
    """
    Perform variable elimination (summing out hidden vars) and
    return posterior P(target=True | evidence).

    Proof evidence: print each world’s contribution before normalisation.
    """
    hidden = [v for v in order if v not in evidence and v != target]
    sum_true = sum_false = 0.0

    print("\nBackward-elimination proof:")
    for world_values in product([False, True], repeat=len(hidden)):
        # Build a complete assignment
        assign = dict(zip(hidden, world_values), **evidence)

        # Contribution when target = False
        assign[target] = False
        p_false = joint(assign)
        sum_false += p_false

        # Contribution when target = True
        assign[target] = True
        p_true = joint(assign)
        sum_true += p_true

        print(f"  hidden={dict(zip(hidden, world_values))}  "
              f"⇒ add False:{p_false:.6f}  True:{p_true:.6f}")

    total = sum_true + sum_false
    print(f"\nNormalisation:  True={sum_true:.6f}  False={sum_false:.6f}  "
          f"Total={total:.6f}")
    return sum_true / total

# ─────────────────────────────────────────────────────────────
# 5.  Run the proof for MaintainSoon (R)
# ─────────────────────────────────────────────────────────────
posterior = eliminate('R', evidence)

print(f"\nPosterior  P(MaintainSoon = True | evidence) = {posterior:.4f}")

THRESHOLD = 0.60   # arbitrary “proof” cut-off
if posterior > THRESHOLD:
    print("Verdict:  Maintenance is LIKELY required  (goal proved).")
else:
    print("Verdict:  Maintenance unlikely  (goal NOT proved).")

