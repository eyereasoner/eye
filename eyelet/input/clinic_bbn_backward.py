#!/usr/bin/env python3
"""
clinic_bbn_backward.py
─────────────────────────────────────────────────────────────
Goal-directed (“backward”) variable-elimination proof in a
small clinical Bayesian network.

Variables (all Boolean, True = present/positive)
------------------------------------------------
  V   ViralSeason        (influenza season)
  C   Cough              (patient coughs)
  F   Fever              (temperature > 38 °C)
  I   InfluenzaInfx      (true viral infection)
  P   PCR_Positive       (PCR nasal swab positive)
  H   HighRisk           (age >65 or chronic cond.)
  A   StartAntivirals    (decision variable, goal)

Structure
---------
  ViralSeason ─┐
               ▼
               I ─▶ P ─▶ A
  Cough ───────┤     ▲
  Fever ───────┘     │
                     │
  HighRisk ──────────┘

CPTs are illustrative, not clinical guidelines!

Evidence
--------
  ViralSeason = True
  Cough       = True
  Fever       = True
  HighRisk    = True
  PCR_Positive = *not available yet*   (latent)

Goal
----
Prove that  P(A=True | evidence) > 0.7
i.e. show that “**start antivirals**” is more likely than not.

The script prints:
  • each elimination row,
  • normalisation step,
  • final posterior & verdict.
"""

from itertools import product
from typing import Dict, Tuple

# ─────────────────────────────────────────────────────────────
# 1.  Conditional Probability Tables (toy numbers)
# ─────────────────────────────────────────────────────────────
# Priors
P_V = {True: 0.4, False: 0.6}                # influenza season
P_C = {True: 0.3, False: 0.7}                # baseline cough
P_F = {True: 0.2, False: 0.8}                # baseline fever
P_H = {True: 0.25, False: 0.75}              # high-risk pop

# Influenza infection | ViralSeason, Cough, Fever
P_I = {
    # (V, C, F)  →  P(I=True | parents)
    (True,  True,  True):  0.9,
    (True,  True,  False): 0.6,
    (True,  False, True):  0.7,
    (True,  False, False): 0.3,
    (False, True,  True):  0.4,
    (False, True,  False): 0.2,
    (False, False, True):  0.2,
    (False, False, False): 0.05,
}

# PCR result | Infection
P_P = {True: 0.95, False: 0.05}   # sensitivity / 1-specificity

# Decision node  StartAntivirals | PCR, HighRisk
P_A = {
    (True,  True):  0.95,   # positive PCR & high-risk
    (True,  False): 0.85,   # positive PCR, low-risk
    (False, True): 0.30,    # negative PCR, high-risk
    (False, False): 0.05,   # else
}

# ─────────────────────────────────────────────────────────────
# 2.  Evidence (observed variables)
# ─────────────────────────────────────────────────────────────
evidence = dict(
    V=True,   # it *is* flu season
    C=True,   # patient coughs
    F=True,   # feverish
    H=True,   # high-risk patient
    # PCR result not back yet: P is hidden
)

# ─────────────────────────────────────────────────────────────
# 3.  Joint-prob evaluator
# ─────────────────────────────────────────────────────────────
def joint(assign: Dict[str, bool]) -> float:
    p  = P_V[assign['V']] if assign['V'] else 1-P_V[True]
    p *= P_C[assign['C']] if assign['C'] else 1-P_C[True]
    p *= P_F[assign['F']] if assign['F'] else 1-P_F[True]
    p *= P_H[assign['H']] if assign['H'] else 1-P_H[True]

    pI = P_I[(assign['V'], assign['C'], assign['F'])]
    p *= pI if assign['I'] else 1-pI

    pP = P_P[assign['I']]
    p *= pP if assign['P'] else 1-pP

    pA = P_A[(assign['P'], assign['H'])]
    p *= pA if assign['A'] else 1-pA
    return p

# Variable order (acyclic)
order = ['V','C','F','H','I','P','A']

# ─────────────────────────────────────────────────────────────
# 4.  Backward elimination proof
# ─────────────────────────────────────────────────────────────
def eliminate(target: str, evidence: Dict[str, bool]) -> float:
    hidden = [v for v in order if v not in evidence and v != target]
    sum_true = sum_false = 0.0

    print("\nBackward-elimination steps:")
    for world in product([False, True], repeat=len(hidden)):
        assign = dict(zip(hidden, world), **evidence)

        assign[target] = False
        pF = joint(assign)
        sum_false += pF

        assign[target] = True
        pT = joint(assign)
        sum_true += pT

        print(f"  hidden={dict(zip(hidden, world))}  "
              f"add  A=False:{pF:.6f}  A=True:{pT:.6f}")

    norm = sum_true + sum_false
    print(f"\nNormalisation: True={sum_true:.6f}  False={sum_false:.6f}  "
          f"Total={norm:.6f}")
    return sum_true / norm

# ─────────────────────────────────────────────────────────────
# 5.  Run proof
# ─────────────────────────────────────────────────────────────
posterior = eliminate('A', evidence)

print(f"\nPosterior  P(StartAntivirals=True | evidence) = {posterior:.4f}")
THRESH = 0.70
if posterior > THRESH:
    print(f"Verdict:  Suggest *start antivirals* (posterior > {THRESH}).")
else:
    print("Verdict:  No antiviral therapy indicated yet.")

