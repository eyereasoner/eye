#!/usr/bin/env python3
"""
clinic.py
─────────────────────────────────────────────────────────────
Goal-directed (“backward”) variable-elimination proof in a
small clinical Bayesian network.

Description
-----------
Variables (all Boolean, True = present/positive)
  V   ViralSeason        (influenza season)
  C   Cough              (patient coughs)
  F   Fever              (temperature > 38 °C)
  I   InfluenzaInfx      (true viral infection)
  P   PCR_Positive       (PCR nasal swab positive)
  H   HighRisk           (age >65 or chronic cond.)
  A   StartAntivirals    (decision variable, goal)

Structure
  ViralSeason ─┐
               ▼
               I ─▶ P ─▶ A
  Cough ───────┤     ▲
  Fever ───────┘     │
                     │
  HighRisk ──────────┘

CPTs are illustrative (toy numbers), not clinical guidance.
Evidence in this example: V=True, C=True, F=True, H=True, P=unknown.
Goal: compute P(A=True | evidence) and show a backward-elimination proof.
"""

from itertools import product
from typing import Dict, Tuple, Iterable, List

# ─────────────────────────────────────────────────────────────
# 1) Conditional Probability Tables (toy numbers)
# ─────────────────────────────────────────────────────────────
# Priors
P_V = {True: 0.4,  False: 0.6}   # influenza season
P_C = {True: 0.3,  False: 0.7}   # baseline cough
P_F = {True: 0.2,  False: 0.8}   # baseline fever
P_H = {True: 0.25, False: 0.75}  # high-risk pop

# Infection | (V,C,F)
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

# PCR | I
P_P = {True: 0.95, False: 0.05}   # sensitivity / (1-specificity)

# StartAntivirals | (P,H)
P_A = {
    (True,  True):  0.95,
    (True,  False): 0.85,
    (False, True):  0.30,
    (False, False): 0.05,
}

# Evidence (observed variables)
EVIDENCE = dict(V=True, C=True, F=True, H=True)  # P is latent

# Variable order (acyclic, parents before children)
ORDER = ['V', 'C', 'F', 'H', 'I', 'P', 'A']

# ─────────────────────────────────────────────────────────────
# 2) Joint probability from CPTs
# ─────────────────────────────────────────────────────────────
def joint(assign: Dict[str, bool]) -> float:
    """
    Return P(assign) by chaining conditionals along ORDER.
    Each variable multiplies either its True-row or (1 - True-row).
    """
    p  = P_V[True] if assign['V'] else (1 - P_V[True])
    p *= P_C[True] if assign['C'] else (1 - P_C[True])
    p *= P_F[True] if assign['F'] else (1 - P_F[True])
    p *= P_H[True] if assign['H'] else (1 - P_H[True])

    pI = P_I[(assign['V'], assign['C'], assign['F'])]
    p *= pI if assign['I'] else (1 - pI)

    pP = P_P[assign['I']]
    p *= pP if assign['P'] else (1 - pP)

    pA = P_A[(assign['P'], assign['H'])]
    p *= pA if assign['A'] else (1 - pA)
    return p

def worlds(ev: Dict[str, bool], include: Iterable[str]) -> Iterable[Dict[str, bool]]:
    """Yield assignments for the variables in `include`, keeping `ev` fixed."""
    inc = list(include)
    for values in product([False, True], repeat=len(inc)):
        yield dict(zip(inc, values), **ev)

# ─────────────────────────────────────────────────────────────
# 3) Backward (“eliminate hidden vars”) posterior
# ─────────────────────────────────────────────────────────────
def eliminate_posterior(target: str, evidence: Dict[str, bool]) -> float:
    """Silent elimination: sum over hidden assignments, return P(target=True|evidence)."""
    hidden = [v for v in ORDER if v not in evidence and v != target]
    sum_true = sum_false = 0.0
    for world in product([False, True], repeat=len(hidden)):
        assign = dict(zip(hidden, world), **evidence)
        assign[target] = False
        sum_false += joint(assign)
        assign[target] = True
        sum_true  += joint(assign)
    norm = sum_true + sum_false
    return 0.0 if norm == 0.0 else (sum_true / norm)

def eliminate_trace(target: str, evidence: Dict[str, bool]) -> float:
    """Verbose elimination: prints each row and the normalisation step."""
    hidden = [v for v in ORDER if v not in evidence and v != target]
    sum_true = sum_false = 0.0
    print("Backward-elimination steps:")
    for world in product([False, True], repeat=len(hidden)):
        assign = dict(zip(hidden, world), **evidence)
        assign[target] = False
        pF = joint(assign)
        sum_false += pF
        assign[target] = True
        pT = joint(assign)
        sum_true += pT
        print(f"  hidden={{{', '.join(f'{k}={v}' for k,v in dict(zip(hidden, world)).items())}}}  "
              f"A=False:{pF:.6f}  A=True:{pT:.6f}")
    norm = sum_true + sum_false
    print(f"\nNormalisation: True={sum_true:.6f}  False={sum_false:.6f}  Total={norm:.6f}")
    return 0.0 if norm == 0.0 else (sum_true / norm)

# Brute-force posterior (cross-check)
def brute_posterior(target: str, evidence: Dict[str, bool]) -> float:
    num = den = 0.0
    # enumerate all unobserved variables including target
    unknown = [v for v in ORDER if v not in evidence]
    for asg in worlds(evidence, unknown):
        p = joint(asg)
        den += p
        if asg[target]:
            num += p
    return 0.0 if den == 0.0 else (num / den)

# ─────────────────────────────────────────────────────────────
# 4) ARC sections
# ─────────────────────────────────────────────────────────────
def arc_answer(evidence: Dict[str, bool], posterior: float, thresh: float) -> None:
    print("Answer")
    print("------")
    print("Evidence:")
    for k in ORDER:
        if k in evidence:
            print(f"  {k} = {evidence[k]}")
    print("\nTarget:  P(A=True | evidence)")
    print(f"Posterior: {posterior:.4f}")
    print("Verdict:",
          f"Suggest *start antivirals* (posterior > {thresh})." if posterior > thresh
          else "No antiviral therapy indicated yet.")
    print()

def arc_reason(evidence: Dict[str, bool]) -> None:
    print("Reason why")
    print("----------")
    print("We eliminate hidden variables by exact summation over their values,")
    print("accumulating the joint probability with A=False and A=True for each row,")
    print("then normalise:  P(A=True|e) = sum_true / (sum_true + sum_false).\n")
    post = eliminate_trace('A', dict(evidence))
    print(f"\nResult: P(A=True | evidence) = {post:.4f}\n")

def arc_check() -> None:
    print("Check (harness)")
    print("---------------")
    eps = 1e-12

    # (1) Joint normalises over all 2^|ORDER| worlds
    total = 0.0
    for vals in product([False, True], repeat=len(ORDER)):
        total += joint(dict(zip(ORDER, vals)))
    assert abs(total - 1.0) < 1e-12, f"Joint does not normalise (sum={total})"

    # (2) Backward elimination equals brute-force posterior under example evidence
    gd = eliminate_posterior('A', EVIDENCE)
    bf = brute_posterior('A', EVIDENCE)
    assert abs(gd - bf) < 1e-12, f"Posterior mismatch: elimination {gd} vs brute {bf}"

    # (3) Local conditional sanity: A ⟂ (others) | (P,H)
    for p in (False, True):
        for h in (False, True):
            ev = {'P': p, 'H': h}
            pr = brute_posterior('A', ev)
            assert abs(pr - P_A[(p,h)]) < 1e-12, f"P(A|P={p},H={h})={pr} != table"

    # (4) PCR conditional sanity: P ⟂ (V,C,F,H) | I
    for i in (False, True):
        ev = {'I': i}
        pr = brute_posterior('P', ev)
        assert abs(pr - P_P[i]) < 1e-12, f"P(P|I={i})={pr} != table"

    # (5) Infection conditional sanity: I | (V,C,F) matches table rows
    for v in (False, True):
        for c in (False, True):
            for f in (False, True):
                ev = {'V': v, 'C': c, 'F': f}
                pr = brute_posterior('I', ev)
                assert abs(pr - P_I[(v,c,f)]) < 1e-12, f"P(I|V={v},C={c},F={f}) wrong"

    print("OK: joint normalisation, posterior equality, and CPT conditionals verified.\n")

# ─────────────────────────────────────────────────────────────
# 5) Main
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    THRESH = 0.70
    post = eliminate_posterior('A', EVIDENCE)   # silent compute for Answer

    arc_answer(EVIDENCE, post, THRESH)
    arc_reason(EVIDENCE)
    arc_check()

