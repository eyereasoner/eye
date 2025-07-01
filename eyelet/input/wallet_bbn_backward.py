#!/usr/bin/env python3
"""
wallet_bbn_backward.py
────────────────────────────────────────────────────────────────
Backward-chaining Bayesian proof for an “agentic wallet”.

Variables (Boolean, True = present)
-----------------------------------
D  DeviceSecure      – signer phone / HSM is secure
N  NetworkSecure     – RPC / Wi-Fi link is secure
U  UserKeyComp       – user’s private key compromised
A  AgentBug          – wallet’s autonomous agent contains a bug
R  RiskHigh          – overall transaction risk is high
M  RequireMultisig   – wallet switches to multisig mode   (goal)

Structure
---------
D ─┐                ┌────────► Agent bug lifts risk ────┐
   │                │                                   ▼
   ▼                ▼                                   R ─▶ M
   U  (key comp)    A  (agent bug)  ────────────────────┘
   ▲
   │
N ─┘   (insecure network raises key-comp risk)

CPTs (illustrative numbers)
---------------------------
• If either *device* or *network* is insecure, key-comp risk rises.
• Agent-bug prior 5 %.
• RiskHigh if (U or A) according to noisy-OR.
• Multisig is required if RiskHigh with 90 %, otherwise 5 %.

Evidence
--------
DeviceSecure   = False    (phone OS is outdated)
NetworkSecure  = False    (unencrypted Wi-Fi)
AgentBug       = *unknown* (no latest audit report)

Goal
----
Prove that P(RequireMultisig = True | evidence) > 0.7

All elimination steps are printed as Bayesian “proof evidence”.
"""

from itertools import product
from typing import Dict

# ─────────────────────────────────────────────────────────────
# 1.  CPTs (all values True-probabilities; False = 1−True)
# ─────────────────────────────────────────────────────────────

# Priors for device / network security
P_D = {True: 0.80,  False: 0.20}           # DeviceSecure
P_N = {True: 0.85,  False: 0.15}           # NetworkSecure

# Agent code bug (independent prior)
P_A = {True: 0.05,  False: 0.95}

# Key-compromise  P(U=True | D,N)
P_U = {
    (True,  True ): 0.01,
    (True,  False): 0.10,
    (False, True ): 0.20,
    (False, False): 0.90,   # ← very risky if both insecure
}

# RiskHigh  P(R=True | U,A)  (noisy-OR style)
P_R = {
    (False, False): 0.02,
    (True,  False): 0.95,   # ← higher than before
    (False, True ): 0.90,   # ← higher than before
    (True,  True ): 0.98,
}

# Multisig decision  P(M=True | RiskHigh)
P_M = {
    True : 0.90,
    False: 0.0001,          # ← wallet almost never uses single-sig if risk low
}

# ─────────────────────────────────────────────────────────────
# 2.  Evidence  (observed)
# ─────────────────────────────────────────────────────────────
evidence = dict(
    D=False,   # insecure device (outdated OS)
    N=False,   # insecure network (open Wi-Fi)
)

# ─────────────────────────────────────────────────────────────
# 3.  Joint probability of a complete assignment
# ─────────────────────────────────────────────────────────────
def joint(a: Dict[str,bool]) -> float:
    """P(assignment)  where assignment has all variables."""
    p  = P_D[a['D']]
    p *= P_N[a['N']]
    p *= P_U[(a['D'], a['N'])]     if a['U'] else 1-P_U[(a['D'], a['N'])]
    p *= P_A[a['A']]              if a['A'] else 1-P_A[True]
    p *= P_R[(a['U'], a['A'])]     if a['R'] else 1-P_R[(a['U'], a['A'])]
    p *= P_M[a['R']]              if a['M'] else 1-P_M[a['R']]
    return p

order = ['D','N','U','A','R','M']          # DAG topological order

# ─────────────────────────────────────────────────────────────
# 4.  Variable-elimination proof  P(M=True | evidence)
# ─────────────────────────────────────────────────────────────
def eliminate(target:str, evidence:Dict[str,bool]) -> float:
    hidden = [v for v in order if v not in evidence and v != target]
    t_sum = f_sum = 0.0

    print("\nBackward-elimination rows:")
    for vals in product([False,True], repeat=len(hidden)):
        world = dict(zip(hidden, vals), **evidence)

        world[target] = False
        pf = joint(world); f_sum += pf

        world[target] = True
        pt = joint(world); t_sum += pt

        print(f"  {dict(zip(hidden,vals))}  →  add  F:{pf:.6f}  T:{pt:.6f}")

    total = t_sum + f_sum
    print(f"\nNormalisation  True={t_sum:.6f}  False={f_sum:.6f}  Total={total:.6f}")
    return t_sum / total

# ─────────────────────────────────────────────────────────────
# 5.  Run the proof
# ─────────────────────────────────────────────────────────────
posterior = eliminate('M', evidence)

print(f"\nPosterior  P(Multisig=True | evidence) = {posterior:.4f}")
THRESH = 0.70
print("Verdict:", "Activate MULTISIG." if posterior > THRESH
                  else "Single-sig acceptable.")

