#!/usr/bin/env python3
"""
wallet.py
Purely logical backward-proof version of the “agentic wallet”.

Variables (Booleans)
--------------------
D  DeviceSecure
N  NetworkSecure
U  UserKeyComp
A  AgentBug
R  RiskHigh
M  RequireMultisig   ← goal

Deterministic rules (logical, no CPTs)
--------------------------------------
1) Key compromise risk:
     U ← (¬D) ∨ (¬N)
   (If either device or network is insecure, key-comp risk is present.)

2) Risk:
     R ← U ∨ A
   (Either key compromise or an agent bug is sufficient to make risk high.)

3) Multisig policy:
     M ← R
   (Multisig is required whenever risk is high.)

Evidence
--------
D = False   (device insecure)
N = False   (network insecure)
A is unknown (two exclusive worlds remain: A=False or A=True)

Queries
-------
M, R, U, A, D, N

For each query we print a proof trace over the surviving worlds and classify the
result as VALID / SATISFIABLE / UNSATISFIABLE. With the evidence above, M is VALID.
"""

from typing import Dict, List, Tuple

# ─────────────────────────────────────────────────────────────
# 1) Evidence (observed facts)
# ─────────────────────────────────────────────────────────────
EVIDENCE: Dict[str, bool] = {
    "D": False,   # DeviceSecure
    "N": False,   # NetworkSecure
    # "A" is intentionally left unknown
}

# ─────────────────────────────────────────────────────────────
# 2) Deterministic propagation (rules)
# ─────────────────────────────────────────────────────────────
def derive_state(partial: Dict[str, bool]) -> Dict[str, bool]:
    """
    Given a partial assignment for D, N, A (A may be provided),
    derive U, R, M deterministically and return the full state.
    """
    D = partial["D"]
    N = partial["N"]
    A = partial["A"]

    # Rules
    U = (not D) or (not N)   # U ← (¬D) ∨ (¬N)
    R = U or A               # R ← U ∨ A
    M = R                    # M ← R

    return {"D": D, "N": N, "A": A, "U": U, "R": R, "M": M}

# ─────────────────────────────────────────────────────────────
# 3) Enumerate surviving worlds under evidence (no weights)
# ─────────────────────────────────────────────────────────────
def enumerate_worlds(evidence: Dict[str, bool]) -> List[Dict[str, bool]]:
    """
    Evidence fixes D and N. A is unknown → two exclusive worlds: A=False/True.
    U, R, M are derived deterministically by the rules.
    """
    worlds: List[Dict[str, bool]] = []
    for A in (False, True):
        base = dict(evidence)
        base["A"] = A
        worlds.append(derive_state(base))
    return worlds

WORLDS = enumerate_worlds(EVIDENCE)

# ─────────────────────────────────────────────────────────────
# 4) Proof routine and classification
# ─────────────────────────────────────────────────────────────
def prove(var: str) -> List[int]:
    """
    Print a proof trace for boolean variable `var` over the surviving worlds,
    and return the list of world indices (1-based) where it holds.
    """
    assert var in ("D","N","U","A","R","M"), f"Unknown variable: {var}"
    print(f"\n=== Proving {var} ===")

    holds_in: List[int] = []
    for i, w in enumerate(WORLDS, 1):
        truth = w[var]

        # Reasons for transparency
        reason = ""
        if var == "U":
            reason = f" (U ← ¬D ∨ ¬N; D={w['D']}, N={w['N']} ⇒ U={w['U']})"
        elif var == "R":
            reason = f" (R ← U ∨ A; U={w['U']}, A={w['A']} ⇒ R={w['R']})"
        elif var == "M":
            reason = f" (M ← R; R={w['R']} ⇒ M={w['M']})"
        elif var in ("D","N","A"):
            reason = " (given/evidence or open choice)"

        print(f"World {i}: D={w['D']} N={w['N']} A={w['A']} U={w['U']} R={w['R']} M={w['M']}   "
              f"{'✓' if truth else '✗'} {var}{reason}")

        if truth:
            holds_in.append(i)

    n = len(WORLDS)
    k = len(holds_in)
    if k == n:
        print(f"Result: VALID — {var} holds in all {n} surviving worlds.")
    elif k == 0:
        print(f"Result: UNSATISFIABLE — {var} is false in all {n} surviving worlds.")
    else:
        print(f"Result: SATISFIABLE (but not valid) — holds in {k}/{n} surviving worlds.")

    return holds_in

# ─────────────────────────────────────────────────────────────
# 5) Run the queries & summarise (logical status only)
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    queries = ["M", "R", "U", "A", "D", "N"]
    results = {q: prove(q) for q in queries}

    print("\n=== Summary ===")
    n = len(WORLDS)
    for q in queries:
        k = len(results[q])
        if k == n:
            status = "VALID"
        elif k == 0:
            status = "UNSATISFIABLE"
        else:
            status = f"SATISFIABLE in {k}/{n} worlds"
        extra = "" if k in (0, n) else " → " + ", ".join(str(i) for i in results[q])
        print(f"{q}: {status}{extra}")

    # Policy verdict (logical analogue of the original numeric goal)
    print("\nVerdict (logical): Since M is VALID under the evidence, "
          "the wallet must require MULTISIG.")

