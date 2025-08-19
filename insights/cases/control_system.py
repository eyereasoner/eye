#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Control Systems — Python translation of:
  https://raw.githubusercontent.com/eyereasoner/arvol/refs/heads/main/input/control-system.pl

ARC-style output:
- Answer     : all control1/2 solutions from the facts (actuator1 and actuator2), with numeric values
- Reason why : step-by-step numeric derivations, keeping the same comment labels as in the Prolog
- Check      : a harness that recomputes the terms, verifies equivalences
               (e.g., log(D1)/log(10) ≡ log10(D1)), and cross-checks measurement10/2 cases
"""

from dataclasses import dataclass
from typing import Dict, List, Tuple
import math

# -----------------------------------------------------------------------------
# Facts (as in the Prolog file)
# -----------------------------------------------------------------------------

measurement1: Dict[str, List[int]] = {
    "input1": [6, 11],
    "disturbance2": [45, 39],
}
measurement2: Dict[str, bool] = {
    "input2": True,
}
measurement3: Dict[str, int] = {
    "input3": 56967,
    "disturbance1": 35766,
}
measurement4: Dict[str, int] = {
    "output2": 24,
}

observation1: Dict[str, int] = {"state1": 80}
observation2: Dict[str, bool] = {"state2": False}
observation3: Dict[str, int] = {"state3": 22}

target2: Dict[str, int] = {"output2": 29}


# -----------------------------------------------------------------------------
# Derived measurement — PND feedback control (measurement10/2)
# -----------------------------------------------------------------------------

def measurement10(I: str) -> float:
    """
    PND feedback control:
      - if measurement1(I) = [M1, M2] with M1 < M2, then M = sqrt(M2 - M1)
      - else M = M1
    """
    M1, M2 = measurement1[I]
    if M1 < M2:
        M3 = M2 - M1
        return math.sqrt(M3)
    else:
        return float(M1)


# -----------------------------------------------------------------------------
# control1/2 rules
# -----------------------------------------------------------------------------

@dataclass
class ControlSolution:
    actuator: str
    C: float
    steps: List[str]  # human-readable derivation lines

def control1_actuator1() -> ControlSolution:
    """
    control1(actuator1, C) :-
        measurement10(input1, M1),
        measurement2(input2, true),
        measurement3(disturbance1, D1),
        C1 is M1*19.6,           % proportional part
        C2 is log(D1)/log(10),   % compensation part
        C  is C1 - C2.           % simple feedforward control
    """
    steps: List[str] = []
    M1 = measurement10("input1")
    assert measurement2["input2"] is True
    D1 = float(measurement3["disturbance1"])
    C1 = M1 * 19.6
    C2 = math.log(D1) / math.log(10.0)  # same as log10(D1)
    C = C1 - C2

    # Keep comment labels in the derivation:
    steps.append(f"M1 = measurement10(input1) = sqrt(11-6) = {M1:.12f}  (PND feedback control)")
    steps.append(f"C1 = M1 * 19.6            = {C1:.12f}  % proportional part")
    steps.append(f"C2 = log(D1)/log(10)      = log(35766)/log(10) = {C2:.12f}  % compensation part")
    steps.append(f"C  = C1 - C2               = {C:.12f}  % simple feedforward control")

    return ControlSolution("actuator1", C, steps)

def control1_actuator2() -> ControlSolution:
    """
    control1(actuator2, C) :-
        observation3(state3, P3),
        measurement4(output2, M4),
        target2(output2, T2),
        E  is T2 - M4,           % error
        D  is P3 - M4,           % differential error
        C1 is 5.8*E,             % proportional part
        N  is 7.3/E,             % nonlinear factor
        C2 is N*D,               % nonlinear differential part
        C  is C1 + C2.
    """
    steps: List[str] = []
    P3 = float(observation3["state3"])
    M4 = float(measurement4["output2"])
    T2 = float(target2["output2"])
    E = T2 - M4
    D = P3 - M4
    C1 = 5.8 * E
    N = 7.3 / E
    C2 = N * D
    C = C1 + C2

    # Keep comment labels in the derivation:
    steps.append(f"E  = T2 - M4  = {T2:.0f} - {M4:.0f} = {E:.12f}  % error")
    steps.append(f"D  = P3 - M4  = {P3:.0f} - {M4:.0f} = {D:.12f}  % differential error")
    steps.append(f"C1 = 5.8 * E  = 5.8*{E:.0f} = {C1:.12f}  % proportional part")
    steps.append(f"N  = 7.3 / E  = 7.3/{E:.0f} = {N:.12f}  % nonlinear factor")
    steps.append(f"C2 = N * D    = {N:.12f}*{D:.0f} = {C2:.12f}  % nonlinear differential part")
    steps.append(f"C  = C1 + C2  = {C1:.12f} + {C2:.12f} = {C:.12f}")

    return ControlSolution("actuator2", C, steps)

def control1_all() -> List[ControlSolution]:
    """Enumerate all control1(_, C) solutions implied by the facts."""
    return [control1_actuator1(), control1_actuator2()]


# -----------------------------------------------------------------------------
# ARC-style presentation
# -----------------------------------------------------------------------------

def print_answer(solutions: List[ControlSolution]) -> None:
    print("Answer")
    print("------")
    for sol in solutions:
        print(f"control1({sol.actuator}, C)  →  C = {sol.C:.12f}")
    print()

def print_reason(solutions: List[ControlSolution]) -> None:
    print("Reason why")
    print("----------")
    for sol in solutions:
        print(f"[{sol.actuator}]")
        for line in sol.steps:
            print("  " + line)
        print()

# -----------------------------------------------------------------------------
# Check (harness)
# -----------------------------------------------------------------------------

def check_harness(solutions: List[ControlSolution]) -> None:
    """
    Re-verify key equalities and numeric identities:
      - measurement10('input1') == sqrt(11-6)
      - measurement10('disturbance2') == 45 (since 45 >= 39)
      - log(D1)/log(10) == log10(D1)
      - The C values recompute to the same numbers (within a tiny epsilon)
    """
    eps = 1e-12

    # PND feedback control cases
    assert abs(measurement10("input1") - math.sqrt(11 - 6)) < eps
    assert abs(measurement10("disturbance2") - 45.0) < eps

    # Compensation log identity
    D1 = float(measurement3["disturbance1"])
    assert abs(math.log(D1) / math.log(10.0) - math.log10(D1)) < eps

    # Recompute and compare each solution
    recomputed: Dict[str, float] = {}
    recomputed["actuator1"] = (measurement10("input1") * 19.6) - (math.log(D1) / math.log(10.0))
    E = float(target2["output2"] - measurement4["output2"])      # 29 - 24 = 5
    D = float(observation3["state3"] - measurement4["output2"])  # 22 - 24 = -2
    recomputed["actuator2"] = (5.8 * E) + ((7.3 / E) * D)

    for sol in solutions:
        assert abs(sol.C - recomputed[sol.actuator]) < eps, f"Mismatch for {sol.actuator}"

# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------

def main():
    sols = control1_all()

    # ----- ARC output -----
    print_answer(sols)
    print_reason(sols)

    print("Check (harness)")
    print("---------------")
    try:
        check_harness(sols)
        print("OK: derivations match the rules; identities and PND logic verified.")
    except AssertionError as e:
        print("FAILED:", e)
        raise

if __name__ == "__main__":
    main()

