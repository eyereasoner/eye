"""
epidemic_policy.py
==================

World model
===========                      (all probabilities are seasonal)

 Uncertainty
 ───────────
   outbreak                P = 0.10

 Infection probabilities  (conditional on outbreak)
 ──────────────────────────────────────────────────
   vaccinate  mask      P(infect | outbreak)
   ─────────  ────      ─────────────────────
      yes       yes            0.05
      yes       no             0.05
      no        yes            0.30
      no        no             0.60

 Decisions & utilities  (€/season)
 ─────────────────────────────────
   vaccinate           −2
   mask mandate        −1
   infection          −200
   healthy              0     (baseline)

Expected-utility calculation
============================
EU(decision) = cost_decisions + (−200)·P(infect)

where P(infect) = 0.10 · P(infect | outbreak)            (if no outbreak,
                                                          infection=0)
"""

from dataclasses import dataclass, field
from itertools import product
from typing import Dict, Tuple


@dataclass(frozen=True)
class Scenario:
    p_outbreak: float = 0.10
    # infection probabilities given an outbreak
    p_inf_vacc_mask: Dict[Tuple[bool, bool], float] = field(
        default_factory=lambda: {
            (True,  True):  0.05,
            (True,  False): 0.05,
            (False, True):  0.30,
            (False, False): 0.60,
        }
    )
    # utilities (negative = cost)
    u_vaccinate: float = -2
    u_mask: float = -1
    u_infect: float = -200


def expected_utility(vaccinate: bool, mask: bool, s: Scenario) -> float:
    decision_cost = (s.u_vaccinate if vaccinate else 0) + \
                    (s.u_mask      if mask      else 0)
    p_infect = s.p_outbreak * s.p_inf_vacc_mask[(vaccinate, mask)]
    return decision_cost + p_infect * s.u_infect


def optimise_policy(s: Scenario):
    best_decision, best_eu = None, float("-inf")
    for vaccinate, mask in product([False, True], repeat=2):
        eu = expected_utility(vaccinate, mask, s)
        if eu > best_eu:
            best_decision, best_eu = {"vaccinate": vaccinate, "mask": mask}, eu
    return best_decision, best_eu


if __name__ == "__main__":
    decision, max_eu = optimise_policy(Scenario())
    print("Optimal decisions (MEU):")
    for k, v in decision.items():
        print(f"  • {k}: {v}")
    print(f"\nMaximum expected utility: {max_eu:.2f}")

