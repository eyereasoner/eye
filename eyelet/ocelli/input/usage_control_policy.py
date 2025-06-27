"""
usage_control_policy.py
─────────────────────────────

Decision:
    • require_obligation  (delete-after-use obligation & audit)

Random behaviour (conditioned on the decision):
    • compliant = True  with P=0.90 if obligation, else 0.40
      – success  (+25 €)   if compliant
      – misuse   (−80 €)  if not compliant

Direct decision cost:
    • enforcing obligation costs −5 €.

We evaluate both choices, pick the one with the higher expected utility,
and print a human-readable report.
"""

from typing import NamedTuple, Dict


class Scenario(NamedTuple):
    p_compliant_if_obligation: float = 0.90
    p_compliant_no_obligation: float = 0.40
    u_success: float = 25
    u_misuse: float = -80
    u_obligation_cost: float = -5


def expected_utility(attach_obligation: bool, s: Scenario) -> float:
    """Return the expected utility for one choice."""
    # compliance probability depends on the choice
    p_compliant = (
        s.p_compliant_if_obligation if attach_obligation
        else s.p_compliant_no_obligation
    )

    # utilities of the stochastic outcomes
    eu_outcome = (
        p_compliant *  s.u_success +
        (1 - p_compliant) * s.u_misuse
    )

    # enforcement cost applies only if we attach the obligation
    decision_cost = s.u_obligation_cost if attach_obligation else 0

    return eu_outcome + decision_cost


def optimise_policy(s: Scenario) -> Dict[str, bool | float]:
    """Evaluate both choices and return the best one plus its EU."""
    eu_with_obl   = expected_utility(True,  s)
    eu_without_obl = expected_utility(False, s)

    attach_obligation = eu_with_obl > eu_without_obl
    max_eu = eu_with_obl if attach_obligation else eu_without_obl

    return {
        "attach_obligation": attach_obligation,
        "max_eu": max_eu,
        "eu_with_obl": eu_with_obl,
        "eu_without_obl": eu_without_obl,
    }


def main() -> None:
    s = Scenario()                        # default parameters
    result = optimise_policy(s)

    # ─────────────  Human-readable report  ─────────────
    print("Usage-control policy selected (plain Python)")
    print("-------------------------------------------")
    print(f"Attach deletion obligation? : {result['attach_obligation']}")
    print(f"Expected utility            : {result['max_eu']:.2f} €\n")

    print("Detailed evaluation:")
    print(f"  EU if obligation attached : {result['eu_with_obl']:.2f} €")
    print(f"  EU if no obligation       : {result['eu_without_obl']:.2f} €")


if __name__ == "__main__":
    main()

