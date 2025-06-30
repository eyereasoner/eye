"""
access_control_policy.py
────────────────────────

Decision options
----------------
grant_direct = True   →  allow Alice straight in
grant_direct = False  →  demand MFA first

Uncertainties
-------------
user_is_attacker              P = 0.15
MFA success  |  legitimate    P = 0.95
             |  attacker      P = 0.20

Utilities (€)
-------------
+30   legit_session      (legitimate user gets access)
-100  breach             (attacker gets access)
-10   lockout            (legitimate user fails MFA)
-5    MFA friction       (charged whenever MFA is required)
"""

from dataclasses import dataclass
from typing import Dict


@dataclass(frozen=True)
class Scenario:
    p_attacker: float = 0.15
    p_mfa_pass_legit: float = 0.95
    p_mfa_pass_attacker: float = 0.20
    u_legit_session: float = 30
    u_breach: float = -100
    u_lockout: float = -10
    u_mfa_friction: float = -5


def expected_utility(grant_direct: bool, s: Scenario) -> float:
    """Compute EU for a single decision."""
    p_legit = 1 - s.p_attacker
    eu = 0.0

    if grant_direct:
        # No MFA, immediate decision
        eu += s.p_attacker * s.u_breach
        eu += p_legit * s.u_legit_session
    else:
        # MFA branch ------------------------------------------
        #   ─ attacker
        eu += s.p_attacker * s.p_mfa_pass_attacker * s.u_breach
        # attacker + MFA-fail gives no additional utility (blocked)

        #   ─ legitimate user
        eu += p_legit * s.p_mfa_pass_legit * s.u_legit_session
        eu += p_legit * (1 - s.p_mfa_pass_legit) * s.u_lockout

        # friction cost applies once per MFA request
        eu += s.u_mfa_friction

    return eu


def optimise_policy(s: Scenario) -> Dict[str, float | bool]:
    """Return optimal decision and its expected utility."""
    eu_grant  = expected_utility(True, s)
    eu_mfa    = expected_utility(False, s)

    grant_direct = eu_grant > eu_mfa
    best_eu = eu_grant if grant_direct else eu_mfa

    return {
        "grant_direct": grant_direct,
        "require_mfa": not grant_direct,
        "best_eu": best_eu,
        "eu_grant_direct": eu_grant,
        "eu_require_mfa": eu_mfa,
    }


def main() -> None:
    s = Scenario()
    res = optimise_policy(s)

    # ───────────── human-readable report ─────────────
    print("Access-control policy (plain Python)")
    print("------------------------------------")
    print(f"Grant immediately? : {res['grant_direct']}")
    print(f"Require MFA?       : {res['require_mfa']}")
    print(f"Expected utility   : {res['best_eu']:.2f} €\n")

    print("Detailed evaluation:")
    print(f"  EU if grant immediately : {res['eu_grant_direct']:.2f} €")
    print(f"  EU if require MFA       : {res['eu_require_mfa']:.2f} €")


if __name__ == "__main__":
    main()

