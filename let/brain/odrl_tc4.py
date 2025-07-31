#!/usr/bin/env python3
"""
Testcase-4: A duty which is prohibited to be fulfilled
======================================================

Scenario (from ODRL Test Conflicts, testcase-4):
  - policy4a: Permission to odrl:use ex:resourceX, with a duty that Alice must ex:signContract
              (assigner ex:bob, target ex:contract).
  - policy4b: Prohibition for Alice to ex:signContract (assigner ex:bob, target ex:contract).

Expected global activation state: Conflict
  "The policies permit and prohibit the action for any possible state of the world."

Reading used:
  In ODRL, a permission with an attached duty is commonly understood as:
    To exercise the permission, the duty must (be allowed to) be fulfilled.
  We model the duty as an *obligation-to-sign* attached to the permission and check whether
  this obligation clashes with an explicit prohibition to sign.

This script:
  1) Encodes policy4a (permission-with-duty) and policy4b (prohibition).
  2) Derives an implied obligation from the duty in policy4a:
       Duty(signContract)  ⇒  Obligation(signContract)  (for the same subject/assigner/target)
  3) Detects unconditional clash with policy4b's Prohibition(signContract).
  4) Always prints:
       - The URI for the global result
       - A concise goal-oriented proof explanation

No external libraries required.
"""

from dataclasses import dataclass
from typing import Optional, List

# --- Policy modeling ----------------------------------------------------------

@dataclass(frozen=True)
class Duty:
    action: str          # e.g., "ex:signContract"
    assignee: str        # "ex:alice"
    assigner: str        # "ex:bob"
    target: str          # "ex:contract"

@dataclass(frozen=True)
class PermissionWithDuty:
    kind: str            # "permission"
    assignee: str        # "ex:alice"
    action: str          # "odrl:use"
    target: str          # "ex:resourceX"
    duty: Duty

@dataclass(frozen=True)
class Prohibition:
    kind: str            # "prohibition"
    assignee: str        # "ex:alice"
    action: str          # "ex:signContract"
    target: str          # "ex:contract"
    assigner: str        # "ex:bob"

@dataclass(frozen=True)
class Obligation:
    kind: str            # "obligation"
    assignee: str
    action: str
    target: str
    assigner: str

# Instantiate the two rules exactly as described in the testcase
POLICY_4A = PermissionWithDuty(
    kind="permission",
    assignee="ex:alice",
    action="odrl:use",
    target="ex:resourceX",
    duty=Duty(
        action="ex:signContract",
        assignee="ex:alice",
        assigner="ex:bob",
        target="ex:contract",
    )
)
POLICY_4B = Prohibition(
    kind="prohibition",
    assignee="ex:alice",
    action="ex:signContract",
    target="ex:contract",
    assigner="ex:bob",
)

# --- Conflict detection -------------------------------------------------------

REPORT_URIS = {
    "Conflict":    "https://w3id.org/force/compliance-report#Conflict",
    "Ambiguous":   "https://w3id.org/force/compliance-report#Ambiguous",
    "Prohibited":  "https://w3id.org/force/compliance-report#Prohibited",
    "Permitted":   "https://w3id.org/force/compliance-report#Permitted",
    "NoConflict":  "https://w3id.org/force/compliance-report#NoConflict",
}

def obligation_from_duty(p: PermissionWithDuty) -> Obligation:
    """
    Convert the duty attached to a permission into an obligation on the same
    subject/assigner/target for the duty's action, to capture that the duty
    must (be allowed to) be fulfilled in order to exercise the permission.
    """
    d = p.duty
    return Obligation(
        kind="obligation",
        assignee=d.assignee,
        action=d.action,
        target=d.target,
        assigner=d.assigner,
    )

def unconditional_conflict(obligation: Obligation, prohibition: Prohibition) -> bool:
    """
    Returns True iff the obligation and prohibition are on the *same*
    assignee, action, target, and assigner — hence they clash in every world.
    """
    return (
        obligation.assignee == prohibition.assignee and
        obligation.action   == prohibition.action   and
        obligation.target   == prohibition.target   and
        obligation.assigner == prohibition.assigner
    )

def classify_global(p: PermissionWithDuty, prohib: Prohibition) -> str:
    implied_ob = obligation_from_duty(p)
    if unconditional_conflict(implied_ob, prohib):
        return REPORT_URIS["Conflict"]
    return REPORT_URIS["NoConflict"]

# --- Proof construction -------------------------------------------------------

def build_goal_oriented_proof(p: PermissionWithDuty, prohib: Prohibition) -> str:
    """
    Goal-oriented proof that the global activation state is Conflict.

    Goal G:
      Show that for every possible state of the world w,
      O(signContract) and F(signContract) both hold for the same subject/assigner/target.

    Strategy:
      1) From policy4a's duty, derive an obligation to sign (to exercise the permission).
      2) From policy4b, there is a prohibition to sign.
      3) The obligation and prohibition are about the *same* action/party/object ⇒ clash in all worlds.
    """
    ob = obligation_from_duty(p)

    lines: List[str] = []
    lines.append("--- Goal-Oriented Proof Explanation ---\n")
    lines.append("Policies:")
    lines.append(f"  (P1) Permission with duty: {p.assignee} may {p.action} {p.target}")
    lines.append(f"       provided she fulfills duty D: {p.duty.action} (assigner {p.duty.assigner}, target {p.duty.target}).")
    lines.append(f"  (F1) Prohibition: {prohib.assignee} must-not {prohib.action} {prohib.target} (assigner {prohib.assigner}).")
    lines.append("")
    lines.append("Deontic reading of duties:")
    lines.append("  (D) A duty attached to a permission induces an obligation to perform the duty action")
    lines.append("      for the same subject/assigner/target when exercising the permission.")
    lines.append("")
    lines.append("Derivation:")
    lines.append(f"  (S1) From (P1) and (D): Obligation O({ob.action} on {ob.target}, assigner {ob.assigner}) holds for {ob.assignee}.")
    lines.append(f"  (S2) From (F1): Prohibition F({prohib.action} on {prohib.target}, assigner {prohib.assigner}) holds for {prohib.assignee}.")
    lines.append("  (S3) O and F refer to the *same* action/assignee/assigner/target.")
    lines.append("  (C) Therefore, for any world w, O(signContract) ∧ F(signContract) holds ⇒ Global state: Conflict.")
    lines.append("")
    lines.append("Intuition:")
    lines.append("  The permission can only be exercised by fulfilling the duty (signing),")
    lines.append("  yet signing is prohibited. The system simultaneously requires and forbids the very same act.")
    return "\n".join(lines)

# --- Main ---------------------------------------------------------------------

def main() -> None:
    permission_with_duty = POLICY_4A
    prohibition          = POLICY_4B

    result_uri = classify_global(permission_with_duty, prohibition)

    # Always print the expected URI first (required by test harnesses)
    print(result_uri)

    # Then print a compact, goal-oriented proof explanation
    print()
    print(build_goal_oriented_proof(permission_with_duty, prohibition))

if __name__ == "__main__":
    main()

