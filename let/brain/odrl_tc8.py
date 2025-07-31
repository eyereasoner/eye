#!/usr/bin/env python3
"""
Testcase-8: Permission X, Permission Y and Prohibition (X & Y)
==============================================================

Scenario (from ODRL Test Conflicts, testcase-8):
  - policy8a: Permission to ex:rent ex:collectionX.
  - policy8b: Permission to ex:sell ex:collectionX.
  - policy8c: Prohibition to ex:rentsell ex:collectionX, where
              ex:rentsell ≡ (ex:rent ⊓ ex:sell)  (OWL intersectionOf).

Expected global activation state: Conflict
  "The policies permit and prohibit the action for any possible state of the world."

Reading used (intersection-of actions):
  If an action A∧B is defined as the intersection of actions A and B, then
  having Permission(A) and Permission(B) (unconditionally, same subject/target)
  yields Permission(A∧B). If there is also a Prohibition(A∧B), we obtain a clash
  in *every* world.

This script:
  1) Encodes the three rules and an action algebra with intersection actions.
  2) Derives Permission(ex:rentsell) from the two base permissions.
  3) Detects the unconditional clash with Prohibition(ex:rentsell).
  4) Always prints:
       - The URI for the global result
       - A concise goal-oriented proof explanation

No external libraries required.
"""

from dataclasses import dataclass
from typing import Dict, FrozenSet, List, Optional, Set, Tuple

# --- Action algebra with "intersectionOf" ------------------------------------

class ActionAlgebra:
    """
    Minimal support for actions defined as intersections of other actions.
    Example: define_intersection("ex:rentsell", {"ex:rent", "ex:sell"})
    """
    def __init__(self) -> None:
        # composite -> frozenset({component actions})
        self.intersections: Dict[str, FrozenSet[str]] = {}

    def define_intersection(self, composite: str, components: Set[str]) -> None:
        if not components or len(components) < 2:
            raise ValueError("Intersection must have at least two components.")
        self.intersections[composite] = frozenset(components)

    def components_of(self, composite: str) -> Optional[FrozenSet[str]]:
        return self.intersections.get(composite)

    def can_derive_permission_on_composite(self, composite: str, permitted_actions: Set[str]) -> bool:
        """
        Return True iff all component actions of 'composite' are in the permitted set.
        """
        comps = self.components_of(composite)
        if comps is None:
            return False
        return comps.issubset(permitted_actions)

# Build the action algebra for testcase-8: rentsell ≡ rent ⊓ sell
ACTIONS = ActionAlgebra()
ACTIONS.define_intersection("ex:rentsell", {"ex:rent", "ex:sell"})

# --- Policy modeling ----------------------------------------------------------

@dataclass(frozen=True)
class Rule:
    kind: str          # "permission" or "prohibition"
    assignee: str      # "ex:alice"
    action: str        # e.g., "ex:rent", "ex:sell", or "ex:rentsell"
    target: str        # e.g., "ex:collectionX"
    refinement: Optional[str] = None  # None => unconditional

# Instantiate the three rules exactly as described in the testcase
POLICY_8A = Rule(kind="permission",  assignee="ex:alice", action="ex:rent",     target="ex:collectionX")
POLICY_8B = Rule(kind="permission",  assignee="ex:alice", action="ex:sell",     target="ex:collectionX")
POLICY_8C = Rule(kind="prohibition", assignee="ex:alice", action="ex:rentsell", target="ex:collectionX")

# --- Report URIs --------------------------------------------------------------

REPORT_URIS = {
    "Conflict":    "https://w3id.org/force/compliance-report#Conflict",
    "Ambiguous":   "https://w3id.org/force/compliance-report#Ambiguous",
    "Prohibited":  "https://w3id.org/force/compliance-report#Prohibited",
    "Permitted":   "https://w3id.org/force/compliance-report#Permitted",
    "NoConflict":  "https://w3id.org/force/compliance-report#NoConflict",
}

# --- Conflict detection -------------------------------------------------------

def same_subject_target(a: Rule, b: Rule) -> bool:
    return a.assignee == b.assignee and a.target == b.target

def conflict_on_intersection(perm_x: Rule, perm_y: Rule, prohib_xy: Rule) -> bool:
    """
    Returns True iff:
      - perm_x and perm_y are unconditional permissions on actions X and Y,
      - prohib_xy is an unconditional prohibition on composite action X∧Y,
      - same assignee and target for all three rules,
      - and the composite is defined as the intersection of X and Y.
      Then Permission(X∧Y) (by derivation) and Prohibition(X∧Y) hold in every world ⇒ Conflict.
    """
    # Basic shape checks
    if not (perm_x.kind == perm_y.kind == "permission" and prohib_xy.kind == "prohibition"):
        return False
    if not (same_subject_target(perm_x, perm_y) and same_subject_target(perm_x, prohib_xy)):
        return False
    if not (perm_x.refinement is None and perm_y.refinement is None and prohib_xy.refinement is None):
        return False

    # Check the intersection definition matches the two base actions
    comps = ACTIONS.components_of(prohib_xy.action)
    if comps is None:
        return False
    if comps != frozenset({perm_x.action, perm_y.action}):
        return False

    # Since both base actions are unconditionally permitted for the same subject/target,
    # we derive permission on the composite.
    permitted_actions = {perm_x.action, perm_y.action}
    derived_perm_on_composite = ACTIONS.can_derive_permission_on_composite(prohib_xy.action, permitted_actions)

    # The prohibition is explicit and unconditional.
    return derived_perm_on_composite

def classify_global(p1: Rule, p2: Rule, f: Rule) -> str:
    if conflict_on_intersection(p1, p2, f):
        return REPORT_URIS["Conflict"]
    # Not expected for this testcase, but kept for completeness.
    return REPORT_URIS["NoConflict"]

# --- Goal-oriented proof construction ----------------------------------------

def build_goal_oriented_proof(perm_x: Rule, perm_y: Rule, prohib_xy: Rule) -> str:
    """
    Goal G:
      Show that for every world w, Perm(X∧Y) and Proh(X∧Y) both hold
      for the same subject and target (X∧Y ≡ intersectionOf(X, Y)).

    Strategy:
      1) From the action axiom: ex:rentsell ≡ ex:rent ⊓ ex:sell.
      2) From (P1) and (P2): Permission(ex:rent) and Permission(ex:sell).
      3) From (1)+(2): derive Permission(ex:rentsell).
      4) From (F1): Prohibition(ex:rentsell).
      5) All rules are unconditional ⇒ the clash holds in every world.
    """
    comps = sorted(ACTIONS.components_of(prohib_xy.action)) if ACTIONS.components_of(prohib_xy.action) else []

    lines: List[str] = []
    lines.append("--- Goal-Oriented Proof Explanation ---\n")
    lines.append("Policies:")
    lines.append(f"  (P1) Permission:  {perm_x.assignee} may {perm_x.action} {perm_x.target}.")
    lines.append(f"  (P2) Permission:  {perm_y.assignee} may {perm_y.action} {perm_y.target}.")
    lines.append(f"  (F1) Prohibition: {prohib_xy.assignee} must-not {prohib_xy.action} {prohib_xy.target}.")
    lines.append("  All three rules are unconditional (no constraints/refinements).")
    lines.append("")
    lines.append("Action axiom (intersection):")
    lines.append(f"  (A) {prohib_xy.action} ≡ " + " ⊓ ".join(comps) + ".")
    lines.append("")
    lines.append("Derivation:")
    lines.append(f"  (S1) From (P1) and (P2): Perm({perm_x.action}) ∧ Perm({perm_y.action}).")
    lines.append(f"  (S2) From (A) and (S1): Perm({prohib_xy.action}) (permission on each component yields permission on their intersection).")
    lines.append(f"  (S3) From (F1): Proh({prohib_xy.action}).")
    lines.append("  (C) Therefore, in any world w, Perm(ex:rentsell) ∧ Proh(ex:rentsell).")
    lines.append("      Since w was arbitrary and the rules are unconditional, the global state is Conflict.")
    lines.append("")
    lines.append("Intuition (\"drink & drive\" analogy):")
    lines.append("  Being allowed to do X and allowed to do Y, while being forbidden to do X AND Y together,")
    lines.append("  creates a direct clash once the composite action X∧Y is recognized as an action in its own right.")
    return "\n".join(lines)

# --- Main ---------------------------------------------------------------------

def main() -> None:
    perm_rent   = POLICY_8A
    perm_sell   = POLICY_8B
    prohib_both = POLICY_8C

    result_uri = classify_global(perm_rent, perm_sell, prohib_both)

    # Always print the expected URI first (required by test harnesses)
    print(result_uri)

    # Then print a compact, goal-oriented proof explanation
    print()
    print(build_goal_oriented_proof(perm_rent, perm_sell, prohib_both))

if __name__ == "__main__":
    main()

