#!/usr/bin/env python3
"""
Testcase-7: Permission vs Prohibition in overlapping collections
================================================================

Scenario (from ODRL Test Conflicts, testcase-7):
  - policy7a: Permission to odrl:read ex:collectionX.
  - policy7b: Prohibition to odrl:read ex:collectionY.
  - Overlap: every member of collectionX is also a member of collectionY.

Expected global activation state: Conflict
  "The policies permit and prohibit the action for any possible state of the world."

Semantics used:
  - Collection lifting (permissions/prohibitions apply to members):
      Perm(act, C)  ∧ partOf(a, C)  ⇒  Perm(act, a)
      Proh(act, C)  ∧ partOf(a, C)  ⇒  Proh(act, a)
  - Because both rules are unconditional, any member 'a' that lies in X ∩ Y
    is simultaneously permitted and prohibited in every world.
"""

from dataclasses import dataclass
from typing import Dict, Set, Optional, List, Iterable

# --- Asset model (collections and partOf with transitive closure) -------------

class AssetModel:
    def __init__(self) -> None:
        # child -> set of direct parents (collections)
        self.part_of: Dict[str, Set[str]] = {}
        # track all seen nodes (assets or collections)
        self.nodes: Set[str] = set()
        # known collections
        self.collections: Set[str] = set()

    def add_collection(self, cid: str) -> None:
        self.collections.add(cid)
        self.nodes.add(cid)
        self.part_of.setdefault(cid, set())  # ensure key exists

    def add_asset(self, aid: str) -> None:
        self.nodes.add(aid)
        self.part_of.setdefault(aid, set())

    def add_part_of(self, child: str, parent_collection: str) -> None:
        self.add_asset(child)
        self.add_collection(parent_collection)
        self.part_of[child].add(parent_collection)

    def is_member_of(self, asset: str, collection: str) -> bool:
        """True iff 'asset' is (transitively) partOf 'collection'."""
        if asset == collection:
            return True
        visited: Set[str] = set()
        frontier: List[str] = [asset]
        while frontier:
            cur = frontier.pop()
            if cur in visited:
                continue
            visited.add(cur)
            for parent in self.part_of.get(cur, ()):
                if parent == collection:
                    return True
                frontier.append(parent)
        return False

    def members_of(self, collection: str) -> Set[str]:
        """All nodes that are (transitively) partOf the given collection (excluding the collection itself)."""
        return {n for n in self.nodes if n != collection and self.is_member_of(n, collection)}

    def intersection_members(self, c1: str, c2: str) -> Set[str]:
        """Members that lie in both collections."""
        m1 = self.members_of(c1)
        m2 = self.members_of(c2)
        return m1.intersection(m2)

# Build the asset universe from the testcase
ASSETS = AssetModel()
# Collections
ASSETS.add_collection("ex:collectionX")
ASSETS.add_collection("ex:collectionY")
# Documents and memberships:
ASSETS.add_part_of("ex:document1", "ex:collectionX")
ASSETS.add_part_of("ex:document2", "ex:collectionX")
ASSETS.add_part_of("ex:document1", "ex:collectionY")
ASSETS.add_part_of("ex:document2", "ex:collectionY")
ASSETS.add_part_of("ex:document3", "ex:collectionY")  # extra member of Y

# --- Policy modeling ----------------------------------------------------------

@dataclass(frozen=True)
class Rule:
    kind: str          # "permission" or "prohibition"
    assignee: str      # "ex:alice"
    action: str        # "odrl:read"
    target: str        # collection IRI
    refinement: Optional[str] = None  # None => unconditional

# Instantiate the two rules exactly as described
POLICY_7A = Rule(kind="permission",  assignee="ex:alice", action="odrl:read", target="ex:collectionX")
POLICY_7B = Rule(kind="prohibition", assignee="ex:alice", action="odrl:read", target="ex:collectionY")

# --- Report URIs --------------------------------------------------------------

REPORT_URIS = {
    "Conflict":    "https://w3id.org/force/compliance-report#Conflict",
    "Ambiguous":   "https://w3id.org/force/compliance-report#Ambiguous",
    "Prohibited":  "https://w3id.org/force/compliance-report#Prohibited",
    "Permitted":   "https://w3id.org/force/compliance-report#Permitted",
    "NoConflict":  "https://w3id.org/force/compliance-report#NoConflict",
}

# --- Conflict detection -------------------------------------------------------

def collection_conflict(permission: Rule, prohibition: Rule) -> bool:
    """
    Returns True iff:
      - permission vs prohibition,
      - same subject and action,
      - both unconditional,
      - AND collections overlap (∃a s.t. a ∈ X ∩ Y).
      Then Perm(read,a) and Proh(read,a) both hold for that a in all worlds.
    """
    if not (permission.kind == "permission" and prohibition.kind == "prohibition"):
        return False
    if not (permission.assignee == prohibition.assignee and permission.action == prohibition.action):
        return False
    if not (permission.refinement is None and prohibition.refinement is None):
        return False

    overlap = ASSETS.intersection_members(permission.target, prohibition.target)
    return len(overlap) > 0

def classify_global(permission: Rule, prohibition: Rule) -> str:
    if collection_conflict(permission, prohibition):
        return REPORT_URIS["Conflict"]
    return REPORT_URIS["NoConflict"]  # (not expected for this testcase)

# --- Goal-oriented proof construction ----------------------------------------

def build_goal_oriented_proof(permission: Rule, prohibition: Rule) -> str:
    """
    Goal G:
      Exhibit an asset a in X ∩ Y and show Perm(read,a) ∧ Proh(read,a).

    Strategy:
      1) From asset facts: partOf(document1, X) and partOf(document1, Y).
      2) Lifting: Perm(read, X) ⇒ Perm(read, document1); Proh(read, Y) ⇒ Proh(read, document1).
      3) Both rules are unconditional ⇒ clash holds in every world.
    """
    overlap = sorted(ASSETS.intersection_members(permission.target, prohibition.target))
    witness = overlap[0] if overlap else "<no-overlap>"

    lines: List[str] = []
    lines.append("--- Goal-Oriented Proof Explanation ---\n")
    lines.append("Policies:")
    lines.append(f"  (P1) Permission:  {permission.assignee} may {permission.action} {permission.target}.")
    lines.append(f"  (F1) Prohibition: {prohibition.assignee} must-not {prohibition.action} {prohibition.target}.")
    lines.append("  Both rules are unconditional (no constraints/refinements).")
    lines.append("")
    lines.append("Asset facts and overlap:")
    lines.append(f"  Members(X) = {sorted(ASSETS.members_of(permission.target))}")
    lines.append(f"  Members(Y) = {sorted(ASSETS.members_of(prohibition.target))}")
    lines.append(f"  Overlap X ∩ Y = {overlap}")
    lines.append("")
    lines.append("Collection lifting:")
    lines.append("  (L1) If Perm(act, C) and partOf(a, C), then Perm(act, a).")
    lines.append("  (L2) If Proh(act, C) and partOf(a, C), then Proh(act, a).")
    lines.append("")
    lines.append("Witness construction:")
    lines.append(f"  Choose a := {witness} ∈ X ∩ Y.")
    lines.append("  From (P1) and (L1): Perm(read, a).")
    lines.append("  From (F1) and (L2): Proh(read, a).")
    lines.append("  Therefore Perm(read, a) ∧ Proh(read, a) holds.")
    lines.append("")
    lines.append("Conclusion:")
    lines.append("  Since both rules are unconditional and the overlap is factual,")
    lines.append("  the clash persists in every possible world ⇒ Global state: Conflict.")
    return "\n".join(lines)

# --- Main ---------------------------------------------------------------------

def main() -> None:
    permission  = POLICY_7A
    prohibition = POLICY_7B

    result_uri = classify_global(permission, prohibition)

    # Always print the expected URI first (required by test harnesses)
    print(result_uri)

    # Then print a compact, goal-oriented proof explanation
    print()
    print(build_goal_oriented_proof(permission, prohibition))

if __name__ == "__main__":
    main()

