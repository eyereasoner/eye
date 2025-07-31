#!/usr/bin/env python3
"""
Generic explicit-step proof builder for action taxonomy conflicts (testcase-2)
=============================================================================

Testcase-2 summary:
  - Unconditional Permission: ex:alice may odrl:read ex:resourceX.
  - Unconditional Prohibition: ex:alice must-not odrl:use ex:resourceX.
  - Action taxonomy: odrl:read ⊑ odrl:use (read is a sub-action of use).

Subsumption rule used (generic):
  If child ⊑ parent then Proh(parent, t, w) ⇒ Proh(child, t, w).

Therefore, in every world w:
  Perm(read, X, w) and Proh(use, X, w) ⇒ Proh(read, X, w) by subsumption,
  hence Perm(read, X, w) ∧ Proh(read, X, w) ⇒ Global state: Conflict.

No external libraries required.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple, Set

# -----------------------------------------------------------------------------
# Tiny logic: terms, formulas, pretty-printers (generic)
# -----------------------------------------------------------------------------

@dataclass(frozen=True)
class Term:
    name: str
    def __str__(self) -> str: return self.name

@dataclass(frozen=True)
class Atom:
    pred: str
    args: Tuple[Any, ...]
    def pretty(self) -> str:
        def fmt(x: Any) -> str: return x if isinstance(x, str) else str(x)
        return f"{self.pred}(" + ", ".join(fmt(a) for a in self.args) + ")"

@dataclass(frozen=True)
class And:
    left: Any
    right: Any
    def pretty(self) -> str:
        L = self.left.pretty() if hasattr(self.left, "pretty") else str(self.left)
        R = self.right.pretty() if hasattr(self.right, "pretty") else str(self.right)
        return f"({L} ∧ {R})"

@dataclass(frozen=True)
class ForAll:
    var: Term
    body: Any  # body may reference var
    def pretty(self) -> str:
        return f"∀{self.var}. {self.body.pretty()}"

# -----------------------------------------------------------------------------
# Proof kernel (pretty output only)
# -----------------------------------------------------------------------------

@dataclass
class Conclusion:
    kind: str
    payload: Any
    def pretty(self) -> str:
        k, p = self.kind, self.payload
        if k == "formula":        return p.pretty()
        if k == "universe-def":   return f"{p[0]} := {p[1]}"
        if k == "choose-world":   return f"{p[0]} ∈ {p[1]} (arbitrary)"
        if k == "taxonomy-fact":  return f"{p[0]} ⊑ {p[1]}"
        if k == "subsumption":    return f"Proh({p['parent']}) ⇒ Proh({p['child']})"
        if k == "classification": return f"Global activation state = {p}"
        return str(p)

@dataclass
class Step:
    id: int
    rule: str
    premises: List[int]
    conclusion: Conclusion
    notes: Optional[str] = None

@dataclass
class Proof:
    steps: List[Step] = field(default_factory=list)
    def add(self, rule: str, premises: List[int], conclusion: Conclusion, notes: Optional[str] = None) -> int:
        sid = len(self.steps) + 1
        self.steps.append(Step(sid, rule, premises, conclusion, notes))
        return sid
    def pretty(self) -> str:
        lines = []
        for s in self.steps:
            prem = f" [{', '.join(map(str, s.premises))}]" if s.premises else ""
            note = f"  // {s.notes}" if s.notes else ""
            lines.append(f"[{s.id}] {s.rule}{prem}: {s.conclusion.pretty()}{note}")
        return "\n".join(lines)

# -----------------------------------------------------------------------------
# Action hierarchy (generic, reusable)
# -----------------------------------------------------------------------------

class ActionHierarchy:
    def __init__(self) -> None:
        # child -> set of direct parents
        self.parents: Dict[str, Set[str]] = {}

    def add_subaction(self, child: str, parent: str) -> None:
        self.parents.setdefault(child, set()).add(parent)
        self.parents.setdefault(parent, set())  # ensure parent exists

    def is_subaction_of(self, child: str, parent: str) -> bool:
        """Return True iff child ⊑ parent (reflexive-transitive closure)."""
        if child == parent:
            return True
        visited: Set[str] = set()
        stack: List[str] = [child]
        while stack:
            cur = stack.pop()
            if cur in visited:
                continue
            visited.add(cur)
            for p in self.parents.get(cur, ()):
                if p == parent:
                    return True
                stack.append(p)
        return False

# -----------------------------------------------------------------------------
# Reusable builders for unconditional premises
# -----------------------------------------------------------------------------

def forall_perm_unconditional(var: Term, assignee: str, action: str, target: str) -> ForAll:
    # Unconditional: holds for all worlds
    return ForAll(var, Atom("Perm", (assignee, action, target, var)))

def forall_proh_unconditional(var: Term, assignee: str, action: str, target: str) -> ForAll:
    # Unconditional: holds for all worlds
    return ForAll(var, Atom("Proh", (assignee, action, target, var)))

# -----------------------------------------------------------------------------
# Report URIs
# -----------------------------------------------------------------------------

REPORT_URIS = {
    "Conflict":   "https://w3id.org/force/compliance-report#Conflict",
    "Ambiguous":  "https://w3id.org/force/compliance-report#Ambiguous",
    "Prohibited": "https://w3id.org/force/compliance-report#Prohibited",
    "Permitted":  "https://w3id.org/force/compliance-report#Permitted",
    "NoConflict": "https://w3id.org/force/compliance-report#NoConflict",
}

# -----------------------------------------------------------------------------
# Generic proof builder for taxonomy-based conflict: Perm(child), Proh(parent), child ⊑ parent
# -----------------------------------------------------------------------------

def build_taxonomic_conflict_proof(assignee: str,
                                   child_action: str,
                                   parent_action: str,
                                   target: str,
                                   hierarchy: ActionHierarchy,
                                   universe_name: str = "W",
                                   witness_name: str = "w0") -> Tuple[Proof, str]:
    """
    Build an explicit proof that:
      - ∀w Perm(child_action, target, w)
      - ∀w Proh(parent_action, target, w)
      - child_action ⊑ parent_action
    entail Conflict.
    """
    if not hierarchy.is_subaction_of(child_action, parent_action):
        raise ValueError(f"Hierarchy does not contain {child_action} ⊑ {parent_action}")

    proof = Proof()
    w = Term("w")

    # [1] Unconditional Permission premise on child action
    perm_prem = forall_perm_unconditional(w, assignee, child_action, target)
    s1 = proof.add("Premise", [], Conclusion("formula", perm_prem),
                   notes=f"Unconditional Permission on {child_action}")

    # [2] Unconditional Prohibition premise on parent action
    proh_prem = forall_proh_unconditional(w, assignee, parent_action, target)
    s2 = proof.add("Premise", [], Conclusion("formula", proh_prem),
                   notes=f"Unconditional Prohibition on {parent_action}")

    # [3] Taxonomy fact: child ⊑ parent
    s3 = proof.add("Taxonomy-Fact", [], Conclusion("taxonomy-fact", (child_action, parent_action)),
                   notes="Action hierarchy contains this subaction relation")

    # [4] Subsumption rule (Prohibition lifts down the hierarchy)
    s4 = proof.add("Subsumption-Rule", [s3],
                   Conclusion("subsumption", {"parent": f"{parent_action}", "child": f"{child_action}"}),
                   notes="If child ⊑ parent then Proh(parent, t, w) ⇒ Proh(child, t, w)")

    # [5] Define the universe of worlds
    s5 = proof.add("Universe-Def", [], Conclusion("universe-def", (universe_name, "all possible worlds")),
                   notes="No conditions restrict applicability")

    # [6] Choose an arbitrary world w0 ∈ W
    s6 = proof.add("Arbitrary-World", [s5], Conclusion("choose-world", (witness_name, universe_name)),
                   notes="World chosen arbitrarily; argument must not depend on which one")

    # [7] UE on Permission at w0
    inst_perm = Atom("Perm", (assignee, child_action, target, witness_name))
    s7 = proof.add("Universal-Elim", [s1, s6], Conclusion("formula", inst_perm),
                   notes=f"Instantiate Permission({child_action}) at {witness_name}")

    # [8] UE on Prohibition(parent) at w0
    inst_proh_parent = Atom("Proh", (assignee, parent_action, target, witness_name))
    s8 = proof.add("Universal-Elim", [s2, s6], Conclusion("formula", inst_proh_parent),
                   notes=f"Instantiate Prohibition({parent_action}) at {witness_name}")

    # [9] Subsumption: from Proh(parent) and child ⊑ parent infer Proh(child)
    inst_proh_child = Atom("Proh", (assignee, child_action, target, witness_name))
    s9 = proof.add("Subsumption-Apply", [s4, s8], Conclusion("formula", inst_proh_child),
                   notes="Apply Prohibition subsumption along child ⊑ parent")

    # [10] ∧-Introduction: Perm(child) ∧ Proh(child) at w0
    conj = And(inst_perm, inst_proh_child)
    s10 = proof.add("And-Intro", [s7, s9], Conclusion("formula", conj),
                    notes="Clash at the arbitrary world")

    # [11] ∀-Introduction: generalize clash to all worlds
    general = ForAll(w, And(
        Atom("Perm", (assignee, child_action, target, w)),
        Atom("Proh", (assignee, child_action, target, w))
    ))
    s11 = proof.add("ForAll-Intro", [s10], Conclusion("formula", general),
                    notes="Since w0 was arbitrary, the conjunction holds in all worlds")

    # [12] Classification: Conflict
    result_uri = REPORT_URIS["Conflict"]
    proof.add("Classification", [s11], Conclusion("classification", result_uri),
              notes="Perm and Proh on the same (child) action hold in every world")

    return proof, result_uri

# -----------------------------------------------------------------------------
# Demonstration: instantiate for testcase-2
# -----------------------------------------------------------------------------

def main() -> None:
    assignee      = "ex:alice"
    child_action  = "odrl:read"
    parent_action = "odrl:use"
    target        = "ex:resourceX"

    # Build the action hierarchy: read ⊑ use
    H = ActionHierarchy()
    H.add_subaction(child_action, parent_action)

    proof, result_uri = build_taxonomic_conflict_proof(
        assignee=assignee,
        child_action=child_action,
        parent_action=parent_action,
        target=target,
        hierarchy=H,
        universe_name="W",
        witness_name="w0"
    )

    # 1) Expected URI first
    print(result_uri)

    # 2) Pretty, numbered proof
    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

if __name__ == "__main__":
    main()

