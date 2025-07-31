#!/usr/bin/env python3
"""
Generic explicit-step proof builder for equality-based activation conditions
===========================================================================

Testcase-5 summary:
  - Permission:  odrl:read ex:resourceX  when ex:age = 18.
  - Prohibition: odrl:read ex:resourceX  when ex:age = 18.
  The activation conditions are identical; whenever the condition holds
  the action is both permitted and prohibited ⇒ Conflict.

This script provides:
  - A tiny reusable proof kernel (terms, formulas, explicit steps).
  - A generic builder `build_equality_overlap_proof` that works for any
    discrete attribute with equality conditions (e.g., age==k, region==EU, …).

No external libraries required.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Iterable, List, Optional, Set, Tuple, Union, FrozenSet

# -----------------------------------------------------------------------------
# Helpers for finite sets (generic over discrete attributes)
# -----------------------------------------------------------------------------

def set_str(S: FrozenSet[Any]) -> str:
    return "{" + ", ".join(map(str, sorted(S))) + "}"

def pick_witness(S: FrozenSet[Any]) -> Any:
    assert len(S) > 0, "Cannot pick a witness from an empty set."
    return sorted(S)[0]

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
class ForAllInSet:
    var: Term
    S: FrozenSet[Any]           # finite activation set for the condition
    body: Atom                  # usually Perm/Proh(..., var)
    def pretty(self) -> str:
        return f"∀{self.var}∈{set_str(self.S)} {self.body.pretty()}"

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
        if k == "set-eq":
            leftA, leftB, inter = p
            return f"{set_str(leftA)} ∩ {set_str(leftB)} = {set_str(inter)}"
        if k == "set-def":        return f"{p[0]} := {set_str(p[1])}"
        if k == "membership":     return f"{p[0]} ∈ {set_str(p[1])}"
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
# Reusable builders for equality-based premises
# -----------------------------------------------------------------------------

def forall_perm_eq(var: Term, attr_name: str, attr_values: FrozenSet[Any],
                   assignee: str, action: str, target: str) -> ForAllInSet:
    """
    ∀v ∈ attr_values. Perm(assignee, action, target, attr_name=v)
    """
    return ForAllInSet(var, attr_values, Atom("Perm", (assignee, action, target, f"{attr_name}={var.name}")))

def forall_proh_eq(var: Term, attr_name: str, attr_values: FrozenSet[Any],
                   assignee: str, action: str, target: str) -> ForAllInSet:
    """
    ∀v ∈ attr_values. Proh(assignee, action, target, attr_name=v)
    """
    return ForAllInSet(var, attr_values, Atom("Proh", (assignee, action, target, f"{attr_name}={var.name}")))

# -----------------------------------------------------------------------------
# Report URIs and classification (generic over finite sets)
# -----------------------------------------------------------------------------

REPORT_URIS = {
    "Conflict":   "https://w3id.org/force/compliance-report#Conflict",
    "Ambiguous":  "https://w3id.org/force/compliance-report#Ambiguous",
    "Prohibited": "https://w3id.org/force/compliance-report#Prohibited",
    "Permitted":  "https://w3id.org/force/compliance-report#Permitted",
    "NoConflict": "https://w3id.org/force/compliance-report#NoConflict",
}

@dataclass
class OverlapResult:
    uri: str
    overlap: FrozenSet[Any]
    mode: str  # "empty" | "subset_conflict" | "proh_subset_ambiguous" | "partial_ambiguous"

def classify_overlap(perm_set: FrozenSet[Any], proh_set: FrozenSet[Any]) -> OverlapResult:
    inter = perm_set.intersection(proh_set)
    if len(inter) == 0:
        return OverlapResult(REPORT_URIS["NoConflict"], frozenset(), "empty")
    if perm_set.issubset(proh_set):
        return OverlapResult(REPORT_URIS["Conflict"], inter, "subset_conflict")
    if proh_set.issubset(perm_set):
        return OverlapResult(REPORT_URIS["Ambiguous"], inter, "proh_subset_ambiguous")
    return OverlapResult(REPORT_URIS["Ambiguous"], inter, "partial_ambiguous")

# -----------------------------------------------------------------------------
# Generic proof builder for equality-based activation overlap
# -----------------------------------------------------------------------------

def build_equality_overlap_proof(
    attr_name: str,
    perm_values: FrozenSet[Any],
    proh_values: FrozenSet[Any],
    assignee: str,
    action: str,
    target: str,
    overlap_name: str = "I",
    witness_label: str = "v0"
) -> Tuple[Proof, str]:
    """
    Build an explicit proof for equality-based activation sets:
      - ∀v ∈ P: Perm(...)
      - ∀v ∈ F: Proh(...)
      - Overlap I = P ∩ F
    Classification:
      - NoConflict if I=∅
      - Conflict    if P ⊆ F  (standing clash on all permitted points)
      - Ambiguous   otherwise
    """
    proof = Proof()
    v = Term("v")

    # [1] Permission premise over perm_values
    perm_prem = forall_perm_eq(v, attr_name, perm_values, assignee, action, target)
    s1 = proof.add("Premise", [], Conclusion("formula", perm_prem),
                   notes=f"Permission holds whenever {attr_name} ∈ {set_str(perm_values)}")

    # [2] Prohibition premise over proh_values
    proh_prem = forall_proh_eq(v, attr_name, proh_values, assignee, action, target)
    s2 = proof.add("Premise", [], Conclusion("formula", proh_prem),
                   notes=f"Prohibition holds whenever {attr_name} ∈ {set_str(proh_values)}")

    # [3] Set intersection P ∩ F
    inter = perm_values.intersection(proh_values)
    if len(inter) == 0:
        s3 = proof.add("Set-Intersection", [], Conclusion("set-eq", (perm_values, proh_values, inter)),
                       notes="No shared activations")
        res = classify_overlap(perm_values, proh_values)
        proof.add("Classification", [s3], Conclusion("classification", res.uri),
                  notes="No overlap ⇒ no direct clash")
        return proof, res.uri

    s3 = proof.add("Set-Intersection", [], Conclusion("set-eq", (perm_values, proh_values, inter)),
                   notes="Compute overlap of activation sets")

    # [4] Define overlap name
    s4 = proof.add("Definition", [s3], Conclusion("set-def", (overlap_name, inter)),
                   notes=f"Name the overlap as {overlap_name}")

    # [5] Choose a witness value from the overlap
    w = pick_witness(inter)
    s5 = proof.add("Choose", [s4], Conclusion("membership", (w, inter)),
                   notes=f"Witness {witness_label} = {w} ∈ {overlap_name}")

    # [6] UE: Perm at the witness
    inst_perm = Atom("Perm", (assignee, action, target, f"{attr_name}={witness_label}"))
    s6 = proof.add("Universal-Elim", [s1, s5], Conclusion("formula", inst_perm),
                   notes="Instantiate permission at the witness")

    # [7] UE: Proh at the witness
    inst_proh = Atom("Proh", (assignee, action, target, f"{attr_name}={witness_label}"))
    s7 = proof.add("Universal-Elim", [s2, s5], Conclusion("formula", inst_proh),
                   notes="Instantiate prohibition at the witness")

    # [8] ∧-Introduction: Perm ∧ Proh at the witness value
    conj = And(inst_perm, inst_proh)
    s8 = proof.add("And-Intro", [s6, s7], Conclusion("formula", conj),
                   notes="Clash at the witness point")

    # [9] ∀-Introduction over the entire overlap I
    general = ForAllInSet(v, inter, And(
        Atom("Perm", (assignee, action, target, f"{attr_name}=v")),
        Atom("Proh", (assignee, action, target, f"{attr_name}=v"))
    ))
    s9 = proof.add("ForAll-Intro", [s8], Conclusion("formula", general),
                   notes=f"Generalize the clash over all elements of {overlap_name}")

    # [10] Classification based on set relationship
    res = classify_overlap(perm_values, proh_values)
    reasoning = {
        "subset_conflict":       f"{overlap_name} equals the entire permission activation set ⇒ standing clash",
        "partial_ambiguous":     f"{overlap_name} is a strict partial overlap ⇒ some values permit, some prohibit",
        "proh_subset_ambiguous": f"Prohibition ⊆ Permission ⇒ both clash (inside {overlap_name}) and permit-only values (outside)",
        "empty":                 "No shared values ⇒ no direct clash",
    }[res.mode]
    proof.add("Classification", [s9], Conclusion("classification", res.uri), notes=reasoning)

    return proof, res.uri

# -----------------------------------------------------------------------------
# Demonstration: instantiate for testcase-5
# -----------------------------------------------------------------------------

def main() -> None:
    # Labels
    assignee = "ex:alice"
    action   = "odrl:read"
    target   = "ex:resourceX"
    attr     = "ex:age"

    # Equality conditions from the testcase: age == 18 on both sides
    perm_values = frozenset({18})
    proh_values = frozenset({18})

    # Build proof with the generic engine
    proof, result_uri = build_equality_overlap_proof(
        attr_name=attr,
        perm_values=perm_values,
        proh_values=proh_values,
        assignee=assignee,
        action=action,
        target=target,
        overlap_name="I",
        witness_label="v0"
    )

    # 1) Print expected URI first
    print(result_uri)

    # 2) Pretty, numbered proof
    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

if __name__ == "__main__":
    main()

