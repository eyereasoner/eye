#!/usr/bin/env python3
"""
Generic explicit-step proof builder: action-intersection conflicts (testcase-8)
===============================================================================

Testcase-8 summary (instance of the generic scheme):
  - Permissions (unconditional): ex:alice may ex:rent ex:collectionX; ex:alice may ex:sell ex:collectionX.
  - Action definition: ex:rentsell ≡ ex:rent ⊓ ex:sell  (intersection of actions).
  - Prohibition (unconditional): ex:alice must-not ex:rentsell ex:collectionX.

Generic reading:
  If a composite action C is defined as ∩ of components {A1,…,An}, then
    Perm(A1,t,w) ∧ … ∧ Perm(An,t,w)  ⇒  Perm(C,t,w).
  Together with Proh(C,t,w) this yields a clash in that world; with unconditional
  rules it yields a clash in every world ⇒ Conflict.

No external libraries required.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Sequence, Tuple

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
    args: Tuple[Any, ...]  # e.g., (assignee, action, target, world)
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
    body: Any  # usually an Atom referring to var
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
        if k == "formula":           return p.pretty()
        if k == "universe-def":      return f"{p[0]} := {p[1]}"
        if k == "choose-world":      return f"{p[0]} ∈ {p[1]} (arbitrary)"
        if k == "action-def":        return f"{p['composite']} ≡ " + " ⊓ ".join(p["components"])
        if k == "perm-intro-cap":    return "∧i Perm(Ai,t,w) ⇒ Perm(∩Ai,t,w)"
        if k == "classification":    return f"Global activation state = {p}"
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
# Action algebra with "intersectionOf" (generic, reusable)
# -----------------------------------------------------------------------------

class ActionAlgebra:
    """Map composite actions to their component actions."""
    def __init__(self) -> None:
        self.intersections: Dict[str, Tuple[str, ...]] = {}

    def define_intersection(self, composite: str, components: Sequence[str]) -> None:
        if len(components) < 2:
            raise ValueError("Intersection must contain at least two component actions.")
        self.intersections[composite] = tuple(components)

    def components_of(self, composite: str) -> Optional[Tuple[str, ...]]:
        return self.intersections.get(composite)

# -----------------------------------------------------------------------------
# Reusable builders for unconditional premises
# -----------------------------------------------------------------------------

def forall_perm_unconditional(var: Term, assignee: str, action: str, target: str) -> ForAll:
    return ForAll(var, Atom("Perm", (assignee, action, target, var)))

def forall_proh_unconditional(var: Term, assignee: str, action: str, target: str) -> ForAll:
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
# Generic proof builder: Perm(Ai), Proh(∩Ai), with composite defined as intersection
# -----------------------------------------------------------------------------

def build_intersection_conflict_proof(
    assignee: str,
    target: str,
    component_actions: Sequence[str],
    composite_action: str,
    algebra: ActionAlgebra,
    universe_name: str = "W",
    witness_name: str = "w0"
) -> Tuple[Proof, str]:
    """
    Construct an explicit proof that:
      ∀w Perm(Ai, target, w) for all components Ai,
      ∀w Proh(Composite, target, w),
      and Composite ≡ ∩ Ai,
    entail Conflict.
    """
    comps = algebra.components_of(composite_action)
    if comps is None or tuple(component_actions) != comps:
        raise ValueError("Composite action is not defined as the intersection of the given components.")

    proof = Proof()
    w = Term("w")

    # [1..k] Unconditional Permission premises for each component action
    perm_step_ids: List[int] = []
    for a in component_actions:
        prem = forall_perm_unconditional(w, assignee, a, target)
        s = proof.add("Premise", [], Conclusion("formula", prem),
                      notes=f"Unconditional Permission on component action {a}")
        perm_step_ids.append(s)

    # [k+1] Unconditional Prohibition premise for the composite action
    proh_prem = forall_proh_unconditional(w, assignee, composite_action, target)
    s_proh = proof.add("Premise", [], Conclusion("formula", proh_prem),
                       notes=f"Unconditional Prohibition on composite action {composite_action}")

    # [k+2] Action definition: composite ≡ intersection of components
    s_def = proof.add("Action-Def (∩)", [],
                      Conclusion("action-def", {"composite": composite_action, "components": list(component_actions)}),
                      notes="Composite action defined as an intersection of component actions")

    # [k+3] Generic introduction rule for intersection on permissions
    s_cap_intro = proof.add("Perm-Intro(∩)", [s_def],
                            Conclusion("perm-intro-cap", None),
                            notes="From all component permissions infer permission on the composite")

    # [k+4] Universe of worlds (unrestricted)
    s_univ = proof.add("Universe-Def", [], Conclusion("universe-def", (universe_name, "all possible worlds")),
                       notes="No constraints/refinements restrict applicability")

    # [k+5] Choose an arbitrary world w0 ∈ W
    s_w0 = proof.add("Arbitrary-World", [s_univ], Conclusion("choose-world", (witness_name, universe_name)),
                     notes="World chosen arbitrarily; argument must not depend on which one")

    # [..] UE: instantiate each component permission at w0
    inst_perm_steps: List[int] = []
    for i, a in enumerate(component_actions):
        inst = Atom("Perm", (assignee, a, target, witness_name))
        sid = proof.add("Universal-Elim", [perm_step_ids[i], s_w0], Conclusion("formula", inst),
                        notes=f"Instantiate component permission {a} at {witness_name}")
        inst_perm_steps.append(sid)

    # [..] Apply Perm-Intro(∩) to obtain permission on the composite at w0
    inst_perm_composite = Atom("Perm", (assignee, composite_action, target, witness_name))
    s_perm_comp = proof.add("Intersection-Apply (Perm)", [s_cap_intro] + inst_perm_steps,
                            Conclusion("formula", inst_perm_composite),
                            notes="All component permissions hold ⇒ permission on the composite")

    # [..] UE: prohibition on the composite at w0
    inst_proh_composite = Atom("Proh", (assignee, composite_action, target, witness_name))
    s_proh_comp = proof.add("Universal-Elim", [s_proh, s_w0], Conclusion("formula", inst_proh_composite),
                            notes="Instantiate composite prohibition at the same world")

    # [..] ∧-Introduction: Perm(composite) ∧ Proh(composite) at w0
    conj = And(inst_perm_composite, inst_proh_composite)
    s_conj = proof.add("And-Intro", [s_perm_comp, s_proh_comp], Conclusion("formula", conj),
                       notes="Clash at the arbitrary world on the same composite action")

    # [..] ∀-Introduction: generalize clash to all worlds
    general = ForAll(w, And(
        Atom("Perm", (assignee, composite_action, target, w)),
        Atom("Proh", (assignee, composite_action, target, w))
    ))
    s_forall = proof.add("ForAll-Intro", [s_conj], Conclusion("formula", general),
                         notes=f"Since {witness_name} was arbitrary, the clash holds in all worlds")

    # [..] Classification: Conflict
    result_uri = REPORT_URIS["Conflict"]
    proof.add("Classification", [s_forall], Conclusion("classification", result_uri),
              notes="Unconditional component permissions + prohibition on their intersection ⇒ standing clash")

    return proof, result_uri

# -----------------------------------------------------------------------------
# Demonstration: instantiate for testcase-8
# -----------------------------------------------------------------------------

def main() -> None:
    assignee = "ex:alice"
    target   = "ex:collectionX"
    comps    = ("ex:rent", "ex:sell")
    compo    = "ex:rentsell"

    # Define action algebra: rentsell ≡ rent ⊓ sell
    ALG = ActionAlgebra()
    ALG.define_intersection(compo, comps)

    proof, result_uri = build_intersection_conflict_proof(
        assignee=assignee,
        target=target,
        component_actions=comps,
        composite_action=compo,
        algebra=ALG,
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

