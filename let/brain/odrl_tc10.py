#!/usr/bin/env python3
"""
Testcase-10: Permission(student) ∨ Permission(employee); Prohibition(student ∧ employee)
=======================================================================================

Scenario (from ODRL Test Conflicts, testcase-10):
  - Alice is allowed to read if she is a student OR an employee.
  - Alice is prohibited to read if she is BOTH a student AND an employee.
  - The provided state-of-the-world (SOTW) has Alice as BOTH student and employee.

Global classification expected: Ambiguous
  "Some states of the world permit an action while other states of the world prohibit the action."

This script:
  1) Encodes the policies directly (no network access).
  2) Evaluates them across all states of the world for two predicates:
        Student(Alice) ∈ {False, True}
        Employee(Alice) ∈ {False, True}
  3) Decides the global activation state using the following semantics:

     For each world w, compute:
       Perm(w)  := (Student(w) or Employee(w))
       Proh(w)  := (Student(w) and Employee(w))
       Conflict(w) := Perm(w) and Proh(w)

     Aggregate over all worlds W:
       - ConflictAlways  := for all w in W, Conflict(w)
       - ExistsPermitted := exists w in W s.t. Perm(w)     (note: Conflict(w) implies Perm(w))
       - ExistsProhibit  := exists w in W s.t. Proh(w)     (note: Conflict(w) implies Proh(w))

     Global outcome:
       if ConflictAlways:           https://w3id.org/force/compliance-report#Conflict
       elif ExistsPermitted and ExistsProhibit:
                                    https://w3id.org/force/compliance-report#Ambiguous
       elif ExistsProhibit and not ExistsPermitted:
                                    https://w3id.org/force/compliance-report#Prohibited
       elif ExistsPermitted and not ExistsProhibit:
                                    https://w3id.org/force/compliance-report#Permitted
       else:                        https://w3id.org/force/compliance-report#NoConflict

  4) Always prints:
       - The URI for the global result
       - A concise goal-oriented proof explanation with explicit witness worlds

Note: The provided SOTW (Student=True, Employee=True) is used as a *witness*,
but the classification is made by quantifying over all possible worlds.
"""

from dataclasses import dataclass
from typing import List, Tuple

# --- Policy modeling (specialized to this testcase) --------------------------

@dataclass(frozen=True)
class World:
    student: bool
    employee: bool

    def __repr__(self) -> str:
        s = "T" if self.student else "F"
        e = "T" if self.employee else "F"
        return f"(Student={s}, Employee={e})"

@dataclass(frozen=True)
class Evaluation:
    world: World
    permitted: bool
    prohibited: bool

    @property
    def conflict(self) -> bool:
        return self.permitted and self.prohibited

def eval_policies(world: World) -> Evaluation:
    """
    Permissions:
      P1: permission if student
      P2: permission if employee
      => permitted(w) = student(w) OR employee(w)

    Prohibition:
      F1: prohibition if student AND employee
      => prohibited(w) = student(w) AND employee(w)
    """
    perm = (world.student or world.employee)
    prohib = (world.student and world.employee)
    return Evaluation(world, perm, prohib)

# --- Classification logic -----------------------------------------------------

REPORT_URIS = {
    "Conflict":    "https://w3id.org/force/compliance-report#Conflict",
    "Ambiguous":   "https://w3id.org/force/compliance-report#Ambiguous",
    "Prohibited":  "https://w3id.org/force/compliance-report#Prohibited",
    "Permitted":   "https://w3id.org/force/compliance-report#Permitted",
    "NoConflict":  "https://w3id.org/force/compliance-report#NoConflict",
}

def classify(evals: List[Evaluation]) -> str:
    conflict_always = all(ev.conflict for ev in evals)
    exists_perm     = any(ev.permitted for ev in evals)
    exists_prohib   = any(ev.prohibited for ev in evals)

    if conflict_always:
        return REPORT_URIS["Conflict"]
    if exists_perm and exists_prohib:
        return REPORT_URIS["Ambiguous"]
    if exists_prohib and not exists_perm:
        return REPORT_URIS["Prohibited"]
    if exists_perm and not exists_prohib:
        return REPORT_URIS["Permitted"]
    return REPORT_URIS["NoConflict"]

# --- Proof construction -------------------------------------------------------

def build_goal_oriented_proof(evals: List[Evaluation]) -> str:
    """
    Goal: Decide the global activation state. Show "Ambiguous" by witness worlds.

    Strategy:
      - Exhibit a world where the action is PROHIBITED.
      - Exhibit (possibly another) world where the action is PERMITTED.
      - Show not-all-worlds are in conflict.
    """
    # Partition worlds by outcome
    conflict_ws   = [ev.world for ev in evals if ev.conflict]
    permitted_ws  = [ev.world for ev in evals if ev.permitted and not ev.prohibited]
    prohibited_ws = [ev.world for ev in evals if ev.prohibited and not ev.permitted]
    pure_neutral  = [ev.world for ev in evals if not ev.permitted and not ev.prohibited]

    # In this testcase there is a conflict world (T,T); there may be a pure permitted world (T,F or F,T)
    # and a neutral world (F,F). We'll construct witnesses accordingly.
    lines: List[str] = []
    lines.append("--- Goal-Oriented Proof Explanation ---\n")
    lines.append("Policies:")
    lines.append("  (P1) Permission to read if recipient is a student.")
    lines.append("  (P2) Permission to read if recipient is an employee.")
    lines.append("  (F1) Prohibition to read if recipient is both a student AND an employee.")
    lines.append("")
    lines.append("World evaluation (Student, Employee) ↦ {Perm, Prohib, Conflict}:")
    for ev in evals:
        w = ev.world
        lines.append(f"  {w}: Perm={ev.permitted}, Prohib={ev.prohibited}, Conflict={ev.conflict}")
    lines.append("")

    # Witness worlds
    # A world with prohibition (possibly conflict world)
    w_prohib = None
    for ev in evals:
        if ev.prohibited:
            w_prohib = ev.world
            break

    # A world with permission (prefer a pure-permitted witness if available)
    w_permit = None
    if permitted_ws:
        w_permit = permitted_ws[0]
    else:
        # fall back to any world where permitted holds (could be a conflict world)
        for ev in evals:
            if ev.permitted:
                w_permit = ev.world
                break

    lines.append("Witness construction:")
    if w_permit is not None:
        lines.append(f"  (W1) Permitted world: {w_permit}  — satisfies P1 ∨ P2.")
    if w_prohib is not None:
        lines.append(f"  (W2) Prohibited world: {w_prohib} — satisfies F1 (student ∧ employee).")
    if conflict_ws:
        lines.append(f"  (W3) Conflict world exists: {conflict_ws[0]} — both permitted and prohibited here.")
    if pure_neutral:
        lines.append(f"  (W4) Neutral world exists: {pure_neutral[0]} — neither permitted nor prohibited.")

    lines.append("")
    lines.append("Derivation:")
    lines.append("  (S1) ∃ world with Prohib(read): witness W2.")
    lines.append("  (S2) ∃ world with Perm(read): witness W1.")
    lines.append("  (S3) Not all worlds are conflicts (e.g., W1 is pure-permitted or W4 is neutral).")
    lines.append("  (C) Since some worlds permit and some worlds prohibit, but not all are conflicts,")
    lines.append("      the global activation state is Ambiguous.")
    return "\n".join(lines)

# --- Main ---------------------------------------------------------------------

def main() -> None:
    # Enumerate all worlds (Student, Employee) for Alice
    worlds: List[World] = [
        World(False, False),  # neither
        World(True,  False),  # student only
        World(False, True),   # employee only
        World(True,  True),   # both (the provided SOTW)
    ]

    evals = [eval_policies(w) for w in worlds]
    result_uri = classify(evals)

    # Always print the expected URI first (required by test harnesses)
    print(result_uri)

    # Then print a compact, goal-oriented proof explanation
    print()
    print(build_goal_oriented_proof(evals))

if __name__ == "__main__":
    main()

