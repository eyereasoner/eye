#!/usr/bin/env python3
"""
Testcase-9: Permission date in 2025, Prohibition date before 2026
=================================================================

Scenario (from ODRL Test Conflicts, testcase-9):
  - policy9a (Permission): odrl:read allowed when 2025-01-01 ≤ date ≤ 2025-12-31.
  - policy9b (Prohibition): odrl:read forbidden when date < 2026-01-01.

Expected global activation state: Conflict
  Rationale: The permission window [2025-01-01, 2025-12-31] lies entirely within
  the prohibition condition (date < 2026-01-01). Therefore, for every date in 2025,
  the action is simultaneously permitted and prohibited.
"""

from dataclasses import dataclass
from datetime import date, timedelta
from typing import Optional, Tuple, List

# --- Interval utilities over dates -------------------------------------------

# Interval is (lower, lower_closed, upper, upper_closed), with None for ±∞
DateInterval = Tuple[Optional[date], bool, Optional[date], bool]

def interval_from_op(op: str, value: date) -> DateInterval:
    """
    Map ODRL-like comparison ops to date intervals.
      - 'lt'   : (-∞, value)             upper open
      - 'lteq' : (-∞, value]             upper closed
      - 'gt'   : (value, +∞)             lower open
      - 'gteq' : [value, +∞)             lower closed
      - 'eq'   : [value, value]          singleton
    """
    if op == "lt":
        return (None, False, value, False)
    if op == "lteq":
        return (None, False, value, True)
    if op == "gt":
        return (value, False, None, False)
    if op == "gteq":
        return (value, True, None, False)
    if op == "eq":
        return (value, True, value, True)
    raise ValueError(f"Unsupported operator: {op}")

def interval_intersection(a: DateInterval, b: DateInterval) -> Optional[DateInterval]:
    """Intersection of two date intervals; None if empty."""
    (al, alc, au, auc) = a
    (bl, blc, bu, buc) = b

    # Lower bound
    if al is None:
        nl, nlc = bl, blc
    elif bl is None:
        nl, nlc = al, alc
    else:
        if al > bl:
            nl, nlc = al, alc
        elif bl > al:
            nl, nlc = bl, blc
        else:  # equal
            nl, nlc = al, alc and blc

    # Upper bound
    if au is None:
        nu, nuc = bu, buc
    elif bu is None:
        nu, nuc = au, auc
    else:
        if au < bu:
            nu, nuc = au, auc
        elif bu < au:
            nu, nuc = bu, buc
        else:  # equal
            nu, nuc = au, auc and buc

    # Emptiness check
    if nl is not None and nu is not None:
        if nl > nu:
            return None
        if nl == nu and not (nlc and nuc):
            return None

    return (nl, nlc, nu, nuc)

def pick_witness(d: DateInterval) -> date:
    """
    Deterministically pick a date inside a non-empty interval.
    Preference: lower bound if closed; otherwise the next day after lower bound,
    or the day before upper bound if lower is -∞.
    """
    (l, lc, u, uc) = d
    if l is not None:
        return l if lc else (l + timedelta(days=1))
    # l is -∞: choose u if closed, otherwise day before u
    assert u is not None
    return u if uc else (u - timedelta(days=1))

# --- Policy modeling (specialized to this testcase) ---------------------------

@dataclass(frozen=True)
class Constraint:
    left_operand: str   # "odrl:dateTime"
    operator: str       # "gteq", "lteq", "lt", ...
    right_operand: date

    def as_interval(self) -> DateInterval:
        return interval_from_op(self.operator, self.right_operand)

@dataclass(frozen=True)
class LogicalConstraint:
    # For this testcase we only need conjunction ("and") of constraints.
    and_constraints: Tuple[Constraint, Constraint]

    def as_interval(self) -> DateInterval:
        i1 = self.and_constraints[0].as_interval()
        i2 = self.and_constraints[1].as_interval()
        inter = interval_intersection(i1, i2)
        if inter is None:
            # Conjunction is unsatisfiable
            return (date.min, True, date.min, False)  # empty sentinel
        return inter

@dataclass(frozen=True)
class Rule:
    kind: str                 # "permission" or "prohibition"
    assignee: str             # "ex:alice"
    action: str               # "odrl:read"
    target: str               # "ex:resourceX"
    # Exactly one of the following is used for this testcase:
    logical_constraint: Optional[LogicalConstraint] = None
    constraint: Optional[Constraint] = None

    def active_interval(self) -> Optional[DateInterval]:
        if self.logical_constraint:
            return self.logical_constraint.as_interval()
        if self.constraint:
            return self.constraint.as_interval()
        return None  # unconditional (not used in this testcase)

# Instantiate rules as per the markdown
PERMISSION_2025 = Rule(
    kind="permission",
    assignee="ex:alice",
    action="odrl:read",
    target="ex:resourceX",
    logical_constraint=LogicalConstraint((
        Constraint("odrl:dateTime", "gteq", date.fromisoformat("2025-01-01")),
        Constraint("odrl:dateTime", "lteq", date.fromisoformat("2025-12-31")),
    ))
)

PROHIB_BEFORE_2026 = Rule(
    kind="prohibition",
    assignee="ex:alice",
    action="odrl:read",
    target="ex:resourceX",
    constraint=Constraint("odrl:dateTime", "lt", date.fromisoformat("2026-01-01")),
)

# --- Report URIs --------------------------------------------------------------

REPORT_URIS = {
    "Conflict":    "https://w3id.org/force/compliance-report#Conflict",
    "Ambiguous":   "https://w3id.org/force/compliance-report#Ambiguous",
    "Prohibited":  "https://w3id.org/force/compliance-report#Prohibited",
    "Permitted":   "https://w3id.org/force/compliance-report#Permitted",
    "NoConflict":  "https://w3id.org/force/compliance-report#NoConflict",
}

# --- Conflict detection -------------------------------------------------------

def classify(permission: Rule, prohibition: Rule) -> Tuple[str, Optional[DateInterval]]:
    """
    Compute the intersection of the permission's active window and the
    prohibition's active window. If non-empty, the same subject/action/target
    is both permitted and prohibited on those dates ⇒ Conflict.
    """
    if not (
        permission.kind == "permission" and
        prohibition.kind == "prohibition" and
        permission.assignee == prohibition.assignee and
        permission.action == prohibition.action and
        permission.target == prohibition.target
    ):
        return (REPORT_URIS["NoConflict"], None)

    p_int = permission.active_interval()
    f_int = prohibition.active_interval()
    if p_int is None or f_int is None:
        return (REPORT_URIS["NoConflict"], None)

    inter = interval_intersection(p_int, f_int)
    if inter is None:
        return (REPORT_URIS["NoConflict"], None)

    # Non-empty overlap of activation windows ⇒ clash on all dates in 'inter'
    return (REPORT_URIS["Conflict"], inter)

# --- Proof construction -------------------------------------------------------

def build_goal_oriented_proof(permission: Rule, prohibition: Rule, inter: DateInterval) -> str:
    """
    Goal G:
      Exhibit a date d in the overlap and show Perm(read,d) ∧ Prohib(read,d).
    """
    witness = pick_witness(inter)

    (pl, plc, pu, puc) = permission.active_interval()
    (fl, flc, fu, fuc) = prohibition.active_interval()

    def fmt_interval(iv: DateInterval) -> str:
        (l, lc, u, uc) = iv
        l_s = "-∞" if l is None else l.isoformat()
        u_s = "+∞" if u is None else u.isoformat()
        return f"{'[' if lc else '('}{l_s}, {u_s}{']' if uc else ')'}"

    lines: List[str] = []
    lines.append("--- Goal-Oriented Proof Explanation ---\n")
    lines.append("Policies (activation windows over odrl:dateTime):")
    lines.append(f"  (P1) Permission window : {fmt_interval(permission.active_interval())}  "
                 f"(i.e., 2025-01-01 ≤ d ≤ 2025-12-31).")
    lines.append(f"  (F1) Prohibition window: {fmt_interval(prohibition.active_interval())}  "
                 f"(i.e., d < 2026-01-01).")
    lines.append("")
    lines.append("Overlap analysis:")
    lines.append(f"  (I) Intersection = {fmt_interval(inter)} (this is exactly the entire year 2025).")
    lines.append("")
    lines.append("Witness construction:")
    lines.append(f"  Choose d := {witness.isoformat()} ∈ Intersection.")
    lines.append("  From (P1): Perm(odrl:read, d).")
    lines.append("  From (F1): Proh(odrl:read, d).")
    lines.append("")
    lines.append("Conclusion:")
    lines.append("  Since the overlap equals the whole permission window and both rules are unconditional")
    lines.append("  beyond their date constraints, the clash holds for every day in 2025 ⇒ Global state: Conflict.")
    return "\n".join(lines)

# --- Main ---------------------------------------------------------------------

def main() -> None:
    result_uri, inter = classify(PERMISSION_2025, PROHIB_BEFORE_2026)

    # Always print the expected URI first
    print(result_uri)

    # Then print the goal-oriented proof explanation
    print()
    if inter is not None:
        print(build_goal_oriented_proof(PERMISSION_2025, PROHIB_BEFORE_2026, inter))
    else:
        print("--- Goal-Oriented Proof Explanation ---\n")
        print("No overlap found; this would contradict the testcase specification.")

if __name__ == "__main__":
    main()

