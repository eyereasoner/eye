#!/usr/bin/env python3
"""
ODRL-like policy checker that EMITS **valid Python code** (not prose).

- Returns structured dataclasses (Decision, Step) instead of strings.
- Pretty-prints results as executable Python assignments (via emit_py).
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Dict, List
from datetime import datetime

# --------------------------
# 📝 Sample ODRL-like policy
# --------------------------
policy = {
    "target": "document123",
    "permissions": [
        {
            "assignee": "user123",
            "action": "read",
            "constraints": [
                {"type": "dateTime", "operator": "gteq", "value": "2025-07-01T00:00:00"}
            ],
        }
    ],
    "prohibitions": [
        {"assignee": "user123", "action": "delete"}
    ],
    "obligations": [
        {"assignee": "user123", "action": "notify", "target": "document123"}
    ],
}

# ✅ Simulated obligation log (fulfilled duties)
obligation_log = {
    ("user123", "document123", "notify"): True  # means user123 sent a notification
}

# -----------------------------------------
# 📦 Structured results (dataclasses)
# -----------------------------------------
@dataclass(frozen=True)
class Step:
    kind: str           # "scope" | "prohibition" | "permission" | "constraint" | "obligation"
    name: str
    status: str         # "passed" | "failed" | "skipped"
    data: Dict[str, Any] = field(default_factory=dict)

@dataclass(frozen=True)
class Decision:
    allow: bool
    code: str           # "ALLOW" | "DENY_SCOPE" | "DENY_PROHIBITION" | "DENY_CONSTRAINT" | "DENY_OBLIGATION" | "DENY_NO_PERMISSION"
    action: str
    subject: str
    target: str
    steps: List[Step] = field(default_factory=list)
    pending_obligations: List[str] = field(default_factory=list)
    satisfied_obligations: List[str] = field(default_factory=list)
    timestamp: str = field(default_factory=lambda: datetime.utcnow().isoformat(timespec="seconds"))

def _event(steps: List[Step], kind: str, name: str, outcome: str, **data) -> None:
    steps.append(Step(kind=kind, name=name, status=outcome, data=data))

# ---------------------------------------
# 🧠 Policy Checker (structured)
# ---------------------------------------
def check_policy(user: str, action: str, target: str, current_time: datetime) -> Decision:
    steps: List[Step] = []

    # Step 1: Check if policy applies to the target
    in_scope = (policy["target"] == target)
    _event(steps, "scope", "target_match", "passed" if in_scope else "failed",
           expected=policy["target"], received=target)
    if not in_scope:
        return Decision(False, "DENY_SCOPE", action, user, policy["target"], steps)

    # Step 2: Check prohibitions (deny overrides allow)
    for rule in policy.get("prohibitions", []):
        if rule["assignee"] == user and rule["action"] == action:
            _event(steps, "prohibition", f"{user}:{action}", "failed", reason="explicit_prohibition")
            return Decision(False, "DENY_PROHIBITION", action, user, target, steps)

    # Step 3: Check permissions
    matched_permission = False
    for rule in policy.get("permissions", []):
        if rule["assignee"] == user and rule["action"] == action:
            matched_permission = True
            _event(steps, "permission", f"{user}:{action}", "passed")

            # Step 3a: Check constraints
            for constraint in rule.get("constraints", []):
                if constraint["type"] == "dateTime" and constraint["operator"] == "gteq":
                    constraint_time = datetime.fromisoformat(constraint["value"])
                    ok = (current_time >= constraint_time)
                    _event(
                        steps, "constraint", "dateTime_gteq",
                        "passed" if ok else "failed",
                        now=current_time.isoformat(),
                        required=constraint_time.isoformat(),
                    )
                    if not ok:
                        return Decision(False, "DENY_CONSTRAINT", action, user, target, steps)

            # Step 4: Check obligations (blocking)
            pending: List[str] = []
            satisfied: List[str] = []
            for obligation in policy.get("obligations", []):
                if obligation["assignee"] == user and obligation["action"] == "notify" and obligation["target"] == target:
                    key = (user, target, "notify")
                    if obligation_log.get(key):
                        satisfied.append("notify")
                        _event(steps, "obligation", "notify", "passed", target=target)
                    else:
                        pending.append("notify")
                        _event(steps, "obligation", "notify", "failed", target=target)
                        return Decision(False, "DENY_OBLIGATION", action, user, target, steps, pending, satisfied)

            return Decision(True, "ALLOW", action, user, target, steps, pending, satisfied)

    # Step 5: No matching permission
    if not matched_permission:
        _event(steps, "permission", "match", "failed", reason="no_matching_permission")
        return Decision(False, "DENY_NO_PERMISSION", action, user, target, steps)

# ------------------------------------------
# 🖨 Pretty-print emitter that outputs **valid Python code**
# ------------------------------------------
def to_python(obj: Any, indent: int = 0) -> str:
    IND  = " " * indent
    IND2 = " " * (indent + 4)

    if isinstance(obj, Decision):
        lines = [
            f"{IND}Decision(",
            f"{IND2}allow={to_python(obj.allow, indent+4)},",
            f"{IND2}code={to_python(obj.code, indent+4)},",
            f"{IND2}action={to_python(obj.action, indent+4)},",
            f"{IND2}subject={to_python(obj.subject, indent+4)},",
            f"{IND2}target={to_python(obj.target, indent+4)},",
            f"{IND2}steps={to_python(obj.steps, indent+4)},",
            f"{IND2}pending_obligations={to_python(obj.pending_obligations, indent+4)},",
            f"{IND2}satisfied_obligations={to_python(obj.satisfied_obligations, indent+4)},",
            f"{IND2}timestamp={to_python(obj.timestamp, indent+4)}",
            f"{IND})",
        ]
        return "\n".join(lines)

    if isinstance(obj, Step):
        lines = [
            f"{IND}Step(",
            f"{IND2}kind={to_python(obj.kind, indent+4)},",
            f"{IND2}name={to_python(obj.name, indent+4)},",
            f"{IND2}status={to_python(obj.status, indent+4)},",
            f"{IND2}data={to_python(obj.data, indent+4)}",
            f"{IND})",
        ]
        return "\n".join(lines)

    if isinstance(obj, dict):
        if not obj: return "{}"
        lines = [f"{IND}{{"]
        for k, v in obj.items():
            lines.append(f"{IND2}{to_python(k)}: {to_python(v, indent+4)},")
        lines.append(f"{IND}}}")
        return "\n".join(lines)

    if isinstance(obj, list):
        if not obj: return "[]"
        lines = [f"{IND}["]
        for v in obj:
            lines.append(f"{IND2}{to_python(v, indent+4)},")
        lines.append(f"{IND}]")
        return "\n".join(lines)

    if isinstance(obj, tuple):
        if not obj: return "()"
        lines = [f"{IND}("]
        for v in obj:
            lines.append(f"{IND2}{to_python(v, indent+4)},")
        lines.append(f"{IND})")
        return "\n".join(lines)

    if isinstance(obj, datetime):
        return f"datetime.fromisoformat({repr(obj.isoformat())})"

    if isinstance(obj, float):
        return repr(round(obj, 6))

    return repr(obj)

def emit_py(name: str, obj: Any) -> None:
    print(f"{name} = {to_python(obj)}\n")

# -------------------------------
# 🔍 Example query: user, action
# -------------------------------
if __name__ == "__main__":
    user = "user123"
    action = "read"
    target = "document123"
    now = datetime(2025, 7, 28)  # Change to test different times

    # Run the policy checker and emit **Python code**
    decision = check_policy(user, action, target, now)
    emit_py("decision", decision)

