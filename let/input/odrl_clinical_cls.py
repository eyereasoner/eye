#!/usr/bin/env python3
"""
Clinical policy checker with Break‑Glass that EMITS **valid Python code** (not prose).

- Returns structured dataclasses (Decision, Step) instead of strings.
- Pretty-prints results as executable Python assignments you can copy/paste or exec.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional
from datetime import datetime

# ---------------------------------
# 🏥 Sample ODRL-like Clinical Policy with Break-Glass
# ---------------------------------
policy = {
    "target": {"patient_id": "patient123", "resource_type": "Observation", "category": "laboratory"},
    "permissions": [
        {
            "assignee": "clinician789",
            "action": "view",
            "constraints": [
                {"type": "purposeOfUse", "operator": "in", "value": ["TREATMENT"]},
                {"type": "consent", "operator": "eq", "value": "granted"},
                {
                    "type": "encounterWindow",
                    "operator": "within",
                    "value": {"start": "2025-07-01T00:00:00", "end": "2025-08-15T23:59:59"},
                },
            ],
        }
    ],
    "prohibitions": [
        {"assignee": "clinician789", "action": "export"}  # block PHI export
    ],
    "obligations": [
        {"assignee": "clinician789", "action": "audit", "target": "patient123"}
    ],
    "break_glass": {
        "enabled": True,
        "window": {"start": "2025-07-01T00:00:00", "end": "2025-08-31T23:59:59"},
        # Post-hoc obligations (do not block immediate access)
        "obligations": [
            {"action": "notify_privacy_officer"},
            {"action": "post_incident_review_within", "value": "PT24H"},
        ],
    },
}

# ✅ Simulated registries/logs
consent_registry = {"patient123": "revoked"}  # change to "granted" to see standard allow
audit_log = {("clinician789", "patient123", "audit"): True}

# Track emergency obligations (what has been done so far)
break_glass_log = {
    # Example: ("clinician789", "patient123", "notify_privacy_officer"): True/False
}

# -----------------------------------------
# 📦 Structured results (dataclasses)
# -----------------------------------------
@dataclass(frozen=True)
class Step:
    kind: str           # "scope" | "break_glass" | "prohibition" | "permission" | "constraint" | "obligation"
    name: str
    status: str         # "passed" | "failed" | "skipped"
    data: Dict[str, Any] = field(default_factory=dict)

@dataclass(frozen=True)
class Decision:
    allow: bool
    code: str           # "ALLOW" | "DENY_SCOPE" | "DENY_PROHIBITION" | "DENY_CONSTRAINT" | "DENY_OBLIGATION" | "DENY_NO_PERMISSION" | "DENY_BREAK_GLASS"
    action: str
    subject: str
    target: Dict[str, Any]
    break_glass_used: bool
    steps: List[Step] = field(default_factory=list)
    pending_obligations: List[str] = field(default_factory=list)
    satisfied_obligations: List[str] = field(default_factory=list)
    timestamp: str = field(default_factory=lambda: datetime.utcnow().isoformat(timespec="seconds"))

def _event(steps: List[Step], kind: str, name: str, outcome: str, **data) -> None:
    steps.append(Step(kind=kind, name=name, status=outcome, data=data))

# ------------------------------------------------
# 🧠 Clinical Policy Checker with Break-Glass (structured)
# ------------------------------------------------
def check_clinical_policy(
    user: str,
    action: str,
    patient_id: str,
    resource_type: str,
    category: str,
    purpose_of_use: str,
    current_time: datetime,
    break_glass: bool = False,
    justification: Optional[str] = None,
) -> Decision:

    steps: List[Step] = []
    t = policy["target"]

    # Step 1: Target applicability
    in_scope = (t["patient_id"] == patient_id and t["resource_type"] == resource_type and t["category"] == category)
    _event(steps, "scope", "target_match", "passed" if in_scope else "failed",
           expected=t, received={"patient_id": patient_id, "resource_type": resource_type, "category": category})
    if not in_scope:
        return Decision(False, "DENY_SCOPE", action, user, t, False, steps)

    # Step 2: Break-glass path (checked first; can bypass prohibitions/consent for 'view' only)
    if break_glass:
        bg = policy.get("break_glass", {})
        enabled = bool(bg.get("enabled", False))
        allowed_action = (action == "view")
        correct_purpose = (purpose_of_use == "EMERGENCY")
        has_justif = bool(justification and justification.strip())
        start = datetime.fromisoformat(bg["window"]["start"])
        end   = datetime.fromisoformat(bg["window"]["end"])
        within = (start <= current_time <= end)

        _event(steps, "break_glass", "enabled", "passed" if enabled else "failed", enabled=enabled)
        _event(steps, "break_glass", "allowed_action", "passed" if allowed_action else "failed", action=action)
        _event(steps, "break_glass", "purpose", "passed" if correct_purpose else "failed", purpose=purpose_of_use)
        _event(steps, "break_glass", "justification", "passed" if has_justif else "failed", justification=justification)
        _event(steps, "break_glass", "window", "passed" if within else "failed",
               start=start.isoformat(), end=end.isoformat(), now=current_time.isoformat())

        if enabled and allowed_action and correct_purpose and has_justif and within:
            pending: List[str] = []
            satisfied: List[str] = []

            # Post-hoc BG obligations (visibility only)
            for ob in bg.get("obligations", []):
                k = (user, patient_id, ob["action"])
                if break_glass_log.get(k):
                    satisfied.append(ob["action"])
                    _event(steps, "obligation", ob["action"], "passed")
                else:
                    pending.append(ob["action"])
                    _event(steps, "obligation", ob["action"], "skipped", note="post_hoc_pending")

            # Standard audit (non-blocking in BG path; surfaced for ops)
            for ob in policy.get("obligations", []):
                if ob["assignee"] == user and ob["action"] == "audit" and ob["target"] == patient_id:
                    key = (user, patient_id, "audit")
                    if audit_log.get(key):
                        satisfied.append("audit")
                        _event(steps, "obligation", "audit", "passed", target=patient_id)
                    else:
                        pending.append("audit")
                        _event(steps, "obligation", "audit", "skipped", target=patient_id, note="should_be_logged")

            return Decision(True, "ALLOW", action, user, t, True, steps, pending, satisfied)

        return Decision(False, "DENY_BREAK_GLASS", action, user, t, True, steps)

    # Step 3: Non-emergency path — Prohibitions (deny overrides allow)
    for rule in policy.get("prohibitions", []):
        if rule["assignee"] == user and rule["action"] == action:
            _event(steps, "prohibition", f"{user}:{action}", "failed", reason="explicit_prohibition")
            return Decision(False, "DENY_PROHIBITION", action, user, t, False, steps)

    # Step 4: Standard permissions and constraints
    matched_permission = False
    for rule in policy.get("permissions", []):
        if rule["assignee"] == user and rule["action"] == action:
            matched_permission = True
            _event(steps, "permission", f"{user}:{action}", "passed")

            for c in rule.get("constraints", []):
                ctype, op, val = c["type"], c["operator"], c["value"]

                if ctype == "purposeOfUse" and op == "in":
                    ok = (purpose_of_use in val)
                    _event(steps, "constraint", "purposeOfUse", "passed" if ok else "failed",
                           received=purpose_of_use, allowed=val)
                    if not ok: return Decision(False, "DENY_CONSTRAINT", action, user, t, False, steps)

                elif ctype == "consent" and op == "eq":
                    status = consent_registry.get(patient_id, "unknown")
                    ok = (status == val)
                    _event(steps, "constraint", "consent", "passed" if ok else "failed",
                           observed=status, required=val)
                    if not ok: return Decision(False, "DENY_CONSTRAINT", action, user, t, False, steps)

                elif ctype == "encounterWindow" and op == "within":
                    start = datetime.fromisoformat(val["start"])
                    end   = datetime.fromisoformat(val["end"])
                    ok = (start <= current_time <= end)
                    _event(steps, "constraint", "encounterWindow", "passed" if ok else "failed",
                           start=start.isoformat(), end=end.isoformat(), now=current_time.isoformat())
                    if not ok: return Decision(False, "DENY_CONSTRAINT", action, user, t, False, steps)

            # Standard obligations (blocking here)
            pending: List[str] = []
            satisfied: List[str] = []
            for ob in policy.get("obligations", []):
                if ob["assignee"] == user and ob["action"] == "audit" and ob["target"] == patient_id:
                    key = (user, patient_id, "audit")
                    if audit_log.get(key):
                        satisfied.append("audit")
                        _event(steps, "obligation", "audit", "passed", target=patient_id)
                    else:
                        pending.append("audit")
                        _event(steps, "obligation", "audit", "failed", target=patient_id)
                        return Decision(False, "DENY_OBLIGATION", action, user, t, False, steps, pending, satisfied)

            return Decision(True, "ALLOW", action, user, t, False, steps, pending, satisfied)

    if not matched_permission:
        _event(steps, "permission", "match", "failed", reason="no_matching_permission")
        return Decision(False, "DENY_NO_PERMISSION", action, user, t, False, steps)

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
            f"{IND2}break_glass_used={to_python(obj.break_glass_used, indent+4)},",
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

# ------------------------------------------
# 🔍 Example scenarios (July 28, 2025)
# ------------------------------------------
if __name__ == "__main__":
    now = datetime(2025, 7, 28)

    # A) Standard path DENIED (consent revoked)
    decision_a = check_clinical_policy(
        user="clinician789",
        action="view",
        patient_id="patient123",
        resource_type="Observation",
        category="laboratory",
        purpose_of_use="TREATMENT",
        current_time=now,
    )

    # B) Break-glass path ALLOWED (bypasses consent & prohibitions for 'view')
    decision_b = check_clinical_policy(
        user="clinician789",
        action="view",
        patient_id="patient123",
        resource_type="Observation",
        category="laboratory",
        purpose_of_use="EMERGENCY",
        current_time=now,
        break_glass=True,
        justification="Patient unresponsive; immediate access required to prior potassium levels.",
    )

    # C) Break-glass for 'export' DENIED (override does not apply to export)
    decision_c = check_clinical_policy(
        user="clinician789",
        action="export",
        patient_id="patient123",
        resource_type="Observation",
        category="laboratory",
        purpose_of_use="EMERGENCY",
        current_time=now,
        break_glass=True,
        justification="Emergency, but export not necessary.",
    )

    # 👉 Emit **Python code** (pretty printed) to recreate the Decision objects
    emit_py("decision_a", decision_a)
    emit_py("decision_b", decision_b)
    emit_py("decision_c", decision_c)

