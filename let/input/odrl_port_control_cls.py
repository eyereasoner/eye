# Port-control policy (formal, dataclasses) that EMITS **Python code**
from __future__ import annotations
from dataclasses import dataclass, asdict, field
from typing import Any, Dict, List, Optional
from datetime import datetime

# -----------------------------------------
# ⚓ Sample ODRL-like Port Control Policy
# -----------------------------------------
policy = {
    "target": {"vessel_imo": "IMO1234567", "operation_area": "Berth-5", "cargo_type": "container"},
    "permissions": [
        {
            "assignee": "terminal_op_42",
            "action": "berth",
            "constraints": [
                {"type": "etaWindow", "operator": "within", "value": {"start": "2025-07-28T06:00:00", "end": "2025-07-28T12:00:00"}},
                {"type": "customsStatus", "operator": "eq", "value": "cleared"},
                {"type": "psciStatus", "operator": "eq", "value": "passed"},
                {"type": "riskScore", "operator": "lte", "value": 70}
            ]
        },
        {
            "assignee": "stevedore_alpha",
            "action": "load",
            "constraints": [
                {"type": "hazmatImpliesPermit", "operator": "implies", "value": "hazmatPermit"},
                {"type": "berthSlotActive", "operator": "eq", "value": True}
            ]
        }
    ],
    "prohibitions": [
        {"assignee": "terminal_op_42", "action": "sail"},
        {"assignee": "*", "action": "load", "constraints": [{"type": "windGust", "operator": "gt", "value": 20.0}]}
    ],
    "obligations": [
        {"assignee": "terminal_op_42", "action": "notify", "target": "harbor_master"},
        {"assignee": "terminal_op_42", "action": "ais_update", "target": "IMO1234567"}
    ],
    "break_glass": {
        "enabled": True,
        "allowed_actions": ["berth", "departEmergency"],
        "window": {"start": "2025-07-28T00:00:00", "end": "2025-08-15T23:59:59"},
        "requires_purpose": "PORT_EMERGENCY",
        "obligations": [
            {"action": "notify_port_security"},
            {"action": "file_incident_report_within", "value": "PT12H"}
        ]
    }
}

# -----------------------------------------
# 🧾 Simulated registries/state
# -----------------------------------------
customs_registry = {"IMO1234567": "cleared"}   # "cleared" | "pending"
psci_registry    = {"IMO1234567": "passed"}    # "passed" | "failed" | "pending"
risk_registry    = {"IMO1234567": 55}          # 0..100
manifest_registry = {"IMO1234567": True}
permits = {"hazmatPermit": True}
berth_slot = {"active": True}

# Obligations (standard) already fulfilled
obligation_log = {
    ("terminal_op_42", "harbor_master", "notify"): True,
    ("terminal_op_42", "IMO1234567", "ais_update"): True
}

# Break-glass obligations tracking (post-hoc)
break_glass_log = {
    # e.g., ("user", "IMO", "notify_port_security"): True/False
}

# -----------------------------------------
# 📦 Structured results (dataclasses)
# -----------------------------------------
@dataclass
class Step:
    kind: str            # "scope" | "prohibition" | "permission" | "constraint" | "obligation" | "break_glass"
    name: str            # identifier for the check
    status: str          # "passed" | "failed" | "skipped"
    data: Dict[str, Any] = field(default_factory=dict)

@dataclass
class Decision:
    allow: bool
    code: str            # "ALLOW" | "DENY_SCOPE" | "DENY_PROHIBITION" | "DENY_CONSTRAINT" | "DENY_OBLIGATION" | "DENY_NO_PERMISSION" | "DENY_BREAK_GLASS"
    action: str
    subject: str         # user
    target: Dict[str, Any]
    break_glass_used: bool
    steps: List[Step] = field(default_factory=list)
    pending_obligations: List[str] = field(default_factory=list)
    satisfied_obligations: List[str] = field(default_factory=list)
    timestamp: str = field(default_factory=lambda: datetime.utcnow().isoformat())

def _event(steps: List[Step], kind: str, name: str, outcome: str, **data):
    steps.append(Step(kind=kind, name=name, status=outcome, data=data))

# -----------------------------------------
# 🧠 Checker returning a Decision object
# -----------------------------------------
def check_port_policy(
    user: str,
    action: str,
    vessel_imo: str,
    operation_area: str,
    cargo_type: str,
    purpose: str,
    current_time: datetime,
    env: Dict[str, Any],
    cargo_info: Optional[Dict[str, Any]] = None,
    break_glass: bool = False,
    justification: Optional[str] = None
) -> Decision:

    steps: List[Step] = []
    t = policy["target"]
    # Scope
    in_scope = (t["vessel_imo"] == vessel_imo and t["operation_area"] == operation_area and t["cargo_type"] == cargo_type)
    _event(steps, "scope", "target_match", "passed" if in_scope else "failed",
           expected=t, received={"vessel_imo": vessel_imo, "operation_area": operation_area, "cargo_type": cargo_type})
    if not in_scope:
        return Decision(False, "DENY_SCOPE", action, user, t, False, steps)

    # Break-glass (checked first)
    if break_glass:
        bg = policy.get("break_glass", {})
        enabled = bg.get("enabled", False)
        allowed_action = action in bg.get("allowed_actions", [])
        correct_purpose = purpose == bg.get("requires_purpose")
        has_justif = bool(justification and justification.strip())
        start = datetime.fromisoformat(bg["window"]["start"])
        end   = datetime.fromisoformat(bg["window"]["end"])
        within = (start <= current_time <= end)

        _event(steps, "break_glass", "enabled", "passed" if enabled else "failed", enabled=enabled)
        _event(steps, "break_glass", "allowed_action", "passed" if allowed_action else "failed", action=action)
        _event(steps, "break_glass", "purpose", "passed" if correct_purpose else "failed", purpose=purpose)
        _event(steps, "break_glass", "justification", "passed" if has_justif else "failed", justification=justification)
        _event(steps, "break_glass", "window", "passed" if within else "failed",
               start=start.isoformat(), end=end.isoformat(), now=current_time.isoformat())

        if enabled and allowed_action and correct_purpose and has_justif and within:
            pending, satisfied = [], []
            for ob in bg.get("obligations", []):
                k = (user, vessel_imo, ob["action"])
                if break_glass_log.get(k):
                    satisfied.append(ob["action"])
                    _event(steps, "obligation", ob["action"], "passed")
                else:
                    pending.append(ob["action"])
                    _event(steps, "obligation", ob["action"], "skipped", note="post_hoc_pending")

            # Standard (non-blocking here) obligations visibility
            for ob in policy.get("obligations", []):
                if ob["assignee"] == user:
                    k = (user, ob["target"], ob["action"])
                    if obligation_log.get(k):
                        satisfied.append(ob["action"])
                        _event(steps, "obligation", ob["action"], "passed", target=ob["target"])
                    else:
                        pending.append(ob["action"])
                        _event(steps, "obligation", ob["action"], "skipped", target=ob["target"], note="should_be_logged")

            return Decision(True, "ALLOW", action, user, t, True, steps, pending, satisfied)

        return Decision(False, "DENY_BREAK_GLASS", action, user, t, True, steps)

    # Prohibitions
    for rule in policy.get("prohibitions", []):
        assignee_match = (rule["assignee"] == user or rule["assignee"] == "*")
        if assignee_match and rule["action"] == action:
            conds = rule.get("constraints", [])
            if not conds:
                _event(steps, "prohibition", rule["action"], "failed", reason="prohibited_unconditionally")
                return Decision(False, "DENY_PROHIBITION", action, user, t, False, steps)
            cond_hit = True
            for c in conds:
                if c["type"] == "windGust" and c["operator"] == "gt":
                    gust = float(env.get("wind_gust_m_s", 0.0))
                    hit = gust > float(c["value"])
                    _event(steps, "prohibition", "wind_gust", "passed" if hit else "skipped", gust=gust, threshold=c["value"])
                    if not hit:
                        cond_hit = False
            if cond_hit:
                _event(steps, "prohibition", rule["action"], "failed", reason="conditional_prohibition_triggered")
                return Decision(False, "DENY_PROHIBITION", action, user, t, False, steps)

    # Permissions & constraints
    matched_permission = False
    for rule in policy.get("permissions", []):
        if rule["assignee"] == user and rule["action"] == action:
            matched_permission = True
            _event(steps, "permission", f"{user}:{action}", "passed")

            for c in rule.get("constraints", []):
                tpe, op, val = c["type"], c["operator"], c["value"]

                if tpe == "etaWindow" and op == "within":
                    start = datetime.fromisoformat(val["start"]); end = datetime.fromisoformat(val["end"])
                    ok = (start <= current_time <= end)
                    _event(steps, "constraint", "etaWindow", "passed" if ok else "failed",
                           start=start.isoformat(), end=end.isoformat(), now=current_time.isoformat())
                    if not ok: return Decision(False, "DENY_CONSTRAINT", action, user, t, False, steps)

                elif tpe == "customsStatus" and op == "eq":
                    status = customs_registry.get(vessel_imo, "pending")
                    ok = (status == val)
                    _event(steps, "constraint", "customsStatus", "passed" if ok else "failed",
                           status=status, required=val)
                    if not ok: return Decision(False, "DENY_CONSTRAINT", action, user, t, False, steps)

                elif tpe == "psciStatus" and op == "eq":
                    status = psci_registry.get(vessel_imo, "pending")
                    ok = (status == val)
                    _event(steps, "constraint", "psciStatus", "passed" if ok else "failed",
                           status=status, required=val)
                    if not ok: return Decision(False, "DENY_CONSTRAINT", action, user, t, False, steps)

                elif tpe == "riskScore" and op == "lte":
                    score = risk_registry.get(vessel_imo, 100)
                    ok = (score <= val)
                    _event(steps, "constraint", "riskScore", "passed" if ok else "failed",
                           score=score, max_allowed=val)
                    if not ok: return Decision(False, "DENY_CONSTRAINT", action, user, t, False, steps)

                elif tpe == "manifestReceived" and op == "eq":
                    ok = bool(manifest_registry.get(vessel_imo, False)) == bool(val)
                    _event(steps, "constraint", "manifestReceived", "passed" if ok else "failed",
                           received=bool(manifest_registry.get(vessel_imo, False)), required=bool(val))
                    if not ok: return Decision(False, "DENY_CONSTRAINT", action, user, t, False, steps)

                elif tpe == "hazmatImpliesPermit" and op == "implies":
                    has_hazmat = bool(cargo_info and cargo_info.get("hazmat_units", 0) > 0)
                    ok = (not has_hazmat) or bool(permits.get(val, False))
                    _event(steps, "constraint", "hazmatImpliesPermit", "passed" if ok else "failed",
                           hazmat=has_hazmat, permit_required=val, permit_present=bool(permits.get(val, False)))
                    if not ok: return Decision(False, "DENY_CONSTRAINT", action, user, t, False, steps)

                elif tpe == "berthSlotActive" and op == "eq":
                    ok = (bool(berth_slot.get("active", False)) == bool(val))
                    _event(steps, "constraint", "berthSlotActive", "passed" if ok else "failed",
                           active=bool(berth_slot.get("active", False)))
                    if not ok: return Decision(False, "DENY_CONSTRAINT", action, user, t, False, steps)

            # Obligations (blocking in standard path)
            pending, satisfied = [], []
            for ob in policy.get("obligations", []):
                if ob["assignee"] == user:
                    k = (user, ob["target"], ob["action"])
                    if obligation_log.get(k):
                        satisfied.append(ob["action"])
                        _event(steps, "obligation", ob["action"], "passed", target=ob["target"])
                    else:
                        pending.append(ob["action"])
                        _event(steps, "obligation", ob["action"], "failed", target=ob["target"])
                        return Decision(False, "DENY_OBLIGATION", action, user, t, False, steps, pending, satisfied)

            return Decision(True, "ALLOW", action, user, t, False, steps, pending, satisfied)

    if not matched_permission:
        _event(steps, "permission", "match", "failed", reason="no_matching_permission")
        return Decision(False, "DENY_NO_PERMISSION", action, user, t, False, steps)

# -----------------------------------------
# 🖨 Emit **valid Python code** for decisions
# -----------------------------------------

def to_python(obj: Any, indent: int = 0) -> str:
    IND  = " " * indent
    IND2 = " " * (indent + 4)

    # Dataclasses
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
            f"{IND})"
        ]
        return "\n".join(lines)

    if isinstance(obj, Step):
        lines = [
            f"{IND}Step(",
            f"{IND2}kind={to_python(obj.kind, indent+4)},",
            f"{IND2}name={to_python(obj.name, indent+4)},",
            f"{IND2}status={to_python(obj.status, indent+4)},",
            f"{IND2}data={to_python(obj.data, indent+4)}",
            f"{IND})"
        ]
        return "\n".join(lines)

    # Built-ins
    if isinstance(obj, dict):
        if not obj:
            return "{}"
        lines = [f"{IND}{{"]
        for k, v in obj.items():
            lines.append(f"{IND2}{to_python(k)}: {to_python(v, indent+4)},")
        lines.append(f"{IND}}}")
        return "\n".join(lines)

    if isinstance(obj, list):
        if not obj:
            return "[]"
        lines = [f"{IND}["]
        for v in obj:
            lines.append(f"{IND2}{to_python(v, indent+4)},")
        lines.append(f"{IND}]")
        return "\n".join(lines)

    if isinstance(obj, tuple):
        if not obj:
            return "()"
        lines = [f"{IND}("]
        for v in obj:
            lines.append(f"{IND2}{to_python(v, indent+4)},")
        lines.append(f"{IND})")
        return "\n".join(lines)

    if isinstance(obj, datetime):
        return f"datetime.fromisoformat({repr(obj.isoformat())})"

    # Scalars
    return repr(obj)

def emit_py(name: str, obj: Any) -> None:
    print(f"{name} = {to_python(obj)}\n")

# -----------------------------------------
# 🔍 Example scenarios (Europe/Brussels, July 28, 2025)
# -----------------------------------------
now = datetime(2025, 7, 28, 8, 30)
env_ok = {"wind_gust_m_s": 12.0}
env_windy = {"wind_gust_m_s": 25.0}

# A) Standard ALLOW
res_a = check_port_policy(
    user="terminal_op_42",
    action="berth",
    vessel_imo="IMO1234567",
    operation_area="Berth-5",
    cargo_type="container",
    purpose="OPERATIONS",
    current_time=now,
    env=env_ok,
    cargo_info={"hazmat_units": 0}
)

# B) Wind prohibition DENY
res_b = check_port_policy(
    user="stevedore_alpha",
    action="load",
    vessel_imo="IMO1234567",
    operation_area="Berth-5",
    cargo_type="container",
    purpose="OPERATIONS",
    current_time=now,
    env=env_windy,
    cargo_info={"hazmat_units": 0}
)

# C) Break-glass ALLOW (after hardening state)
customs_registry["IMO1234567"] = "pending"
psci_registry["IMO1234567"] = "failed"
risk_registry["IMO1234567"] = 85

res_c = check_port_policy(
    user="terminal_op_42",
    action="berth",
    vessel_imo="IMO1234567",
    operation_area="Berth-5",
    cargo_type="container",
    purpose="PORT_EMERGENCY",
    current_time=now,
    env=env_ok,
    cargo_info={"hazmat_units": 0},
    break_glass=True,
    justification="Fire on adjacent berth; immediate relocation required."
)

# 👉 Emits **Python code** you can paste/exec to recreate the Decision objects
emit_py("decision_a", res_a)
emit_py("decision_b", res_b)
emit_py("decision_c", res_c)

