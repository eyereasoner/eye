from datetime import datetime

# -----------------------------------------
# ⚓ Sample ODRL-like Port Control Policy
# -----------------------------------------
policy = {
    "target": {  # vessel visit scope
        "vessel_imo": "IMO1234567",
        "operation_area": "Berth-5",
        "cargo_type": "container"
    },
    "permissions": [
        {
            "assignee": "terminal_op_42",
            "action": "berth",
            "constraints": [
                {"type": "etaWindow", "operator": "within",
                 "value": {"start": "2025-07-28T06:00:00", "end": "2025-07-28T12:00:00"}},
                {"type": "customsStatus", "operator": "eq", "value": "cleared"},
                {"type": "psciStatus", "operator": "eq", "value": "passed"},
                {"type": "riskScore", "operator": "lte", "value": 70}
            ]
        },
        {
            "assignee": "customs_agent_7",
            "action": "inspect",
            "constraints": [
                {"type": "manifestReceived", "operator": "eq", "value": True}
            ]
        },
        {
            "assignee": "stevedore_alpha",
            "action": "load",
            "constraints": [
                # If any hazmat present, a hazmat permit must exist
                {"type": "hazmatImpliesPermit", "operator": "implies", "value": "hazmatPermit"},
                {"type": "berthSlotActive", "operator": "eq", "value": True}
            ]
        }
    ],
    "prohibitions": [
        # Terminal ops may not authorize sailing
        {"assignee": "terminal_op_42", "action": "sail"},
        # Weather safety: loading prohibited above wind threshold
        {"assignee": "*", "action": "load",
         "constraints": [{"type": "windGust", "operator": "gt", "value": 20.0}]}
    ],
    "obligations": [
        {"assignee": "terminal_op_42", "action": "notify", "target": "harbor_master"},
        {"assignee": "terminal_op_42", "action": "ais_update", "target": "IMO1234567"}
    ],
    "break_glass": {
        "enabled": True,
        "allowed_actions": ["berth", "departEmergency"],   # what can be overridden
        "window": {"start": "2025-07-28T00:00:00", "end": "2025-08-15T23:59:59"},
        "requires_purpose": "PORT_EMERGENCY",
        # Post-hoc obligations (do not block immediate access)
        "obligations": [
            {"action": "notify_port_security"},
            {"action": "file_incident_report_within", "value": "PT12H"}  # ISO 8601 duration
        ]
    }
}

# ✅ Simulated registries/state for the visit (IMO1234567)
customs_registry = {"IMO1234567": "cleared"}        # "cleared" | "pending"
psci_registry    = {"IMO1234567": "passed"}         # "passed" | "failed" | "pending"
risk_registry    = {"IMO1234567": 55}               # 0..100 (higher = riskier)
manifest_registry = {"IMO1234567": True}            # manifest delivered?
permits = {"hazmatPermit": True}                    # permit presence
berth_slot = {"active": True}                       # slot assigned/active?

# ✅ Obligations already fulfilled
obligation_log = {
    ("terminal_op_42", "harbor_master", "notify"): True,
    ("terminal_op_42", "IMO1234567", "ais_update"): True
}

# ✅ Break-glass post-hoc obligations tracking
break_glass_log = {
    # Example: ("user", "IMO", "notify_port_security"): True/False
}

# ---------------------------------------------------------
# 🧠 Port Policy Checker (with reasoning trace)
# ---------------------------------------------------------
def check_port_policy(
    user, action, vessel_imo, operation_area, cargo_type,
    purpose, current_time, env,
    cargo_info=None, break_glass=False, justification: str | None = None
):
    reasoning = []
    t = policy["target"]
    if not (
        t["vessel_imo"] == vessel_imo
        and t["operation_area"] == operation_area
        and t["cargo_type"] == cargo_type
    ):
        reasoning.append(f"Policy does not apply to IMO='{vessel_imo}', area='{operation_area}', cargo='{cargo_type}'")
        return False, reasoning
    reasoning.append(f"Evaluating policy for IMO='{vessel_imo}', area='{operation_area}', cargo='{cargo_type}'")

    # ---- Break-glass path (checked first; can bypass some checks) ----
    if break_glass:
        bg = policy.get("break_glass", {})
        if not bg.get("enabled", False):
            reasoning.append("Break-glass requested but not enabled.")
            return False, reasoning
        if action not in bg.get("allowed_actions", []):
            reasoning.append(f"Break-glass not allowed for action '{action}'.")
            return False, reasoning
        if purpose != bg.get("requires_purpose"):
            reasoning.append(f"Break-glass requires purpose '{bg.get('requires_purpose')}'.")
            return False, reasoning
        if not justification or not justification.strip():
            reasoning.append("Break-glass requires a non-empty justification.")
            return False, reasoning
        start = datetime.fromisoformat(bg["window"]["start"])
        end   = datetime.fromisoformat(bg["window"]["end"])
        if not (start <= current_time <= end):
            reasoning.append(f"Break-glass window failed: {current_time} not within [{start}, {end}]")
            return False, reasoning

        reasoning.append("Break-glass conditions met: emergency + justification + within window.")
        reasoning.append("Bypassing prohibitions and compliance constraints for this action (minimum safety still applies).")

        # Post-hoc break-glass obligations (do not block)
        pending = []
        for ob in bg.get("obligations", []):
            k = (user, vessel_imo, ob["action"])
            if break_glass_log.get(k):
                reasoning.append(f"Break-glass obligation satisfied: {ob['action']}.")
            else:
                pending.append(ob["action"])
        if pending:
            reasoning.append("Post-hoc obligations pending: " + ", ".join(pending))

        # Standard obligations (e.g., notify harbor master, AIS)
        for ob in policy.get("obligations", []):
            if ob["assignee"] == user:
                k = (user, ob["target"], ob["action"])
                if obligation_log.get(k):
                    reasoning.append(f"Standard obligation '{ob['action']}' has been fulfilled.")
                else:
                    reasoning.append(f"Standard obligation '{ob['action']}' not yet fulfilled (should be logged).")

        reasoning.append("All break-glass checks passed. Permission granted immediately.")
        return True, reasoning

    # ---- Prohibitions (deny can depend on environment) ----
    for rule in policy.get("prohibitions", []):
        assignee_match = rule["assignee"] == user or rule["assignee"] == "*"
        if assignee_match and rule["action"] == action:
            # Optional conditional prohibition
            conds = rule.get("constraints", [])
            cond_ok = True
            for c in conds:
                if c["type"] == "windGust" and c["operator"] == "gt":
                    if env.get("wind_gust_m_s", 0.0) <= c["value"]:
                        cond_ok = False
                    else:
                        reasoning.append(f"Prohibition condition met: wind_gust {env['wind_gust_m_s']} m/s > {c['value']} m/s")
            if cond_ok:
                reasoning.append(f"Action '{action}' is prohibited for user '{user}'.")
                return False, reasoning

    # ---- Permissions and constraints ----
    for rule in policy.get("permissions", []):
        if rule["assignee"] == user and rule["action"] == action:
            reasoning.append(f"Permission found for action '{action}' by '{user}'")
            for c in rule.get("constraints", []):
                tpe, op, val = c["type"], c["operator"], c["value"]

                if tpe == "etaWindow" and op == "within":
                    start = datetime.fromisoformat(val["start"])
                    end   = datetime.fromisoformat(val["end"])
                    if not (start <= current_time <= end):
                        reasoning.append(f"ETA window failed: {current_time} not within [{start}, {end}]")
                        return False, reasoning
                    reasoning.append(f"ETA window passed: {current_time} within [{start}, {end}]")

                elif tpe == "customsStatus" and op == "eq":
                    status = customs_registry.get(vessel_imo, "pending")
                    if status != val:
                        reasoning.append(f"Customs constraint failed: '{status}' != '{val}'")
                        return False, reasoning
                    reasoning.append(f"Customs constraint passed: '{status}' == '{val}'")

                elif tpe == "psciStatus" and op == "eq":
                    status = psci_registry.get(vessel_imo, "pending")
                    if status != val:
                        reasoning.append(f"PSC constraint failed: '{status}' != '{val}'")
                        return False, reasoning
                    reasoning.append(f"PSC constraint passed: '{status}' == '{val}'")

                elif tpe == "riskScore" and op == "lte":
                    score = risk_registry.get(vessel_imo, 100)
                    if score > val:
                        reasoning.append(f"Risk constraint failed: score {score} > {val}")
                        return False, reasoning
                    reasoning.append(f"Risk constraint passed: score {score} ≤ {val}")

                elif tpe == "manifestReceived" and op == "eq":
                    ok = bool(manifest_registry.get(vessel_imo, False))
                    if ok != val:
                        reasoning.append("Manifest constraint failed: not received.")
                        return False, reasoning
                    reasoning.append("Manifest constraint passed: received.")

                elif tpe == "hazmatImpliesPermit" and op == "implies":
                    has_hazmat = bool(cargo_info and cargo_info.get("hazmat_units", 0) > 0)
                    if has_hazmat and not permits.get(val, False):
                        reasoning.append("Hazmat constraint failed: permit missing.")
                        return False, reasoning
                    reasoning.append("Hazmat constraint passed.")

                elif tpe == "berthSlotActive" and op == "eq":
                    if berth_slot.get("active", False) != val:
                        reasoning.append("Berth-slot constraint failed: slot not active.")
                        return False, reasoning
                    reasoning.append("Berth-slot constraint passed: active.")

            # Obligations (must be fulfilled in standard path)
            for ob in policy.get("obligations", []):
                if ob["assignee"] == user:
                    k = (user, ob["target"], ob["action"])
                    if obligation_log.get(k):
                        reasoning.append(f"Obligation '{ob['action']}' has been fulfilled.")
                    else:
                        reasoning.append(f"Obligation '{ob['action']}' NOT fulfilled. Denying.")
                        return False, reasoning

            reasoning.append("All checks passed. Permission granted.")
            return True, reasoning

    reasoning.append(f"No matching permission for action '{action}' and user '{user}'")
    return False, reasoning


# -----------------------------------------
# 🔍 Example scenarios (July 28, 2025)
# -----------------------------------------
now = datetime(2025, 7, 28, 8, 30)

# Environment snapshot
env_ok = {"wind_gust_m_s": 12.0}
env_windy = {"wind_gust_m_s": 25.0}

# Cargo summary
cargo = {"hazmat_units": 0}

# A) Standard path ALLOW (berth within window, all clear)
decision_a, trace_a = check_port_policy(
    user="terminal_op_42",
    action="berth",
    vessel_imo="IMO1234567",
    operation_area="Berth-5",
    cargo_type="container",
    purpose="OPERATIONS",
    current_time=now,
    env=env_ok,
    cargo_info=cargo
)

# B) Wind prohibition DENY (loading in high gusts)
decision_b, trace_b = check_port_policy(
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

# C) Break-glass ALLOW (emergency berth despite pending compliance)
# Simulate tougher compliance state
customs_registry["IMO1234567"] = "pending"
psci_registry["IMO1234567"] = "failed"
risk_registry["IMO1234567"] = 85

decision_c, trace_c = check_port_policy(
    user="terminal_op_42",
    action="berth",
    vessel_imo="IMO1234567",
    operation_area="Berth-5",
    cargo_type="container",
    purpose="PORT_EMERGENCY",
    current_time=now,
    env=env_ok,
    cargo_info=cargo,
    break_glass=True,
    justification="Fire on adjacent berth; immediate relocation required."
)

# -----------------------------------------
# 🖨 Output
# -----------------------------------------
def print_result(label, decision, trace):
    print(f"\n=== {label} ===")
    print("DECISION:", "ALLOW" if decision else "DENY")
    print("REASONING TRACE:")
    for step in trace:
        print("-", step)

print_result("A) Standard berth", decision_a, trace_a)
print_result("B) Load denied by wind", decision_b, trace_b)
print_result("C) Break-glass berth", decision_c, trace_c)

