from datetime import datetime

# ---------------------------------
# 🏥 Sample ODRL-like Clinical Policy with Break-Glass
# ---------------------------------
policy = {
    "target": {
        "patient_id": "patient123",
        "resource_type": "Observation",
        "category": "laboratory"
    },
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
                    "value": {
                        "start": "2025-07-01T00:00:00",
                        "end":   "2025-08-15T23:59:59"
                    }
                }
            ]
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
        "window": {  # period during which break-glass may be used
            "start": "2025-07-01T00:00:00",
            "end":   "2025-08-31T23:59:59"
        },
        # Post-hoc obligations (do not block immediate access)
        "obligations": [
            {"action": "notify_privacy_officer"},
            {"action": "post_incident_review_within", "value": "PT24H"}  # ISO 8601 duration
        ]
    }
}

# ✅ Simulated registries/logs
consent_registry = {
    "patient123": "revoked"   # set to "revoked" to demonstrate break-glass bypass
}

audit_log = {
    ("clinician789", "patient123", "audit"): True
}

# Track emergency obligations (what has been done so far)
break_glass_log = {
    # Example: ("clinician789", "patient123", "notify_privacy_officer"): True/False
    # We'll start empty to show "pending obligations" in the trace.
}

# ------------------------------------------------
# 🧠 Clinical Policy Checker with Break-Glass
# ------------------------------------------------
def check_clinical_policy(
    user,
    action,
    patient_id,
    resource_type,
    category,
    purpose_of_use,
    current_time,
    break_glass=False,
    justification: str | None = None
):
    reasoning = []

    # Step 1: Target applicability
    t = policy["target"]
    if not (
        t["patient_id"] == patient_id
        and t["resource_type"] == resource_type
        and t["category"] == category
    ):
        reasoning.append(
            f"Policy does not apply to patient='{patient_id}', resource='{resource_type}', category='{category}'"
        )
        return False, reasoning

    reasoning.append(
        f"Evaluating policy for patient='{patient_id}', resource='{resource_type}', category='{category}'"
    )

    # Step 2: Break-glass path (checked first; it can bypass prohibitions/consent for view only)
    if break_glass:
        bg = policy.get("break_glass", {})
        if not bg.get("enabled", False):
            reasoning.append("Break-glass requested but not enabled in policy.")
            return False, reasoning

        if action != "view":
            reasoning.append("Break-glass only permits 'view' access; other actions remain restricted.")
            return False, reasoning

        if purpose_of_use != "EMERGENCY":
            reasoning.append("Break-glass requires purpose_of_use == 'EMERGENCY'.")
            return False, reasoning

        if not justification or not justification.strip():
            reasoning.append("Break-glass requires a non-empty justification.")
            return False, reasoning

        # Emergency window check
        start = datetime.fromisoformat(bg["window"]["start"])
        end = datetime.fromisoformat(bg["window"]["end"])
        if not (start <= current_time <= end):
            reasoning.append(
                f"Break-glass window failed: {current_time} not within [{start}, {end}]"
            )
            return False, reasoning

        reasoning.append("Break-glass conditions met: EMERGENCY + justification present + within window.")
        reasoning.append("Bypassing consent and prohibitions for 'view' (minimum necessary applies).")

        # Record/post-hoc obligations (do not block access)
        pending = []
        for ob in bg.get("obligations", []):
            k = (user, patient_id, ob["action"])
            if break_glass_log.get(k):
                reasoning.append(f"Break-glass obligation satisfied: {ob['action']}.")
            else:
                pending.append(ob["action"])

        if pending:
            reasoning.append(
                "Post-hoc obligations pending: " + ", ".join(pending)
            )

        # NOTE: we still require standard audit if configured
        for ob in policy.get("obligations", []):
            if ob["assignee"] == user and ob["action"] == "audit" and ob["target"] == patient_id:
                key = (user, patient_id, "audit")
                if audit_log.get(key):
                    reasoning.append("Standard obligation 'audit' has been fulfilled.")
                else:
                    reasoning.append("Standard obligation 'audit' not yet fulfilled (should be logged).")

        reasoning.append("All break-glass checks passed. Permission granted immediately.")
        return True, reasoning

    # Step 3: Non-emergency path — Prohibitions (deny overrides allow)
    for rule in policy.get("prohibitions", []):
        if rule["assignee"] == user and rule["action"] == action:
            reasoning.append(f"Action '{action}' is explicitly prohibited for user '{user}'")
            return False, reasoning

    # Step 4: Standard permissions and constraints
    for rule in policy.get("permissions", []):
        if rule["assignee"] == user and rule["action"] == action:
            reasoning.append(f"Permission found for action '{action}' by user '{user}'")

            for c in rule.get("constraints", []):
                ctype, op, val = c["type"], c["operator"], c["value"]

                if ctype == "purposeOfUse" and op == "in":
                    if purpose_of_use not in val:
                        reasoning.append(f"Purpose constraint failed: '{purpose_of_use}' not in {val}")
                        return False, reasoning
                    reasoning.append(f"Purpose constraint passed: '{purpose_of_use}' is allowed")

                elif ctype == "consent" and op == "eq":
                    status = consent_registry.get(patient_id, "unknown")
                    if status != val:
                        reasoning.append(f"Consent constraint failed: consent is '{status}', required '{val}'")
                        return False, reasoning
                    reasoning.append(f"Consent constraint passed: '{status}' == '{val}'")

                elif ctype == "encounterWindow" and op == "within":
                    start = datetime.fromisoformat(val["start"])
                    end = datetime.fromisoformat(val["end"])
                    if not (start <= current_time <= end):
                        reasoning.append(f"Encounter window failed: {current_time} not within [{start}, {end}]")
                        return False, reasoning
                    reasoning.append(f"Encounter window passed: {current_time} within [{start}, {end}]")

            # Standard obligations
            for ob in policy.get("obligations", []):
                if ob["assignee"] == user and ob["action"] == "audit" and ob["target"] == patient_id:
                    key = (user, patient_id, "audit")
                    if audit_log.get(key):
                        reasoning.append("Obligation 'audit' has been fulfilled.")
                    else:
                        reasoning.append("Obligation 'audit' NOT fulfilled. Denying access.")
                        return False, reasoning

            reasoning.append("All checks passed. Permission granted.")
            return True, reasoning

    # Step 5: No matching permission
    reasoning.append(f"No matching permission for action '{action}' and user '{user}'")
    return False, reasoning


# ------------------------------------------
# 🔍 Example scenarios (July 28, 2025)
# ------------------------------------------
now = datetime(2025, 7, 28)

# A) Standard path DENIED (consent revoked)
decision_a, trace_a = check_clinical_policy(
    user="clinician789",
    action="view",
    patient_id="patient123",
    resource_type="Observation",
    category="laboratory",
    purpose_of_use="TREATMENT",
    current_time=now
)

# B) Break-glass path ALLOWED (bypasses consent & prohibitions for 'view')
decision_b, trace_b = check_clinical_policy(
    user="clinician789",
    action="view",
    patient_id="patient123",
    resource_type="Observation",
    category="laboratory",
    purpose_of_use="EMERGENCY",
    current_time=now,
    break_glass=True,
    justification="Patient unresponsive; immediate access required to prior potassium levels."
)

# C) Break-glass for 'export' DENIED (override does not apply to export)
decision_c, trace_c = check_clinical_policy(
    user="clinician789",
    action="export",
    patient_id="patient123",
    resource_type="Observation",
    category="laboratory",
    purpose_of_use="EMERGENCY",
    current_time=now,
    break_glass=True,
    justification="Emergency, but export not necessary."
)

# ------------------------------------------
# 🖨 Output
# ------------------------------------------
def print_result(label, decision, trace):
    print(f"\n=== {label} ===")
    print("DECISION:", "ALLOW" if decision else "DENY")
    print("REASONING TRACE:")
    for step in trace:
        print("-", step)

print_result("A) Standard path (consent revoked)", decision_a, trace_a)
print_result("B) Break-glass path (view with justification)", decision_b, trace_b)
print_result("C) Break-glass export attempt (denied)", decision_c, trace_c)

