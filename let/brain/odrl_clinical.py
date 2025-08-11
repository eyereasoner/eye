#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
odrl_clinical.py — ODRL-style clinical access policy with BREAK-GLASS and ARC output

What this script does
---------------------
• Defines a small ODRL-like clinical policy for viewing a lab Observation.
• Supports a BREAK-GLASS path (emergency override) with a time window and post-hoc duties.
• Evaluates three demo scenarios (standard, break-glass view, break-glass export).
• Prints ONLY ARC sections:
    - Answer: per-scenario decision + compact checklist of key gates.
    - Reason why: evaluation order and what break-glass bypasses.
    - Check (harness): quick self-tests for consent, window, purpose, obligations, and precedence.

Run
---
    python odrl_clinical.py
"""

from datetime import datetime

# ---------------------------------
# Sample ODRL-like Clinical Policy
# ---------------------------------
policy = {
    "target": {
        "patient_id": "patient123",
        "resource_type": "Observation",
        "category": "laboratory",
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
                        "end": "2025-08-15T23:59:59",
                    },
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
        "window": {  # period during which break-glass may be used
            "start": "2025-07-01T00:00:00",
            "end": "2025-08-31T23:59:59",
        },
        # Post-hoc obligations (do not block immediate access)
        "obligations": [
            {"action": "notify_privacy_officer"},
            {"action": "post_incident_review_within", "value": "PT24H"},  # ISO 8601
        ],
    },
}

# ✅ Simulated registries/logs
consent_registry = {
    "patient123": "revoked"  # set to "granted" to allow standard non-emergency view
}
audit_log = {("clinician789", "patient123", "audit"): True}
# Track break-glass post-hoc obligations (what has been done so far)
break_glass_log = {
    # Example entries (initially empty):
    # ("clinician789","patient123","notify_privacy_officer"): True/False
    # ("clinician789","patient123","post_incident_review_within"): True/False
}

# ------------------------------------------------
# Clinical Policy Checker with Break-Glass (core)
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
    justification: str | None = None,
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

    # Step 2: Break-glass path (checked first; bypasses consent & prohibitions for 'view' only)
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
        start = datetime.fromisoformat(bg["window"]["start"])
        end = datetime.fromisoformat(bg["window"]["end"])
        if not (start <= current_time <= end):
            reasoning.append(f"Break-glass window failed: {current_time} not within [{start}, {end}]")
            return False, reasoning

        reasoning.append("Break-glass conditions met: EMERGENCY + justification present + within window.")
        reasoning.append("Bypassing consent and prohibitions for 'view' (minimum necessary applies).")

        # Record/post-hoc obligations (do not block immediate access)
        pending = []
        for ob in bg.get("obligations", []):
            k = (user, patient_id, ob["action"])
            if break_glass_log.get(k):
                reasoning.append(f"Break-glass obligation satisfied: {ob['action']}.")
            else:
                pending.append(ob["action"])
        if pending:
            reasoning.append("Post-hoc obligations pending: " + ", ".join(pending))

        # Still require standard audit if configured
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
# Example scenarios (July 28, 2025)
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
    current_time=now,
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
    justification="Patient unresponsive; immediate access required to prior potassium levels.",
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
    justification="Emergency, but export not necessary.",
)

# ───────────────────────────────────────────────────────────────
# ARC sections (Answer / Reason why / Check) — only ARC output
# ───────────────────────────────────────────────────────────────

def _section(title: str):
    bar = "=" * 72
    print("\n" + bar)
    print(title)
    print(bar)

def _eval_snapshot(u, a, pid, rtype, cat, pou, ts, *, used_bg, just):
    """Compute key gates for checklist display."""
    target_ok = (
        policy["target"]["patient_id"] == pid
        and policy["target"]["resource_type"] == rtype
        and policy["target"]["category"] == cat
    )

    # Prohibition match?
    prohibition_hit = any(
        r.get("assignee") == u and r.get("action") == a
        for r in policy.get("prohibitions", [])
    )

    # Matching permission
    perm = next(
        (r for r in policy.get("permissions", []) if r.get("assignee") == u and r.get("action") == a),
        None
    )

    # Standard constraints (if any)
    purpose_ok = consent_ok = window_ok = None
    if perm and not used_bg:
        for c in perm.get("constraints", []):
            if c["type"] == "purposeOfUse":
                purpose_ok = (pou in c["value"])
            elif c["type"] == "consent":
                consent_ok = (consent_registry.get(pid, "unknown") == c["value"])
            elif c["type"] == "encounterWindow":
                start = datetime.fromisoformat(c["value"]["start"])
                end = datetime.fromisoformat(c["value"]["end"])
                window_ok = (start <= ts <= end)

    # Obligations
    needs_audit = any(
        o.get("assignee") == u and o.get("action") == "audit" and o.get("target") == pid
        for o in policy.get("obligations", [])
    )
    audit_ok = audit_log.get((u, pid, "audit"), False) if needs_audit else True

    # Break-glass gates
    bg = policy.get("break_glass", {})
    bg_enabled = bg.get("enabled", False)
    bg_action_ok = (a == "view")
    bg_purpose_ok = (pou == "EMERGENCY")
    bg_just_ok = bool(just and just.strip())
    if bg:
        start_bg = datetime.fromisoformat(bg["window"]["start"])
        end_bg = datetime.fromisoformat(bg["window"]["end"])
        bg_window_ok = (start_bg <= ts <= end_bg)
        # pending obligations
        pending_bg = [ob["action"] for ob in bg.get("obligations", []) if not break_glass_log.get((u, pid, ob["action"]), False)]
    else:
        bg_window_ok, pending_bg = None, []

    return {
        "target_ok": target_ok,
        "prohibition_hit": prohibition_hit,
        "permission_found": perm is not None,
        "purpose_ok": purpose_ok,
        "consent_ok": consent_ok,
        "window_ok": window_ok,
        "needs_audit": needs_audit,
        "audit_ok": audit_ok,
        "bg_used": used_bg,
        "bg_enabled": bg_enabled,
        "bg_action_ok": bg_action_ok,
        "bg_purpose_ok": bg_purpose_ok,
        "bg_just_ok": bg_just_ok,
        "bg_window_ok": bg_window_ok,
        "bg_pending": pending_bg,
    }

def _print_answer_case(label, decision, trace, u, a, pid, rtype, cat, pou, ts, *, used_bg=False, just=""):
    _section(f"Answer — {label}")
    snap = _eval_snapshot(u, a, pid, rtype, cat, pou, ts, used_bg=used_bg, just=just)
    print(f"Query:  user={u}  action={a}  patient={pid}  res={rtype}/{cat}  pou={pou}  at={ts.isoformat()}")
    print(f"Result: {'ALLOW' if decision else 'DENY'}")
    print("\nKey checks")
    print(f"  • Policy applies to target?              {snap['target_ok']}")
    if used_bg:
        print(f"  • Break-glass enabled?                   {snap['bg_enabled']}")
        print(f"  • Break-glass action is 'view'?          {snap['bg_action_ok']}")
        print(f"  • Break-glass purpose == EMERGENCY?      {snap['bg_purpose_ok']}")
        print(f"  • Break-glass justification present?     {snap['bg_just_ok']}")
        print(f"  • Break-glass within window?             {snap['bg_window_ok']}")
        print(f"  • Post-hoc obligations pending:          {', '.join(snap['bg_pending']) if snap['bg_pending'] else 'none'}")
        print(f"  • Standard audit obligation fulfilled?   {snap['audit_ok']}")
    else:
        print(f"  • Prohibition matched?                   {snap['prohibition_hit']}")
        print(f"  • Permission found?                      {snap['permission_found']}")
        if snap["purpose_ok"] is not None:
            print(f"  • Purpose-of-use allowed?                {snap['purpose_ok']}")
        if snap["consent_ok"] is not None:
            print(f"  • Consent status ok?                     {snap['consent_ok']}")
        if snap["window_ok"] is not None:
            print(f"  • Encounter window ok?                   {snap['window_ok']}")
        print(f"  • Standard audit obligation fulfilled?   {snap['audit_ok']}")
    print("\nTrace (for transparency):")
    for s in trace:
        print("   -", s)

# ---------------- ARC: Reason why ----------------
def print_reason_arc():
    _section("Reason why")
    print("Evaluation order:")
    print("  A) Break-glass path (if requested): requires EMERGENCY, justification, action=='view',")
    print("     and time within the break-glass window. It BYPASSES prohibitions and consent checks")
    print("     for 'view' (minimum necessary), but still records post-hoc obligations and the")
    print("     standard audit duty (which does not block access).")
    print("  B) Standard path (otherwise):")
    print("     1) Prohibitions (deny overrides).")
    print("     2) Permission match (assignee+action) and constraints: purpose-of-use, consent, encounter window.")
    print("     3) Obligations (e.g., audit) must be fulfilled; otherwise deny.")
    print("The result is ALLOW only if all applicable gates pass.")

# ------------- ARC: Check (harness) -------------
def print_check_arc():
    _section("Check (harness)")
    ok_all = True

    # Baselines from demo above
    ok_all &= (decision_a is False)  # consent revoked → deny in standard path
    print(f"A) Standard path denied when consent revoked? {decision_a is False}")

    ok_all &= (decision_b is True)   # break-glass view allowed
    print(f"B) Break-glass view allowed with EMERGENCY+justification+window? {decision_b is True}")

    ok_all &= (decision_c is False)  # break-glass does not allow 'export'
    print(f"C) Break-glass export attempt denied? {decision_c is False}")

    # Break-glass guards
    dec_purpose, _ = check_clinical_policy(
        "clinician789", "view", "patient123", "Observation", "laboratory",
        "TREATMENT", now, break_glass=True, justification="reason"
    )
    print(f"Break-glass requires EMERGENCY purpose? {dec_purpose is False}")
    ok_all &= (dec_purpose is False)

    dec_just, _ = check_clinical_policy(
        "clinician789", "view", "patient123", "Observation", "laboratory",
        "EMERGENCY", now, break_glass=True, justification=""
    )
    print(f"Break-glass requires non-empty justification? {dec_just is False}")
    ok_all &= (dec_just is False)

    dec_window, _ = check_clinical_policy(
        "clinician789", "view", "patient123", "Observation", "laboratory",
        "EMERGENCY", datetime(2025, 9, 1), break_glass=True, justification="late"
    )
    print(f"Break-glass enforces window bounds? {dec_window is False}")
    ok_all &= (dec_window is False)

    # Standard consent flow: grant consent and verify allow (audit still required)
    saved_consent = dict(consent_registry)
    try:
        consent_registry["patient123"] = "granted"
        dec_std, _ = check_clinical_policy(
            "clinician789", "view", "patient123", "Observation", "laboratory",
            "TREATMENT", now
        )
        print(f"Standard path allows when consent granted? {dec_std is True}")
        ok_all &= (dec_std is True)
    finally:
        consent_registry.clear()
        consent_registry.update(saved_consent)

    # Obligation gating in standard path: audit missing → deny
    saved_audit = dict(audit_log)
    try:
        audit_log[("clinician789","patient123","audit")] = False
        dec_obl, _ = check_clinical_policy(
            "clinician789", "view", "patient123", "Observation", "laboratory",
            "TREATMENT", now
        )
        print(f"Standard path denies when audit obligation unfulfilled? {dec_obl is False}")
        ok_all &= (dec_obl is False)
    finally:
        audit_log.clear()
        audit_log.update(saved_audit)

    # Determinism/idempotence
    d1, _ = check_clinical_policy(
        "clinician789", "view", "patient123", "Observation", "laboratory",
        "EMERGENCY", now, break_glass=True, justification="x"
    )
    d2, _ = check_clinical_policy(
        "clinician789", "view", "patient123", "Observation", "laboratory",
        "EMERGENCY", now, break_glass=True, justification="x"
    )
    print(f"Deterministic on identical inputs? {d1 == d2}")
    ok_all &= (d1 == d2)

    print(f"\nAll checks passed? {ok_all}")

# ───────────────────────────────────── Main ─────────────────────────────────────

if __name__ == "__main__":
    _print_answer_case(
        "A) Standard path (consent revoked → DENY)",
        decision_a, trace_a,
        "clinician789", "view", "patient123", "Observation", "laboratory",
        "TREATMENT", now, used_bg=False
    )
    _print_answer_case(
        "B) Break-glass view (ALLOW)",
        decision_b, trace_b,
        "clinician789", "view", "patient123", "Observation", "laboratory",
        "EMERGENCY", now, used_bg=True,
        just="Patient unresponsive; immediate access required to prior potassium levels."
    )
    _print_answer_case(
        "C) Break-glass export attempt (DENY)",
        decision_c, trace_c,
        "clinician789", "export", "patient123", "Observation", "laboratory",
        "EMERGENCY", now, used_bg=True, just="Emergency, but export not necessary."
    )
    print_reason_arc()
    print_check_arc()

