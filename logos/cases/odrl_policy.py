#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
odrl_policy.py — Minimal ODRL-style policy evaluator with ARC output

What this script does
---------------------
• Defines a tiny, ODRL-inspired policy with:
    - a permission (with a date-time constraint),
    - a prohibition,
    - and an obligation (notify).
• Evaluates a (user, action, target) query at a given timestamp.
• Then prints explicit ARC sections:
    - Answer: compact checklist of key gates and the final decision.
    - Reason why: the evaluation order / precedence rules.
    - Check (harness): quick self-tests for time constraint, prohibition precedence,
      obligation gating, and determinism.

How to run
----------
    python odrl_policy.py

Adjust the variables near the bottom (user/action/target/now) to try scenarios.
"""

from datetime import datetime

# ================================================================
# Sample ODRL-like policy (kept simple and readable)
# ================================================================
policy = {
    "target": "document123",
    "permissions": [
        {
            "assignee": "user123",
            "action": "read",
            "constraints": [
                {
                    "type": "dateTime",
                    "operator": "gteq",
                    "value": "2025-07-01T00:00:00",
                }
            ],
        }
    ],
    "prohibitions": [
        {
            "assignee": "user123",
            "action": "delete",
        }
    ],
    "obligations": [
        {
            "assignee": "user123",
            "action": "notify",
            "target": "document123",
        }
    ],
}

# Simulated obligation log (fulfilled duties).
# True means user123 sent a notification about document123.
obligation_log = {
    ("user123", "document123", "notify"): True
}

# ================================================================
# Policy Checker
# ================================================================
def check_policy(user, action, target, current_time):
    reasoning = []

    # Step 1: target applicability
    if policy["target"] != target:
        reasoning.append(f"Policy does not apply to target '{target}'")
        return False, reasoning
    reasoning.append(f"Evaluating policy for target '{target}'")

    # Step 2: Prohibitions (deny overrides allow)
    for rule in policy.get("prohibitions", []):
        if rule["assignee"] == user and rule["action"] == action:
            reasoning.append(
                f"Action '{action}' is explicitly prohibited for user '{user}'"
            )
            return False, reasoning

    # Step 3: Permissions (must match and satisfy constraints)
    for rule in policy.get("permissions", []):
        if rule["assignee"] == user and rule["action"] == action:
            reasoning.append(
                f"Permission found for action '{action}' by user '{user}'"
            )
            # Step 3a: constraints
            for constraint in rule.get("constraints", []):
                if constraint["type"] == "dateTime" and constraint["operator"] == "gteq":
                    constraint_time = datetime.fromisoformat(constraint["value"])
                    if current_time < constraint_time:
                        reasoning.append(
                            f"Time constraint failed: current time {current_time} < {constraint_time}"
                        )
                        return False, reasoning
                    else:
                        reasoning.append(
                            f"Time constraint passed: {current_time} >= {constraint_time}"
                        )

            # Step 4: Obligations (must be fulfilled)
            for obligation in policy.get("obligations", []):
                if (
                    obligation["assignee"] == user
                    and obligation["action"] == "notify"
                    and obligation["target"] == target
                ):
                    key = (user, target, "notify")
                    if obligation_log.get(key):
                        reasoning.append("Obligation 'notify' has been fulfilled.")
                    else:
                        reasoning.append(
                            "Obligation 'notify' has NOT been fulfilled. Denying access."
                        )
                        return False, reasoning

            reasoning.append("All checks passed. Permission granted.")
            return True, reasoning

    # Step 5: No matching permission
    reasoning.append(
        f"No matching permission found for action '{action}' and user '{user}'"
    )
    return False, reasoning


# ================================================================
# Example query: user, action, target, and time
# (Adjust these to experiment)
# ================================================================
user = "user123"
action = "read"
target = "document123"
now = datetime(2025, 7, 28)  # Change to test different times

# Run the policy checker
decision, trace = check_policy(user, action, target, now)

# ───────────────────────────────────────────────────────────────
# ARC sections (Answer / Reason why / Check) — clearly separated
# ───────────────────────────────────────────────────────────────

def _section(title: str):
    bar = "=" * 72
    print("\n" + bar)
    print(title)
    print(bar)

def _eval_snapshot(u, a, t, ts):
    """Re-evaluate key booleans for the ARC Answer."""
    target_ok = (policy["target"] == t)
    # Prohibition hit?
    prohibition_hit = any(
        r.get("assignee") == u and r.get("action") == a
        for r in policy.get("prohibitions", [])
    )
    # Matching permission (first)
    perm = next(
        (r for r in policy.get("permissions", []) if r.get("assignee") == u and r.get("action") == a),
        None
    )
    constraint_ok = None
    if perm:
        ok = True
        for c in perm.get("constraints", []):
            if c.get("type") == "dateTime" and c.get("operator") == "gteq":
                thr = datetime.fromisoformat(c["value"])
                ok = ok and (ts >= thr)
        constraint_ok = ok
    # Obligation needed & status
    needs_notify = any(
        o.get("assignee") == u and o.get("action") == "notify" and o.get("target") == t
        for o in policy.get("obligations", [])
    )
    notify_fulfilled = obligation_log.get((u, t, "notify"), False) if needs_notify else True
    return {
        "target_ok": target_ok,
        "prohibition_hit": prohibition_hit,
        "permission_found": perm is not None,
        "constraint_ok": constraint_ok,
        "needs_notify": needs_notify,
        "notify_fulfilled": notify_fulfilled,
    }

# ------------------- ARC: Answer -------------------
def print_answer_arc(u, a, t, ts, decision, trace):
    _section("Answer")
    snap = _eval_snapshot(u, a, t, ts)
    print(f"Query:  user={u}  action={a}  target={t}  at={ts.isoformat()}")
    print(f"Result: {'ALLOW' if decision else 'DENY'}")
    print("\nKey checks")
    print(f"  • Policy applies to target?       {snap['target_ok']}")
    print(f"  • Prohibition matched?            {snap['prohibition_hit']}")
    print(f"  • Permission found?               {snap['permission_found']}")
    if snap["constraint_ok"] is not None:
        print(f"  • Constraints satisfied?          {snap['constraint_ok']}")
    print(f"  • Obligation required (notify)?   {snap['needs_notify']}")
    print(f"  • Obligation fulfilled?           {snap['notify_fulfilled']}")
    print("\nTrace (for transparency):")
    for s in trace:
        print("   -", s)

# ---------------- ARC: Reason why ----------------
def print_reason_arc():
    _section("Reason why")
    print("Evaluation order:")
    print("  1) Target match — the policy must apply to the requested target.")
    print("  2) Prohibitions — if a prohibition matches (assignee+action), access is denied.")
    print("  3) Permissions — if a permission matches, all its constraints must hold.")
    print("  4) Obligations — any required duties (e.g., 'notify') must be fulfilled;")
    print("     otherwise access is denied. Only then is access granted.")

# ------------- ARC: Check (harness) -------------
def print_check_arc():
    _section("Check (harness)")
    ok_all = True

    # A) Baseline: current defaults should ALLOW (time ok, notify fulfilled)
    decA, _ = check_policy("user123", "read", "document123", datetime(2025, 7, 28))
    print(f"Baseline allow? {decA}")
    ok_all &= decA is True

    # B) Time constraint: before threshold should DENY
    decB, trB = check_policy("user123", "read", "document123", datetime(2025, 6, 30))
    print(f"Time constraint enforced (pre-threshold denies)? {decB is False}")
    ok_all &= decB is False

    # C) Prohibition precedence: 'delete' must be denied even if time & notify ok
    decC, _ = check_policy("user123", "delete", "document123", datetime(2025, 7, 28))
    print(f"Prohibition overrides permission? {decC is False}")
    ok_all &= decC is False

    # D) Obligation fulfillment: remove notify → DENY; restore afterwards
    saved = dict(obligation_log)
    try:
        obligation_log[("user123", "document123", "notify")] = False
        decD, _ = check_policy("user123", "read", "document123", datetime(2025, 7, 28))
        print(f"Obligation gating enforced (missing → deny)? {decD is False}")
        ok_all &= decD is False
    finally:
        obligation_log.clear()
        obligation_log.update(saved)

    # E) Determinism/idempotence
    decE1, _ = check_policy("user123", "read", "document123", datetime(2025, 7, 28))
    decE2, _ = check_policy("user123", "read", "document123", datetime(2025, 7, 28))
    print(f"Deterministic on same inputs? {decE1 == decE2}")
    ok_all &= (decE1 == decE2)

    print(f"\nAll checks passed? {ok_all}")

# Run ARC sections
print_answer_arc(user, action, target, now, decision, trace)
print_reason_arc()
print_check_arc()

