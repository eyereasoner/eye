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
                {
                    "type": "dateTime",
                    "operator": "gteq",
                    "value": "2025-07-01T00:00:00"
                }
            ]
        }
    ],
    "prohibitions": [
        {
            "assignee": "user123",
            "action": "delete"
        }
    ],
    "obligations": [
        {
            "assignee": "user123",
            "action": "notify",
            "target": "document123"
        }
    ]
}

# ✅ Simulated obligation log (fulfilled duties)
obligation_log = {
    ("user123", "document123", "notify"): True  # means user123 sent a notification
}

# ---------------------------------------
# 🧠 Policy Checker (with thinking trace)
# ---------------------------------------
def check_policy(user, action, target, current_time):
    reasoning = []

    # Step 1: Check if policy applies to the target
    if policy["target"] != target:
        reasoning.append(f"Policy does not apply to target '{target}'")
        return False, reasoning
    reasoning.append(f"Evaluating policy for target '{target}'")

    # Step 2: Check prohibitions (deny overrides allow)
    for rule in policy.get("prohibitions", []):
        if rule["assignee"] == user and rule["action"] == action:
            reasoning.append(f"Action '{action}' is explicitly prohibited for user '{user}'")
            return False, reasoning

    # Step 3: Check permissions
    for rule in policy.get("permissions", []):
        if rule["assignee"] == user and rule["action"] == action:
            reasoning.append(f"Permission found for action '{action}' by user '{user}'")

            # Step 3a: Check constraints
            for constraint in rule.get("constraints", []):
                if constraint["type"] == "dateTime" and constraint["operator"] == "gteq":
                    constraint_time = datetime.fromisoformat(constraint["value"])
                    if current_time < constraint_time:
                        reasoning.append(f"Time constraint failed: current time {current_time} < {constraint_time}")
                        return False, reasoning
                    else:
                        reasoning.append(f"Time constraint passed: {current_time} >= {constraint_time}")

            # Step 4: Check obligations
            for obligation in policy.get("obligations", []):
                if obligation["assignee"] == user and obligation["action"] == "notify" and obligation["target"] == target:
                    key = (user, target, "notify")
                    if obligation_log.get(key):
                        reasoning.append("Obligation 'notify' has been fulfilled.")
                    else:
                        reasoning.append("Obligation 'notify' has NOT been fulfilled. Denying access.")
                        return False, reasoning

            reasoning.append("All checks passed. Permission granted.")
            return True, reasoning

    # Step 5: No matching permission
    reasoning.append(f"No matching permission found for action '{action}' and user '{user}'")
    return False, reasoning

# -------------------------------
# 🔍 Example query: user, action
# -------------------------------
user = "user123"
action = "read"
target = "document123"
now = datetime(2025, 7, 28)  # Change to test different times

# Run the policy checker
decision, trace = check_policy(user, action, target, now)

# -------------------------------
# 🖨 Output: Decision & Thinking
# -------------------------------
print(f"\n=== {user} {action} {target} ===")
print("DECISION:", "ALLOW" if decision else "DENY")
print("REASONING TRACE:")
for step in trace:
    print("-", step)

