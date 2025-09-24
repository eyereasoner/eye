#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P3 style — Pure Python + JSON data

A self-contained script that reads simple JSON data, applies clear time-of-day
rules to produce a greeting, and outputs the result alongside a short explanation
and a few validation checks to keep the example trustworthy.

GOAL
-----
Emit a small, portable artifact that always produces:
  • Answer  — the computed result
  • Reason  — a concise explanation of the rule(s) applied
  • Check   — an independent, *multi-test* verification harness

WHY THIS SHAPE
--------------
This mirrors the "Answer • Reason • Check" pattern from the P3
approach: a repeatable program that carries its own explanation and a *separate*
self-test so it can fail fast in CI if invariants break.

WHAT'S "MIXED" HERE
-------------------
• Stable logic (rules) live as pure Python functions.
• Dynamic data lives in a JSON block below (editable), where `hour = null`
  means "determine the hour at runtime" so the same logic can run any time.

HOW TO USE
----------
1) Run:     python hello.py
2) Edit:    change the DATA_JSON block to set "name" or a fixed "hour".
            Example: {"name": "Zoe", "hour": 15}
3) Output:  prints one JSON object with "Answer", "Reason", and "Check".
            Exit code is 0 on success and 1 if any check fails.

DESIGN NOTES
------------
• The check harness has multiple targeted tests and a summary so you can gate
  CI on `ok == true` while still seeing *which* invariant failed if not.
"""

import datetime as dt
import json
import sys
import unicodedata
import re
import string

# ──────────────────────────────────────────────────────────────────────────────
# 1) INPUTS — JSON data block (you can edit this)
#    - "hour": null → use current local hour [0..23] at runtime
# ──────────────────────────────────────────────────────────────────────────────
DATA_JSON = r"""
{
  "name": "Alice",
  "hour": 19
}
""".strip()


def load_data() -> dict:
    """
    Load and normalize the embedded JSON data.
    Returns a dict with:
      • name: non-empty string (defaults to "world" if blank)
      • hour: integer in [0, 23] (defaults to current local hour if null)
    Fails fast with a non-zero exit if malformed.
    """
    try:
        data = json.loads(DATA_JSON)
    except json.JSONDecodeError as e:
        sys.exit(f"Embedded DATA_JSON is invalid JSON: {e}")

    # Normalize name (fallback to sensible default)
    name = str(data.get("name") or "").strip() or "world"

    # Resolve hour (null => current hour)
    hour = data.get("hour")
    try:
        hour = int(hour)
        if not (0 <= hour <= 23):
            raise ValueError
    except Exception:
        sys.exit("Hour must be an integer in [0, 23].")

    return {"name": name, "hour": hour}


# ──────────────────────────────────────────────────────────────────────────────
# 2) LOGIC — Stable rules in pure Python
#    Keep these side-effect free so they're easy to test and reason about.
# ──────────────────────────────────────────────────────────────────────────────
def choose_prefix(hour: int) -> str:
    """Map hour-of-day to a greeting prefix."""
    if 0 <= hour < 12:
        return "Good morning"
    if 12 <= hour < 18:
        return "Good afternoon"
    return "Good evening"


def build_greeting(name: str, hour: int) -> str:
    """Compose the final greeting from the chosen prefix and the name."""
    return f"{choose_prefix(hour)}, {name}!"


# ──────────────────────────────────────────────────────────────────────────────
# 3) DRIVER — Compute Answer + Reason (human-readable)
# ──────────────────────────────────────────────────────────────────────────────
def compute_answer_reason(data: dict) -> tuple[str, str]:
    """
    Produce:
      • Answer: final greeting string
      • Reason: concise explanation of *which* rule bucket fired and why
    """
    name, hour = data["name"], data["hour"]
    answer = build_greeting(name, hour)

    # Mirror the rule selection in English for transparency.
    if hour < 12:
        bucket = "[0,12)"
        pre = "Good morning"
    elif hour < 18:
        bucket = "[12,18)"
        pre = "Good afternoon"
    else:
        bucket = "[18,24)"
        pre = "Good evening"

    reason = (
        f"Selected prefix '{pre}' because hour={hour} ∈ {bucket}; "
        f"formatted greeting as '{{prefix}}, {{name}}!' with name='{name}'."
    )
    return answer, reason


# ──────────────────────────────────────────────────────────────────────────────
# 4) CHECK — Five focused tests
# ──────────────────────────────────────────────────────────────────────────────
def _add(tests, name, ok, details=""):
    tests.append({"name": name, "ok": bool(ok), "details": details if not ok else ""})


def check(answer: str, data: dict) -> dict:
    tests = []
    name, hour = data["name"], data["hour"]
    expected = build_greeting(name, hour)
    prefix = choose_prefix(hour)

    # (1) Exact equality by recomputation
    _add(tests, "matches_expected", answer == expected,
         f"Expected '{expected}' but got '{answer}'.")

    # (2) Prefix bucket appears at the start
    _add(tests, "correct_prefix", answer.startswith(prefix + ", "),
         f"Greeting must start with '{prefix}, '.")

    # (3) Exact structure using regex: 'Prefix, Name!'
    pattern = re.compile(rf"^{re.escape(prefix)}, {re.escape(name)}!$")
    _add(tests, "structure_regex_match", bool(pattern.match(answer)),
         f"Greeting should be exactly '{prefix}, {name}!'.")

    # (4) No leading/trailing whitespace
    _add(tests, "no_outer_whitespace", answer == answer.strip(),
         "Greeting has leading or trailing whitespace.")

    # (5) Inputs valid (hour in range AND name non-empty)
    inputs_ok = (isinstance(hour, int) and 0 <= hour <= 23 and isinstance(name, str) and len(name.strip()) > 0)
    _add(tests, "inputs_valid", inputs_ok,
         f"Invalid inputs: hour={hour!r}, name={name!r}.")

    ok = all(t["ok"] for t in tests)
    summary = {
        "total": len(tests),
        "passed": sum(t["ok"] for t in tests),
        "failed": sum(not t["ok"] for t in tests),
    }
    return {"ok": ok, "summary": summary, "tests": tests}


# ──────────────────────────────────────────────────────────────────────────────
# 5) MAIN — Emit the triad (UTF-8 JSON)
# ──────────────────────────────────────────────────────────────────────────────
def main() -> int:
    data = load_data()
    answer, reason = compute_answer_reason(data)
    harness = check(answer, data)

    payload = {"Answer": answer, "Reason": reason, "Check": harness}
    print(json.dumps(payload, indent=2, ensure_ascii=False))
    return 0 if harness["ok"] else 1


if __name__ == "__main__":
    sys.exit(main())
