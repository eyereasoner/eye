#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P3 — Mixed Computation “Hello”

A tiny p3 example where facts about a person and the current hour are fed
to a single N3 backward rule that infers the right greeting, then reports the
result with a brief explanation and a small set of sanity checks.

WHAT THIS DOES
--------------
This program demonstrates the EYE “Answer • Reason • Check” pattern:
  • Answer — the derived greeting
  • Reason — a concise explanation of the rule path taken
  • Check  — an independent harness (5 focused tests)

SHAPE / RESPONSIBILITIES
------------------------
• Stable Logic (N3, ONE backward rule):
    - The *single* backward rule (<=) computes the greeting *and* calculates
      the greeting prefix from the hour *inside the rule body*:
        - q = ⌊hour/6⌋ ∈ {0,1,2,3}
        - pick prefix via list:at over a 4-item list
        - format "%s, %s!" from (prefix, name) ⇒ :p :says ?g
• Dynamic Data (runtime):
    - Python provides `:me :name "Alice"` (or from argv) and `:ctx :hour <int>`.
      Hour defaults to the local hour; override with HELLO_HOUR env var.
• Goal (N3):
    - Pure query pattern: `{ :me :says ?g }.` (no rules inside)

TEMP FILES
----------
All three artifacts — data.n3, logic.n3, goal.n3 — are written into a
TemporaryDirectory for a single EYE run and then discarded automatically.

USAGE
-----
  python hell_eye.py

OUTPUT
------
A single JSON object to stdout containing:
  { "Answer": "...", "Reason": "...", "Check": { ... } }

DEPENDENCIES
------------
• Python 3.8+
• EYE on PATH (e.g., `eye --version`)
"""

import json
import os
import re
import subprocess
import sys
import tempfile
import datetime as dt
from typing import Tuple, Dict, Any


# ──────────────────────────────────────────────────────────────────────────────
# 1) STABLE LOGIC (N3) — exactly ONE backward rule with in-body prefix calc
#    - Uses:
#        math:integerQuotient to bucket hour → q ∈ {0,1,2,3}
#        list:at to select prefix by q
#        string:format to produce the final string
# ──────────────────────────────────────────────────────────────────────────────
STABLE_LOGIC = """@prefix : <http://example.org/hello#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix list: <http://www.w3.org/2000/10/swap/list#> .
@prefix string: <http://www.w3.org/2000/10/swap/string#> .

# Head: conclude that a person says a greeting string.
{  ?p :says ?g }
<=
{  # Read the hour from context
   :ctx :hour ?h .

   # Bucket the hour into 0..3 via integer division by 6:
   #   0 → [0..5], 1 → [6..11], 2 → [12..17], 3 → [18..23]
   ( ?h 6 ) math:integerQuotient ?q .

   # Select the greeting prefix from a 4-item list using that bucket:
   #   0 → "Good morning"
   #   1 → "Good morning"
   #   2 → "Good afternoon"
   #   3 → "Good evening"
   ( ( "Good morning" "Good morning" "Good afternoon" "Good evening" ) ?q ) list:memberAt ?pre .

   # Get the name of the person and format "%s, %s!"
   ?p :name ?name .
   ( "%s, %s!" ?pre ?name ) string:format ?g .
} .
"""

# GOAL — plain query pattern (no rules here)
GOAL = """@prefix : <http://example.org/hello#> .
{ :me :says ?g } => { :me :says ?g } .
"""


# ──────────────────────────────────────────────────────────────────────────────
# 2) DATA — provide name and hour as N3 facts (runtime dynamic inputs)
# ──────────────────────────────────────────────────────────────────────────────
def make_data(name: str, hour: int) -> str:
    """
    Create a minimal N3 dataset that the logic will read.
    We provide both :name and :hour; the rule computes everything else.
    """
    return f"""@prefix : <http://example.org/hello#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:me   :name "{name}" .
:ctx  :hour "{hour}"^^xsd:integer .
"""


# ──────────────────────────────────────────────────────────────────────────────
# 3) DRIVER — write temp files, invoke EYE, parse Answer, explain Reason
# ──────────────────────────────────────────────────────────────────────────────
def run_eye_once(data_n3: str, logic_n3: str, goal_n3: str) -> str:
    """
    Write data/logic/goal into a temp dir and run EYE as a single shot.
    Returns EYE's stdout (stripped).
    """
    with tempfile.TemporaryDirectory() as td:
        td_path = td  # for clarity
        data_path = os.path.join(td_path, "data.n3")
        logic_path = os.path.join(td_path, "logic.n3")
        goal_path = os.path.join(td_path, "goal.n3")

        with open(data_path, "w", encoding="utf-8") as f:
            f.write(data_n3)
        with open(logic_path, "w", encoding="utf-8") as f:
            f.write(logic_n3)
        with open(goal_path, "w", encoding="utf-8") as f:
            f.write(goal_n3)

        # --quiet: less chatter; --nope: skip proof (we make our own Reason text)
        cmd = ["eye", "--quiet", "--nope", data_path, logic_path, "--query", goal_path]
        out = subprocess.check_output(cmd, text=True)
        return out.strip()


def parse_answer(eye_output: str) -> str:
    """
    Extract the greeting string from a line like:
      :me :says "Good morning, Alice!" .
    This is a simple slice on the first/last double quote; good enough here.
    """
    start = eye_output.find('"')
    end = eye_output.rfind('"')
    if start != -1 and end > start:
        return eye_output[start + 1 : end]
    raise ValueError(f"Could not parse greeting from: {eye_output}")


def explain_reason(name: str, hour: int) -> str:
    """
    Human explanation mirroring the rule's math:
      q = ⌊hour/6⌋ ∈ {0,1,2,3} → prefix via list:at → format "%s, %s!"
    """
    q = max(0, min(3, hour // 6))
    prefixes = ["Good morning", "Good morning", "Good afternoon", "Good evening"]
    pre = prefixes[q]
    bucket = {0: "[0,6)", 1: "[6,12)", 2: "[12,18)", 3: "[18,24)"}[q]
    return (f"Computed q=⌊hour/6⌋={q} so hour={hour} ∈ {bucket} ⇒ prefix='{pre}'; "
            f"the single backward rule built \"%s, %s!\" with name='{name}'.")


# ──────────────────────────────────────────────────────────────────────────────
# 4) CHECK — 5 focused tests (independent of EYE output beyond 'answer' string)
#    These checks *recompute* expectations and assert invariants.
# ──────────────────────────────────────────────────────────────────────────────
def _add(tests, name, ok, details=""):
    tests.append({"name": name, "ok": bool(ok), "details": details if not ok else ""})


def check(answer: str, name: str, hour: int) -> Dict[str, Any]:
    tests = []

    # Helper: recompute expected prefix via the same q=⌊hour/6⌋ mapping
    q = max(0, min(3, hour // 6))
    prefixes = ["Good morning", "Good morning", "Good afternoon", "Good evening"]
    pre = prefixes[q]
    expected = f"{pre}, {name}!"

    # (1) Exact equality by recomputation
    _add(tests, "matches_expected", answer == expected,
         f"Expected '{expected}' but got '{answer}'.")

    # (2) Prefix agreement (bucket math correctness in output)
    _add(tests, "prefix_bucket_math", answer.startswith(pre + ", "),
         f"Greeting must start with '{pre}, ' given hour={hour} (q={q}).")

    # (3) Exact structure: 'Prefix, Name!' (tight regex — no stray spaces)
    pattern = re.compile(rf"^{re.escape(pre)}, {re.escape(name)}!$")
    _add(tests, "structure_regex_match", bool(pattern.match(answer)),
         f"Greeting should be exactly '{pre}, {name}!'.")

    # (4) No leading/trailing whitespace
    _add(tests, "no_outer_whitespace", answer == answer.strip(),
         "Greeting has leading or trailing whitespace.")

    # (5) Inputs valid (defensive guardrail)
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
# 5) MAIN — gather inputs, run EYE in a temp dir, emit triad to stdout (JSON)
# ──────────────────────────────────────────────────────────────────────────────
def main() -> int:
    name = "Alice"
    hour = 19

    # Prepare artifacts and run EYE once (everything in temp files)
    data_n3 = make_data(name, hour)
    eye_output = run_eye_once(data_n3, STABLE_LOGIC, GOAL)

    # Build Answer • Reason • Check
    answer = parse_answer(eye_output)
    reason = explain_reason(name, hour)
    harness = check(answer, name, hour)

    # Emit one JSON object (UTF-8, keep “∈” and friends)
    payload = {"Answer": answer, "Reason": reason, "Check": harness}
    print(json.dumps(payload, indent=2, ensure_ascii=False))
    return 0 if harness["ok"] else 1


if __name__ == "__main__":
    sys.exit(main())

