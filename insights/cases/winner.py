#!/usr/bin/env python3
"""
Translate a small N3 rules+facts snippet about runners and winners into a
plain answer, a reason (natural-language translation of the rules), and a
check harness that mechanically validates the conclusion.

Assumptions captured from the N3:

Rule 1 (exclusive-or phrased via contradiction):
If X is a Runner and a Winner, then for any other Y who is a Runner and Y != X,
asserting that Y is a Winner leads to contradiction.  (In the N3, this is written
by making the implication { Y a :Winner. } => ($ $).)

Rule 2 (impossibility of two distinct winners):
If two distinct runners X and Y are both Winners, then contradiction ($ $).
"""

import re
from textwrap import dedent

N3 = dedent("""\
@prefix log: <http://www.w3.org/2000/10/swap/log#>.
@prefix : <http://example.org/#>.

# Exclusive or example.
# If X is a winner, then Y is not a winner
{
    ?X a :Runner.
    ?X a :Winner.
    ?Y a :Runner.
    ?Y log:notEqualTo ?X.
} => {
    {
        ?Y a :Winner.
    } => ($ $).
}.

# It is impossible that both X and Y are winners
{
    ?X a :Runner.
    ?X a :Winner.
    ?Y a :Runner.
    ?Y a :Winner.
    ?Y log:notEqualTo ?X.
} => ($ $).

# facts
:Joe a :Runner.
:Vic a :Runner.
:Ray a :Runner.
:Tom a :Runner.

:Vic a :Winner.
#:Ray a :Winner.

# query
{
    ?S ?P ?O.
} =^ {
    ?S ?P ?O.
}.

{
    {
        ?S a :Winner.
    } => ($ $).
} =^ {
    {
        ?S a :Winner.
    } => ($ $).
}.
""")

# ---------- Minimal parser for the concrete snippet ----------
# We only need to extract *facts* that are outside any {...} rule/query blocks,
# and we only care about triples of the form ":Name a :Runner/ :Winner ."

def extract_facts(n3_text):
    runners, winners = set(), set()

    # Remove end-of-line comments (but keep commented-out facts intact by ignoring them later)
    lines = n3_text.splitlines()

    brace_depth = 0
    fact_re = re.compile(r"^:(\w+)\s+a\s+:(Runner|Winner)\s*\.\s*$")

    for raw in lines:
        line = raw.rstrip()

        # Track brace depth to skip inside rule/query bodies
        opens = line.count("{")
        closes = line.count("}")
        # If the line is commented out entirely, skip it early
        stripped = line.lstrip()
        if stripped.startswith("#"):
            brace_depth += opens - closes
            continue

        if brace_depth == 0:
            # Also ignore inline comments after facts
            # (split once on # if present)
            line_wo_comment = line.split("#", 1)[0].strip()
            if not line_wo_comment:
                brace_depth += opens - closes
                continue

            m = fact_re.match(line_wo_comment)
            if m:
                who, what = m.group(1), m.group(2)
                if what == "Runner":
                    runners.add(who)
                elif what == "Winner":
                    winners.add(who)

        brace_depth += opens - closes

    return runners, winners

runners, winners = extract_facts(N3)

# ---------- Reasoning exactly as per the two rules ----------
# Rule 2 says: it is impossible to have two distinct winners among runners.
# Rule 1 says: if there exists a winner X among runners, then any other runner Y != X
# being a winner would lead to contradiction. Together, they enforce "at most one winner".
#
# With the provided facts we *do* have one winner (:Vic), so all other runners cannot be winners.

inconsistent_now = len(winners) > 1
unique_winner = None
forbidden_if_asserted = set()

if not inconsistent_now:
    if len(winners) == 1:
        unique_winner = next(iter(winners))
        # Rule 1 consequence: every other runner would contradict if asserted Winner
        forbidden_if_asserted = {y for y in runners if y != unique_winner}
    else:
        # No winner asserted in facts; rules would still forbid having 2 or more.
        unique_winner = None
        forbidden_if_asserted = set()  # nothing to forbid until someone is asserted a winner

# ---------- Build the textual outputs ----------

def english_join(names):
    names = list(names)
    if not names:
        return ""
    if len(names) == 1:
        return names[0]
    return ", ".join(names[:-1]) + " and " + names[-1]

answer_lines = []
if inconsistent_now:
    answer_lines.append("❌ Inconsistent facts: more than one winner is asserted, which the rules forbid.")
else:
    if unique_winner:
        others = sorted(runners - {unique_winner})
        answer_lines.append(f"✅ The (only) winner is **{unique_winner}**.")
        if others:
            answer_lines.append(f"The other runners ({', '.join(others)}) cannot be winners without contradiction.")
    else:
        answer_lines.append("ℹ️ No winner is asserted in the facts. The rules allow **at most one** runner to be a winner.")

reason = dedent(f"""\
    • Rule 1 (exclusion by contradiction): If some X is a Runner and a Winner, then for any other Runner Y ≠ X,
      the statement “Y is a Winner” would entail a contradiction (written as {{ Y a :Winner. }} ⇒ ($ $) in N3).
    • Rule 2 (no two winners): If two distinct runners X and Y are both Winners, that entails a contradiction (($ $)).
    • Facts: runners = {{{", ".join(sorted(runners))}}}; winners asserted = {{{", ".join(sorted(winners)) or "∅"}}}.
    • Therefore, there can be **at most one** winner. Since {unique_winner or "no one"} is asserted as Winner,
      every other runner would cause a contradiction if also asserted a Winner.""")

# ---------- Check harness ----------
# We mechanically verify the exclusivity by attempting to add each other runner as a winner
# and reporting whether the rule set would be violated.

def consistency_if_add(who):
    """Return True iff adding 'who' as a Winner would keep things consistent
       with the rules (i.e., no two distinct winners)."""
    added = set(winners)
    added.add(who)
    # Only runners matter for the rules; ignore non-runners if any were present.
    actual_winners = [w for w in added if w in runners]
    return len(set(actual_winners)) <= 1  # Rule 2 captures the contradiction exactly.

check_reports = []
if inconsistent_now:
    check_reports.append("Already inconsistent, so further checks are moot.")
else:
    if unique_winner:
        for y in sorted(runners - {unique_winner}):
            ok = consistency_if_add(y)
            check_reports.append(f"Assert '{y} a :Winner.': {'OK' if ok else '❌ CONTRADICTION (two distinct winners)'}")
    else:
        # No current winner: try asserting each runner and see that each single assertion is OK,
        # but any two-at-once would be a contradiction.
        for y in sorted(runners):
            ok = consistency_if_add(y)
            check_reports.append(f"Assert '{y} a :Winner.': {'OK' if ok else '❌ CONTRADICTION'}")

# ---------- Print the three requested sections ----------
print("Answer")
print("------")
print("\n".join(answer_lines))
print()

print("Reason")
print("------")
print(reason)
print()

print("Check (harness)")
print("----------------")
for line in check_reports:
    print("•", line)

