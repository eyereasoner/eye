#!/usr/bin/env python3
# ──────────────────────────────────────────────────────────────
#  GPS-style reasoner for weighted state transitions
#  -------------------------------------------------------------
#  • Reads a block of N3 rules (gps:description … <= { … }.)
#  • Parses every transition (pre-state, post-state, action,
#    duration, cost, success-probability, comfort-probability)
#    plus its *patient filter* (gender / age).
#  • Loads one concrete Patient instance (you can add more).
#  • Runs a breadth-first search that
#       – accumulates the 4 metrics,
#       – enforces exactly the same limits that appeared in the
#         original N3 query (≤ P180D, ≤ €500, ≥ 0.35, ≥ 0.35),
#       – prevents literal loops,
#       – returns *every distinct ordered action list* that
#         reaches GOAL_STATE within MAX_STAGECOUNT stages.
#  • Finally prints the solutions sorted by:
#       1. highest success probability
#       2. lowest cost
#       3. shortest duration
#
#  The code purposefully keeps the parsing & reasoning strictly
#  separated, so you can re-use `search_all()` with *any*
#  TransitionRule list you like (e.g. from Turtle, SPARQL, CSV…).
# ──────────────────────────────────────────────────────────────

from __future__ import annotations

import re
import textwrap
from dataclasses import dataclass
from typing import Callable, List, Tuple, Deque
from collections import deque

# ╔═══════════════════════════════════════╗
# ║ 1. PATIENT KNOWLEDGE BASE             ║
# ╚═══════════════════════════════════════╝
# A minimal RDF-ish fact set is encoded as a Python dataclass.
# You can of course load such facts from Turtle in the same way,
# but keeping it inline makes the example 100 % self-contained.

@dataclass(frozen=True)
class Patient:
    """All patient facts that may appear in rule pre-conditions."""
    uri: str
    name: str
    gender: str   # "Male" | "Female"
    age: int      # in years
    weight: float # kg – not used in current rules, but useful later
    diagnosis: str
    state: str    # current care:state_n

# Example patient from the very first message
patient_1 = Patient(
    uri       = "data:patient_1",
    name      = "Jane Doe",
    gender    = "Female",
    age       = 14,
    weight    = 75.0,
    diagnosis = "49049000",     # SNOMED code
    state     = "state_2",      # she starts in care:state_2
)

# ╔═══════════════════════════════════════╗
# ║ 2.  N3 RULE BLOCK                     ║
# ╚═══════════════════════════════════════╝
# Paste the *COMPLETE* set of gps:description rules here.
# The parser will read them verbatim – no further editing
# anywhere else in the script is required.

N3_RULES = textwrap.dedent("""
PREFIX math: <http://www.w3.org/2000/10/swap/math#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX e: <http://eulersharp.sourceforge.net/2003/03swap/log-rules#>
PREFIX action: <http://josd.github.io/eye/reasoning/gps/action#>
PREFIX medication: <http://josd.github.io/eye/reasoning/gps/medication#>
PREFIX sct: <http://snomed.info/id/>
PREFIX care: <http://josd.github.io/eye/reasoning/gps/care#>
PREFIX gps: <http://josd.github.io/eye/reasoning/gps/gps-schema#>

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_0.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_0
     "P1D"^^xsd:dayTimeDuration
     57
     0.7477904055178553
     0.934831799669019
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_1.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_1
     "P1D"^^xsd:dayTimeDuration
     70
     0.5152027484597278
     0.35499402902840843
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_2.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_2
     "P1D"^^xsd:dayTimeDuration
     96
     0.9681442366905944
     0.7426727154075693
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_3.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_3
     "P1D"^^xsd:dayTimeDuration
     31
     0.5660910990739108
     0.7169792720333495
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_4.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_4
     "P1D"^^xsd:dayTimeDuration
     84
     0.5835187993156218
     0.7760823157205528
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_5.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_5
     "P1D"^^xsd:dayTimeDuration
     86
     0.6351939110847972
     0.7600626863692657
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_6.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_6
     "P1D"^^xsd:dayTimeDuration
     98
     0.638190559973025
     0.8747541994558314
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_7.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_7
     "P1D"^^xsd:dayTimeDuration
     84
     0.9271417353168782
     0.6106566931325379
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_8.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_8
     "P1D"^^xsd:dayTimeDuration
     81
     0.6371636347285053
     0.6489621573174171
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_9.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_9
     "P1D"^^xsd:dayTimeDuration
     75
     0.7699710306952833
     0.6403617451988984
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.
""").strip()

if not N3_RULES:
    raise RuntimeError("⚠️  You forgot to paste the N3 rules into N3_RULES!")

# ╔═══════════════════════════════════════╗
# ║ 3.  RULE STRUCTURE                    ║
# ╚═══════════════════════════════════════╝
# After parsing each gps:description we store its content
# in a lightweight dataclass so the search engine can use it
# like a normal Python object.

@dataclass(frozen=True)
class TransitionRule:
    """One weighted transition edge in the state space."""
    from_state : str
    to_state   : str
    action     : str
    duration_d : int          # whole days; always 1 in examples
    cost       : float        # euros (or any currency)
    success_p  : float        # ∈ (0,1]
    comfort_p  : float        # ∈ (0,1]
    condition  : Callable[[Patient], bool]  # extra filters

    # Convenience: check if the rule can fire *now* for patient p
    def applies(self, p: Patient, cur_state: str) -> bool:
        return cur_state == self.from_state and self.condition(p)

# ╔═══════════════════════════════════════╗
# ║ 4.  RULE PARSER (very tolerant)       ║
# ╚═══════════════════════════════════════╝
# We extract fields with one big regex.  If your rules evolve,
# just tweak the regular expression.

RULE_RE = re.compile(
    r"""{\s*care:Parkinson\s+gps:description\s+\(\s*
        \{\?patient\s+care:state\s+care:(state_\d+)\.\}\s*         # FROM
        \{\?patient\s+gps:medication\s+medication:(Medication_\d+)\.\}\s*
        \{\?patient\s+care:state\s+care:(state_\d+)\.\}\s*         # TO
        action:([A-Za-z0-9_]+)\s*
        "P(\d+)D"\^\^xsd:dayTimeDuration\s*                       # duration
        (\d+)\s*                                                  # cost
        ([0-9.]+)\s*                                              # success
        ([0-9.]+)\s*                                              # comfort
        \)\s*\}\s*<=\s*\{\s*
        (?P<body>.*?)\}\s*\.\s*""",
    re.S | re.X,
)

GENDER_RE = re.compile(r"care:gender\s+care:(Male|Female)")
MINAGE_RE = re.compile(r"math:greaterThan\s+(\d+)")

def _parse_rules(n3: str) -> List[TransitionRule]:
    """Convert each gps:description {...} <= {...}. into a TransitionRule."""
    rules: List[TransitionRule] = []

    def _make_condition(gender: str | None, min_age: int | None):
        """Return a *callable* that tests a patient object."""
        return lambda p: (gender is None or p.gender == gender) and \
                         (min_age is None or p.age > min_age)

    for m in RULE_RE.finditer(n3):
        from_state, _med, to_state, action, dur, cost, succ, comfort, body = m.groups()

        gender: str | None = None
        min_age: int | None = None

        g = GENDER_RE.search(body)
        if g:
            gender = g.group(1)

        a = MINAGE_RE.search(body)
        if a:
            min_age = int(a.group(1))

        rules.append(
            TransitionRule(
                from_state  = from_state,
                to_state    = to_state,
                action      = action,
                duration_d  = int(dur),
                cost        = float(cost),
                success_p   = float(succ),
                comfort_p   = float(comfort),
                condition   = _make_condition(gender, min_age),
            )
        )

    return rules

# Parse immediately so that a failure is obvious on start-up
RULES = _parse_rules(N3_RULES)
print(f"→ Parsed {len(RULES)} transition rules from the N3 block.\n")

# ╔═══════════════════════════════════════╗
# ║ 5.  SEARCH PARAMETERS (from query)    ║
# ╚═══════════════════════════════════════╝
GOAL_STATE      = "state_6"
MAX_DURATION_D  = 180      # "P180D"  –  180 × 24 × 3600 seconds
MAX_COST        = 500.0
MIN_SUCCESS_P   = 0.35
MIN_COMFORT_P   = 0.35
MAX_STAGECOUNT  = 10       # taken from gps:stagecount limit

# ╔═══════════════════════════════════════╗
# ║ 6.  SEARCH NODE DEFINITION            ║
# ╚═══════════════════════════════════════╝
@dataclass
class Node:
    """One *partial* path in the BFS fringe."""
    state   : str
    duration: int          # total days so far
    cost    : float
    succ    : float
    comfort : float
    path    : List[str]    # ordered list of actions taken

# ╔═══════════════════════════════════════╗
# ║ 7.  LOOP-FREE, NO-PRUNING BFS         ║
# ╚═══════════════════════════════════════╝
def search_all(p: Patient) -> List[Node]:
    """
    Enumerate *every* distinct action sequence (up to MAX_STAGECOUNT)
    that satisfies the four numeric limits and ends in GOAL_STATE.
    """

    start = Node(p.state, duration=0, cost=0.0, succ=1.0, comfort=1.0, path=[])
    queue : Deque[Node] = deque([start])
    sols  : List[Node]  = []

    # visited keeps pairs (state, tuple(action-list)) to prevent
    # infinite A→B→A loops but **never** discards alternative paths
    # that merely differ in actions taken.
    visited: set[Tuple[str, Tuple[str, ...]]] = set()

    while queue:
        cur = queue.popleft()

        if cur.state == GOAL_STATE:
            sols.append(cur)
            # Do NOT 'continue'; longer non-looping paths may exist.

        # stage count –  identical to the N3 gps:stagecount limit
        if len(cur.path) >= MAX_STAGECOUNT:
            continue

        for rule in RULES:
            if not rule.applies(p, cur.state):
                continue

            # --- accumulate metrics ---
            nxt_dur = cur.duration + rule.duration_d
            nxt_cos = cur.cost     + rule.cost
            nxt_suc = cur.succ     * rule.success_p
            nxt_com = cur.comfort  * rule.comfort_p

            # --- global constraint check (same as the N3 query) ---
            if not (nxt_dur <= MAX_DURATION_D and
                    nxt_cos <= MAX_COST and
                    nxt_suc >= MIN_SUCCESS_P and
                    nxt_com >= MIN_COMFORT_P):
                continue

            nxt_path = tuple(cur.path + [rule.action])
            signature = (rule.to_state, nxt_path)

            # if the *exact same* state+action sequence occurred -> loop
            if signature in visited:
                continue
            visited.add(signature)

            queue.append(
                Node(rule.to_state, nxt_dur, nxt_cos,
                     nxt_suc, nxt_com, list(nxt_path))
            )

    return sols

# ╔═══════════════════════════════════════╗
# ║ 8.  PRETTY PRINTING OF RESULTS        ║
# ╚═══════════════════════════════════════╝
def print_solutions(sols: List[Node]) -> None:
    """Human-friendly table of every valid path."""
    if not sols:
        print("⛔  No path satisfies the limits.")
        return

    # sort: 1) highest success, 2) lowest cost, 3) shortest duration
    sols.sort(key=lambda n: (-n.succ, n.cost, n.duration))

    print(f"✅  Found {len(sols)} solution path(s):\n")
    for idx, n in enumerate(sols, 1):
        print(f"Solution #{idx}")
        print(f"  Steps      : {len(n.path)}  (≤ {MAX_STAGECOUNT})")
        print(f"  Duration   : {n.duration} day(s)  (≤ {MAX_DURATION_D})")
        print(f"  Cost       : {n.cost:.2f}          (≤ {MAX_COST})")
        print(f"  Success P  : {n.succ:.3f}          (≥ {MIN_SUCCESS_P})")
        print(f"  Comfort P  : {n.comfort:.3f}       (≥ {MIN_COMFORT_P})")
        for i, act in enumerate(n.path, 1):
            print(f"    {i:2d}. {act}")
        print(f"  Final state: {n.state}\n")

# ──────────────────────────────────────────────────────────────
# 9.  ARC OUTPUT — Answer / Reason why / Check (harness)
# ──────────────────────────────────────────────────────────────

def applicable_from_state(p: Patient, state: str) -> List[TransitionRule]:
    """List all rules that *could* fire from `state` for patient `p` (ignoring global limits)."""
    return [r for r in RULES if r.applies(p, state)]

def replay_totals(p: Patient, actions: List[str]) -> Tuple[int,float,float,float,str]:
    """Recompute totals by replaying actions from p.state."""
    # index transitions by (from_state, action)
    idx = {}
    for r in RULES:
        idx.setdefault((r.from_state, r.action), []).append(r)
    cur = p.state
    dur=0; cost=0.0; succ=1.0; comf=1.0
    for a in actions:
        cand = [r for r in idx.get((cur, a), []) if r.applies(p, cur)]
        if not cand:
            raise RuntimeError(f"Action {a} not applicable from state {cur}")
        r = cand[0]
        cur = r.to_state
        dur += r.duration_d; cost += r.cost; succ *= r.success_p; comf *= r.comfort_p
    return dur, cost, succ, comf, cur

if __name__ == "__main__":
    # ---------------- Answer ----------------
    print("============================================")
    print("GPS (N3 rules) — Answer / Reason why / Check (harness)")
    print("============================================\n")
    print(f"Patient     : {patient_1.name}  (gender={patient_1.gender}, age={patient_1.age})")
    print(f"Start state : {patient_1.state}")
    print(f"Goal state  : {GOAL_STATE}\n")

    solutions = search_all(patient_1)

    print("Answer")
    print("======")
    if not solutions:
        print("No admissible path exists under the limits for this patient.\n")
    else:
        # sort by spec: highest succ, lowest cost, shortest duration
        solutions.sort(key=lambda n: (-n.succ, n.cost, n.duration))
        print_solutions(solutions)
        best = solutions[0]
        print(f"Optimal by (succ↓, cost↑, dur↑): path = {best.path}, "
              f"succ={best.succ:.3f}, cost={best.cost:.2f}, dur={best.duration}\n")

    # ---------------- Reason why ----------------
    print("Reason why")
    print("==========")
    print("Global limits (identical to N3 query):")
    print(f"  duration ≤ {MAX_DURATION_D} days, cost ≤ €{MAX_COST:.0f}, "
          f"success ≥ {MIN_SUCCESS_P}, comfort ≥ {MIN_COMFORT_P}, stagecount ≤ {MAX_STAGECOUNT}\n")

    apps = applicable_from_state(patient_1, patient_1.state)
    print(f"From the start state {patient_1.state}, applicable rules for this patient: {len(apps)}")
    if not apps:
        print("  • None. Every rule with FROM=state_2 requires gender=Male (and sometimes age>18),")
        print("    but the patient is Female, 14. Therefore the search has no outgoing move,")
        print("    and no path can reach the goal.\n")
    else:
        for r in apps:
            print(f"  • {r.action}: {r.from_state} → {r.to_state}  "
                  f"(+{r.duration_d}d, +€{r.cost:.0f}, ×succ {r.success_p:.3f}, ×comf {r.comfort_p:.3f})")
        print()

    # ---------------- Check (harness) ----------------
    print("Check (harness)")
    print("===============")

    # 1) Negative: current patient has no solutions.
    assert len(solutions) == 0, "Expected no solution for Female age 14 in state_2."
    print("• No-solution case confirmed for Jane Doe (Female, 14). ✓")

    # 2) Sanity-positive: a male adult from state_2 *does* have a path to state_6.
    patient_2 = Patient(
        uri="data:patient_2", name="John Doe",
        gender="Male", age=25, weight=80.0,
        diagnosis="49049000", state="state_2"
    )
    sols2 = search_all(patient_2)
    assert len(sols2) >= 1, "Expected at least one path for Male 25 in state_2."
    # sort by spec to identify the best one deterministically
    sols2.sort(key=lambda n: (-n.succ, n.cost, n.duration))
    best2 = sols2[0]
    dur, cost, succ, comf, end = replay_totals(patient_2, best2.path)

    assert end == GOAL_STATE, "Best path should reach the goal."
    assert dur <= MAX_DURATION_D and cost <= MAX_COST and succ >= MIN_SUCCESS_P and comf >= MIN_COMFORT_P
    # The direct rule state_2→state_6 exists for Male (Medication_5).
    assert any(("take_pill_Medication_5" in n.path and n.state == GOAL_STATE) for n in sols2), \
        "Expected a direct transition via take_pill_Medication_5 to state_6."

    print(f"• Positive sanity: {patient_2.name} has {len(sols2)} solution(s). "
          f"Best path {best2.path} reaches {end} with "
          f"succ={succ:.3f}, cost=€{cost:.0f}, dur={dur}d, comf={comf:.3f}. ✓")

    print("\nHarness complete: parsing, applicability, limits, and search behavior verified. ✓")

