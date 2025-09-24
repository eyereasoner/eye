#!/usr/bin/env python3
"""
P3 Case: "Having Lunch with a Colleague"

This single file follows the “Answer · Reason Why · Check (harness)” pattern.
It embeds:
  • RDF/Turtle facts (DATA) — small domain snapshot
  • A plain-English LOGIC description (below)
  • A Python Driver that implements the logic
  • A descriptive test harness with multiple named scenarios

───────────────────────────────────────────────────────────────────────────────
LOGIC
───────────────────────────────────────────────────────────────────────────────
Goal
  Decide whether two colleagues can have lunch together today, and if so,
  recommend a restaurant and start time.

Inputs
  • People: name, diet {vegetarian|omnivore}, budget (€), lunch-break start/end
  • Restaurants: menu tags, price per person, walk/wait/service minutes,
                 opening hours

Derived/Assumptions
  • Shared lunch window = intersection of the two lunch breaks.
  • Total time at a restaurant = 2*walk + wait + service (walk both ways).

Feasibility rules
  1) Overlap required: if there’s no shared window, result is infeasible.
  2) Candidate filter for each restaurant R:
       a) Price: R.price ≤ min(budget₁, budget₂).
       b) Hard limits: walk ≤ 10 and wait ≤ 10 (inclusive).
       c) Diet coverage:
            - vegetarian person requires restaurant to have “vegetarian” OR “vegan”.
            - omnivore has no special requirement.
       d) Timing fit:
            - Choose the earliest feasible start S ≥ max(shared_window_start, R.open_start)
            - The meal interval [S, S + total_time] must finish by both
              shared_window_end and R.open_end.

Choice rule (preference)
  • Among all candidates that fit, choose the one with minimal total_time.
  • Tie-breaks: earlier start S, then restaurant name (alphabetical).

Answer content
  • “Yes/No” + recommended place/time (if any)
  • Context: participants, diets/budgets, shared window, constraints, objective
  • Reason Why: accept/reject trace for each restaurant, plus winner rationale.

Harness
  • Multiple named scenarios, clear expectations printed, with PASS/FAIL.
  • Covers price/budget edges, time-window edges, diet coverage, opening hours,
    tie-breakers, and equality boundaries on hard limits.

"""

# -------------------------
# Embedded RDF/Turtle (DATA)
# -------------------------
RDF_TURTLE = r"""@prefix ex:  <http://example.org/lunch#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:Alice a ex:Person ;
  ex:colleagueOf ex:Bob ;
  ex:hasDiet ex:vegetarian ;
  ex:hasBudget "20.00"^^xsd:decimal ;
  ex:hasBreak ex:LB1 .

ex:Bob a ex:Person ;
  ex:colleagueOf ex:Alice ;
  ex:hasDiet ex:omnivore ;
  ex:hasBudget "18.00"^^xsd:decimal ;
  ex:hasBreak ex:LB2 .

ex:LB1 a ex:LunchBreak ;
  ex:windowStart "12:00:00"^^xsd:time ;
  ex:windowEnd   "13:00:00"^^xsd:time .

ex:LB2 a ex:LunchBreak ;
  ex:windowStart "12:15:00"^^xsd:time ;
  ex:windowEnd   "13:15:00"^^xsd:time .

ex:GreenBowl a ex:Restaurant ;
  ex:hasMenuTag ex:vegetarian , ex:vegan ;
  ex:pricePerPerson "14.50"^^xsd:decimal ;
  ex:walkMinutes "6"^^xsd:int ;
  ex:waitMinutes "5"^^xsd:int ;
  ex:serviceMinutes "25"^^xsd:int ;
  ex:openStart "11:30:00"^^xsd:time ;
  ex:openEnd   "14:30:00"^^xsd:time .

ex:PastaPlace a ex:Restaurant ;
  ex:hasMenuTag ex:omnivore , ex:vegetarian ;
  ex:pricePerPerson "17.00"^^xsd:decimal ;
  ex:walkMinutes "8"^^xsd:int ;
  ex:waitMinutes "12"^^xsd:int ;
  ex:serviceMinutes "30"^^xsd:int ;
  ex:openStart "12:00:00"^^xsd:time ;
  ex:openEnd   "14:00:00"^^xsd:time .

ex:SushiGo a ex:Restaurant ;
  ex:hasMenuTag ex:fish , ex:omnivore ;
  ex:pricePerPerson "22.00"^^xsd:decimal ;
  ex:walkMinutes "4"^^xsd:int ;
  ex:waitMinutes "8"^^xsd:int ;
  ex:serviceMinutes "30"^^xsd:int ;
  ex:openStart "11:45:00"^^xsd:time ;
  ex:openEnd   "14:30:00"^^xsd:time .
"""

# -------------
# Python Driver
# -------------
from dataclasses import dataclass
from typing import List, Dict, Optional, Tuple


# ---------- Utilities
def tmin(hhmm: str) -> int:
    """Return minutes since 00:00 for 'HH:MM' (or 'HH:MM:SS', seconds ignored)."""
    parts = hhmm.split(":")
    hh, mm = int(parts[0]), int(parts[1])
    return hh * 60 + mm


def hhmm(minutes: int) -> str:
    """Format minutes since midnight as 'HH:MM'."""
    h, m = divmod(minutes, 60)
    return f"{h:02d}:{m:02d}"


# ---------- Domain classes
@dataclass(frozen=True)
class Person:
    name: str
    diet: str           # "vegetarian" or "omnivore"
    budget_eur: float
    break_start: str    # "HH:MM"
    break_end: str      # "HH:MM"


@dataclass(frozen=True)
class Restaurant:
    iri: str
    name: str
    menu_tags: List[str]      # e.g., ["vegetarian","vegan"]
    price: float              # per person
    walk_min: int
    wait_min: int
    service_min: int
    open_start: str           # "HH:MM"
    open_end: str             # "HH:MM"


# ---------- Logic impl (matches spec in docstring)
def overlap_window(p1: Person, p2: Person) -> Optional[Tuple[int, int]]:
    s = max(tmin(p1.break_start), tmin(p2.break_start))
    e = min(tmin(p1.break_end),   tmin(p2.break_end))
    return (s, e) if s < e else None


def covers_diet(r: Restaurant, person: Person) -> bool:
    if person.diet == "vegetarian":
        return ("vegetarian" in r.menu_tags) or ("vegan" in r.menu_tags)
    return True  # omnivore


def hard_limits_ok(r: Restaurant) -> bool:
    return r.walk_min <= 10 and r.wait_min <= 10  # inclusive


def total_time_cost(r: Restaurant) -> int:
    return 2 * r.walk_min + r.wait_min + r.service_min


def pick_recommendation(p1: Person, p2: Person, restaurants: List[Restaurant]) -> Dict:
    reason_lines: List[str] = []
    context_lines: List[str] = []

    # Context: who, budgets/diets, breaks
    context_lines.append(f"Participants: {p1.name} ({p1.diet}, €{p1.budget_eur:.2f}) "
                         f"& {p2.name} ({p2.diet}, €{p2.budget_eur:.2f}).")
    context_lines.append(f"Breaks: {p1.name} {p1.break_start}–{p1.break_end}, "
                         f"{p2.name} {p2.break_start}–{p2.break_end}.")
    context_lines.append("Constraints: price ≤ min(budgets); walk ≤ 10; wait ≤ 10; "
                         "diet coverage required; fit within shared window AND opening hours.")
    context_lines.append("Objective: minimize total time = 2*walk + wait + service; "
                         "tie-break by earlier start, then by name.")

    ow = overlap_window(p1, p2)
    if not ow:
        return {
            "feasible": False,
            "answer": "No — lunch together is not feasible (no overlapping lunch break).",
            "context": "\n".join(context_lines + ["Shared window: none (no overlap)."]),
            "reason": "No shared window means no plan can fit.",
            "decision": {},
            "check": {}
        }

    start, end = ow
    overlap_len = end - start
    context_lines.append(f"Shared window: {hhmm(start)}–{hhmm(end)} ({overlap_len} min).")

    budget_min = min(p1.budget_eur, p2.budget_eur)
    context_lines.append(f"Shared budget ceiling: €{budget_min:.2f}.")

    candidates = []
    for r in restaurants:
        facts = []
        # Price
        if r.price <= budget_min:
            facts.append(f"price €{r.price:.2f} ≤ €{budget_min:.2f}")
        else:
            reason_lines.append(f"✗ {r.name}: price €{r.price:.2f} exceeds shared budget €{budget_min:.2f}.")
            continue

        # Hard limits
        if not hard_limits_ok(r):
            reason_lines.append(
                f"✗ {r.name}: hard limits violated (walk {r.walk_min} min / wait {r.wait_min} min; limits are 10/10)."
            )
            continue
        facts.append(f"walk {r.walk_min} min, wait {r.wait_min} min within limits")

        # Diet coverage
        if not (covers_diet(r, p1) and covers_diet(r, p2)):
            reason_lines.append(f"✗ {r.name}: menu does not cover both diets.")
            continue
        facts.append("menu covers diets")

        # Time fit
        t_cost = total_time_cost(r)
        if t_cost > overlap_len:
            reason_lines.append(
                f"✗ {r.name}: total time {t_cost} min > shared window {overlap_len} min."
            )
            continue

        earliest_start = max(start, tmin(r.open_start))
        latest_finish_allowed = min(end, tmin(r.open_end))
        if earliest_start + t_cost > latest_finish_allowed:
            reason_lines.append(
                f"✗ {r.name}: opening hours {r.open_start}–{r.open_end} cannot fit {t_cost} min within "
                f"{hhmm(start)}–{hhmm(end)}."
            )
            continue

        facts.append(
            f"open {r.open_start}–{r.open_end}; earliest start {hhmm(earliest_start)}; "
            f"total {t_cost} min fits"
        )
        candidates.append((t_cost, earliest_start, r, "; ".join(facts)))

    if not candidates:
        return {
            "feasible": False,
            "answer": "No — lunch together is not feasible (no restaurant meets all constraints).",
            "context": "\n".join(context_lines),
            "reason": "\n".join(reason_lines),
            "decision": {},
            "check": {}
        }

    # Choose per preference / tie-breakers
    candidates.sort(key=lambda x: (x[0], x[1], x[2].name))
    best_cost, best_start, best_r, best_facts = candidates[0]
    finish = best_start + best_cost

    decision = {
        "restaurant": best_r.name,
        "start": best_start,
        "finish": finish,
        "walk": best_r.walk_min,
        "wait": best_r.wait_min,
        "service": best_r.service_min,
        "total": best_cost,
        "price": best_r.price,
        "opening": (best_r.open_start, best_r.open_end),
        "facts": best_facts,
    }

    answer = (f"Yes — {best_r.name} at {hhmm(best_start)} "
              f"(finish by {hhmm(finish)}; total {best_cost} min; €{best_r.price:.2f}/person).")

    reason_lines.append(
        f"✓ Recommended: {best_r.name} — minimal total time {best_cost} min; "
        f"start {hhmm(best_start)}; {best_facts}."
    )

    return {
        "feasible": True,
        "answer": answer,
        "context": "\n".join(context_lines),
        "reason": "\n".join(reason_lines),
        "decision": decision,
        "check": {
            "recommended": best_r.name,
            "start_minutes": best_start,
            "finish_minutes": finish,
            "total_minutes": best_cost,
            "price_ok": decision["price"] <= budget_min,
            "fits_window": best_start >= start and finish <= end,
            "fits_opening": best_start >= tmin(best_r.open_start) and finish <= tmin(best_r.open_end),
        }
    }


# ---------- Base inputs
def build_case_inputs() -> Tuple[Person, Person, List[Restaurant]]:
    alice = Person("Alice", "vegetarian", 20.00, "12:00", "13:00")
    bob   = Person("Bob",   "omnivore",   18.00, "12:15", "13:15")

    restaurants = [
        Restaurant("ex:GreenBowl", "Green Bowl",
                   ["vegetarian", "vegan"], 14.50, 6, 5, 25, "11:30", "14:30"),
        Restaurant("ex:PastaPlace", "Pasta Place",
                   ["omnivore", "vegetarian"], 17.00, 8, 12, 30, "12:00", "14:00"),
        Restaurant("ex:SushiGo", "Sushi Go",
                   ["fish", "omnivore"], 22.00, 4, 8, 30, "11:45", "14:30"),
    ]
    return alice, bob, restaurants


# ---------- Descriptive harness
def run_harness() -> None:
    print("\n# Check (harness) — descriptive scenarios")
    failures = []

    def check(name: str,
              result: Dict,
              expect_feasible: bool,
              expect_restaurant: Optional[str] = None,
              predicates: Optional[List[Tuple[str, callable]]] = None) -> None:
        """
        predicates: list of (label, predicate_callable) pairs.
        Each predicate is a zero-arg callable that returns True on success.
        """
        print(f"\n— Scenario: {name}")
        print("  Expected:",
              "FEASIBLE" if expect_feasible else "INFEASIBLE",
              f"| Restaurant: {expect_restaurant or 'n/a'}")
        print("  Observed:", "FEASIBLE" if result.get("feasible") else "INFEASIBLE")
        if result.get("feasible"):
            d = result["decision"]
            print(f"  ⇒ Choice: {d['restaurant']} at {hhmm(d['start'])} "
                  f"(finish {hhmm(d['finish'])}; total {d['total']} min)")
        else:
            print("  ⇒ No recommendation")

        try:
            assert result.get("feasible") == expect_feasible, "feasibility mismatch"
            if expect_feasible and expect_restaurant:
                assert result["decision"]["restaurant"] == expect_restaurant, "wrong restaurant"
            if predicates:
                for label, pred in predicates:
                    try:
                        ok = bool(pred())
                    except Exception as e:
                        raise AssertionError(f"{label} raised {e.__class__.__name__}: {e}")
                    assert ok, f"assertion failed: {label}"
            print("  Result: PASS")
        except AssertionError as e:
            print("  Result: FAIL —", e)
            failures.append((name, str(e)))

    # Base case
    alice, bob, base_rs = build_case_inputs()
    base = pick_recommendation(alice, bob, base_rs)
    # Shared window 12:15–13:00 → 45 min; Green Bowl total 42 min; start 12:15; finish 12:57
    check(
        "Base case → should recommend Green Bowl",
        base,
        expect_feasible=True,
        expect_restaurant="Green Bowl",
        predicates=[
            ("fits_window",        lambda res=base: res['check']['fits_window']),
            ("fits_opening",       lambda res=base: res['check']['fits_opening']),
            ("price_ok",           lambda res=base: res['check']['price_ok']),
            ("total == 42",        lambda res=base: res['decision']['total'] == 42),
            ("start == 12:15",     lambda res=base: hhmm(res['decision']['start']) == '12:15'),
            ("finish == 12:57",    lambda res=base: hhmm(res['decision']['finish']) == '12:57'),
        ],
    )

    # Budget too tight → infeasible
    tight_alice = Person("Alice", "vegetarian", 12.00, "12:00", "13:00")
    r1 = pick_recommendation(tight_alice, bob, base_rs)
    check("Budget tightened to €12 → infeasible", r1, expect_feasible=False)

    # Shrink overlap window → infeasible
    short_bob = Person("Bob", "omnivore", 18.00, "12:40", "13:00")
    r2 = pick_recommendation(alice, short_bob, base_rs)
    check("Overlap shrunk to 20 min → infeasible", r2, expect_feasible=False)

    # Diet coverage fails for an otherwise fast/cheap place
    quick_only_omnivore = Restaurant(
        "ex:Quick", "Quick Bites", ["omnivore"], 9.50, 5, 5, 15, "11:30", "14:30"
    )
    r3 = pick_recommendation(alice, bob, base_rs + [quick_only_omnivore])
    check("Add 'Quick Bites' (no veg) → still not chosen",
          r3, expect_feasible=True, expect_restaurant="Green Bowl")

    # Hard limits boundary equality (walk=10, wait=10) should be allowed
    edge_ok = Restaurant(
        "ex:EdgeDiner", "Edge Diner", ["vegetarian", "omnivore"],
        10.00, 10, 10, 0, "11:30", "14:30"
    )
    r4 = pick_recommendation(alice, bob, base_rs + [edge_ok])
    # Edge Diner total = 20 + 10 + 0 = 30 min; earlier start same 12:15 → Edge Diner should win (30 < 42)
    check("Equality on hard limits; faster total → new winner",
          r4, expect_feasible=True, expect_restaurant="Edge Diner",
          predicates=[
              ("total == 30",     lambda res=r4: res['decision']['total'] == 30),
              ("start == 12:15",  lambda res=r4: hhmm(res['decision']['start']) == '12:15'),
          ])

    # Tie on total_time → earlier start wins
    tiebreak_A = Restaurant("ex:TieA", "Tie A", ["vegetarian"], 10.00, 5, 5, 20, "11:30", "14:30")  # total 35
    tiebreak_B = Restaurant("ex:TieB", "Tie B", ["vegetarian"], 10.00, 5, 5, 20, "12:30", "14:30")  # total 35 but opens later
    r5 = pick_recommendation(alice, bob, [tiebreak_A, tiebreak_B])
    check("Tie on total; earlier start breaks tie", r5, expect_feasible=True, expect_restaurant="Tie A")

    # Tie on total AND start → alphabetical by name
    tiebreak_C1 = Restaurant("ex:TieC1", "Cafe One", ["vegetarian"], 10.00, 5, 5, 20, "11:30", "14:30")  # total 35
    tiebreak_C2 = Restaurant("ex:TieC2", "Cafe Two", ["vegetarian"], 10.00, 5, 5, 20, "11:30", "14:30")  # total 35
    r6 = pick_recommendation(alice, bob, [tiebreak_C2, tiebreak_C1])
    check("Tie on total & start; alphabetical by name", r6, expect_feasible=True, expect_restaurant="Cafe One")

    # Opening-hours boundary: finish exactly at closing/window end is allowed
    boundary_ok = Restaurant(
        "ex:Boundary", "Boundary Cafe", ["vegetarian"], 10.00, 6, 9, 24, "12:15", "13:00"
    )
    # total = 12 + 9 + 24 = 45; shared window 12:15–13:00 = 45; earliest start 12:15; finish 13:00 → valid
    r7 = pick_recommendation(alice, bob, [boundary_ok])
    check("Finish exactly at window & closing time", r7, expect_feasible=True, expect_restaurant="Boundary Cafe",
          predicates=[
              ("total == 45",     lambda res=r7: res['decision']['total'] == 45),
              ("finish == 13:00", lambda res=r7: hhmm(res['decision']['finish']) == '13:00'),
          ])

    if failures:
        print("\nHarness summary: FAILURES")
        for name, msg in failures:
            print(f"  • {name}: {msg}")
        raise SystemExit(1)
    else:
        print("\nHarness summary: PASS (all scenarios)")


# ---------- Main
def main():
    alice, bob, restaurants = build_case_inputs()
    result = pick_recommendation(alice, bob, restaurants)

    print("# Answer")
    print(result["answer"])

    print("\n—— Context ——")
    print(result["context"])

    print("\n# Reason Why")
    print(result["reason"])

    # Run descriptive harness
    run_harness()


if __name__ == "__main__":
    main()

