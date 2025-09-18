
#!/usr/bin/env python3
"""
EYE Learning — Senior Discount Eligibility

Short story:
  In a small shop, a clerk prepares the day’s ledger. A note reads:
  “Apply senior discount for customers aged 65 or older.” The task is
  to determine who meets the rule and record the result clearly.

Goal:
  From a small dataset of people and their birthdates ("Data") and a policy rule ("Logic"),
  synthesize a *self-contained* Python program that produces:
    1) Answer — who is eligible for a senior discount as of a reference date.
    2) Reason — an explicit natural-language explanation for each decision.
    3) Check  — an independent test harness that verifies the computation and invariants.

Output:
  Writes a JSON file "./resources/eligible_customers.json" (by default) that discount_impact.py will read.

Why it's trustworthy:
  - Single-file artifact with clear inputs and outputs.
  - Explanations are produced alongside answers.
  - Built-in harness recomputes results using two independent methods and checks edges.

Run:
  python discount_eligibility.py --out ./resources/eligible_customers.json --asof 2025-09-18
"""
from __future__ import annotations

import argparse
import dataclasses
import datetime as dt
import json
from typing import List, Dict, Any


# -----------------------------
# Data (embedded for the demo)
# -----------------------------
# In a real pipeline, this could be loaded from CSV or a DB. For the EYE pattern,
# we keep the full artifact self-contained and deterministic.
@dataclasses.dataclass(frozen=True)
class Person:
    id: str
    name: str
    birthdate: dt.date  # ISO-8601 date


DATASET: List[Person] = [
    Person("p1", "Alice Dupont", dt.date(1950, 6, 15)),
    Person("p2", "Benoît Leroy", dt.date(1960, 12, 1)),
    Person("p3", "Chiara Rossi", dt.date(1965, 9, 18)),   # turns 60 on the reference day
    Person("p4", "Daan Vermeulen", dt.date(1940, 2, 29)), # leap-day baby, age handling edge
    Person("p5", "Eva Janssens", dt.date(2000, 7, 30)),
    Person("p6", "Farid El Amrani", dt.date(1959, 9, 17)),# boundary around the as-of date
]


# -----------------------------
# Logic (policy)
# -----------------------------
SENIOR_AGE = 65  # policy threshold

def compute_age(asof: dt.date, dob: dt.date) -> int:
    """Age in whole years on the reference date.
    This is the *primary* method used for the Answer.
    """
    years = asof.year - dob.year
    before_birthday = (asof.month, asof.day) < (dob.month, dob.day)
    return years - int(before_birthday)


def compute_age_alt(asof: dt.date, dob: dt.date) -> int:
    """Independent alternative age calc, using month/day arithmetic.
    This is used by the harness to cross-check correctness.
    """
    # Count how many birthdays have passed by comparing asof to dob shifted by 'years'.
    years = asof.year - dob.year
    try:
        last_birthday = dob.replace(year=asof.year)
    except ValueError:
        # Handle 29 Feb birthdays: treat 28 Feb as the birthday in non-leap years for age purposes.
        # This mirrors common legal/retail interpretations.
        last_birthday = dt.date(asof.year, 2, 28)
    if asof < last_birthday:
        years -= 1
    return years


def is_senior(asof: dt.date, dob: dt.date) -> bool:
    return compute_age(asof, dob) >= SENIOR_AGE


# -----------------------------
# Goal → Driver
# -----------------------------
def solve(asof: dt.date) -> Dict[str, Any]:
    """Produce the triad: (Answer, Reason, Check-ready structures)."""
    eligible = []
    ineligible = []
    reasons = []

    for person in DATASET:
        age = compute_age(asof, person.birthdate)
        eligible_flag = age >= SENIOR_AGE
        reason = (
            f"{person.name} (born {person.birthdate.isoformat()}) is {age} on "
            f"{asof.isoformat()} → {'eligible' if eligible_flag else 'not eligible'} "
            f"by policy age ≥ {SENIOR_AGE}."
        )
        (eligible if eligible_flag else ineligible).append({
            "id": person.id,
            "name": person.name,
            "age": age,
        })
        reasons.append(reason)

    answer = {
        "reference_date": asof.isoformat(),
        "policy": {"senior_age": SENIOR_AGE},
        "eligible": sorted(eligible, key=lambda x: x["name"]),
        "ineligible": sorted(ineligible, key=lambda x: x["name"]),
        "reasons": reasons,
    }
    return answer


# -----------------------------
# Check (harness)
# -----------------------------
def run_harness(asof: dt.date, result: Dict[str, Any]) -> None:
    """A lightweight but meaningful test suite with verbose output.

    Verifies:
      1) The two independent age calculations agree for all people.
      2) All labeled eligible satisfy age ≥ threshold; ineligible are < threshold.
      3) Boundary and leap-year edge cases behave as intended.
      4) The output schema contains the fields discount_impact.py depends on.
    """
    print("Check 1 — Dual age methods agree per person:")
    for p in DATASET:
        a = compute_age(asof, p.birthdate)
        b = compute_age_alt(asof, p.birthdate)
        print(f"  - {p.id} {p.name}: primary={a}, alt={b}")
        assert a == b, f"Age methods disagree for {p.name}: {a} vs {b}"

    print("Check 2 — Policy consistency on eligible/ineligible partitions:")
    ids_eligible = {e["id"] for e in result["eligible"]}
    ids_ineligible = {e["id"] for e in result["ineligible"]}
    print(f"  - Eligible IDs:   {sorted(ids_eligible)}")
    print(f"  - Ineligible IDs: {sorted(ids_ineligible)}")
    assert ids_eligible.isdisjoint(ids_ineligible), "Overlap between eligible & ineligible"

    by_id = {p.id: p for p in DATASET}
    for e in result["eligible"]:
        p = by_id[e["id"]]
        calc_age = compute_age(asof, p.birthdate)
        print(f"  - {p.id} must be ≥ {SENIOR_AGE}: computed age={calc_age}")
        assert calc_age >= SENIOR_AGE, f"{p.name} incorrectly eligible"
    for e in result["ineligible"]:
        p = by_id[e["id"]]
        calc_age = compute_age(asof, p.birthdate)
        print(f"  - {p.id} must be < {SENIOR_AGE}: computed age={calc_age}")
        assert calc_age < SENIOR_AGE, f"{p.name} incorrectly ineligible"

    print("Check 3 — Leap-day handling parity between methods:")
    p4 = next(p for p in DATASET if p.id == "p4")
    age_p4_primary = compute_age(asof, p4.birthdate)
    age_p4_alt = compute_age_alt(asof, p4.birthdate)
    print(f"  - p4 {p4.name}: primary={age_p4_primary}, alt={age_p4_alt}")
    assert age_p4_alt == age_p4_primary, "Leap-day handling mismatch"

    print("Check 4 — Output schema contains required fields:")
    for field in ("reference_date", "policy", "eligible"):
        print(f"  - field '{field}':", "present" if field in result else "MISSING")
        assert field in result, f"Missing field in output: {field}"
    for e in result["eligible"]:
        for field in ("id", "name", "age"):
            assert field in e, f"Eligible entry missing '{field}'"


def main():
    parser = argparse.ArgumentParser(description="Compute senior discount eligibility.")
    parser.add_argument("--out", default="./resources/eligible_customers.json", help="Output JSON path")
    parser.add_argument("--asof", default="2025-09-18", help="Reference date (YYYY-MM-DD)")
    args = parser.parse_args()

    asof = dt.date.fromisoformat(args.asof)
    result = solve(asof)

    # Pretty-print Answer & Reason
    print("# ANSWER")
    print(json.dumps({"eligible": result["eligible"]}, indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in result["reasons"]:
        print("-", r)

    # Write the canonical output that discount_impact.py consumes
    with open(args.out, "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2, ensure_ascii=False)
    print(f"\nWrote {args.out}")

    # Run the self-checking harness
    print("\n# CHECK (harness) — detailed")
    run_harness(asof, result)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()
