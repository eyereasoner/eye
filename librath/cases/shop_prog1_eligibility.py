
#!/usr/bin/env python3
"""
P3 — Program 1: Senior Discount Eligibility

Short story (header):
  In a small shop, a clerk prepares the day’s ledger. A note reads:
  “Apply senior discount for customers aged 65 or older.” The task is
  to determine who meets the rule and record the result clearly.
"""
from __future__ import annotations

import argparse
import dataclasses
import datetime as dt
import json
from typing import List, Dict, Any

### DATA
# -----------------------------
# Data (embedded for the demo)
# -----------------------------
@dataclasses.dataclass(frozen=True)
class Person:
    id: str
    name: str
    birthdate: dt.date

DATASET: List[Person] = [
    Person("p1", "Alice Dupont", dt.date(1950, 6, 15)),
    Person("p2", "Benoît Leroy", dt.date(1960, 12, 1)),
    Person("p3", "Chiara Rossi", dt.date(1965, 9, 18)),
    Person("p4", "Daan Vermeulen", dt.date(1940, 2, 29)),
    Person("p5", "Eva Janssens", dt.date(2000, 7, 30)),
    Person("p6", "Farid El Amrani", dt.date(1959, 9, 17)),
]

### LOGIC
# -----------------------------
# Logic (policy)
# -----------------------------
SENIOR_AGE = 65

def compute_age(asof: dt.date, dob: dt.date) -> int:
    years = asof.year - dob.year
    before_birthday = (asof.month, asof.day) < (dob.month, dob.day)
    return years - int(before_birthday)

def compute_age_alt(asof: dt.date, dob: dt.date) -> int:
    years = asof.year - dob.year
    try:
        last_birthday = dob.replace(year=asof.year)
    except ValueError:
        last_birthday = dt.date(asof.year, 2, 28)
    if asof < last_birthday:
        years -= 1
    return years

def is_senior(asof: dt.date, dob: dt.date) -> bool:
    return compute_age(asof, dob) >= SENIOR_AGE

# Primary driver: produces the Answer JSON used by downstream steps.
def solve(asof: dt.date) -> Dict[str, Any]:
    eligible, ineligible, reasons = [], [], []
    for person in DATASET:
        age = compute_age(asof, person.birthdate)
        ok = age >= SENIOR_AGE
        (eligible if ok else ineligible).append({"id": person.id, "name": person.name, "age": age})
        reasons.append(
            f"{person.name} (born {person.birthdate.isoformat()}) is {age} on "
            f"{asof.isoformat()} → {'eligible' if ok else 'not eligible'} by policy age ≥ {SENIOR_AGE}."
        )
    return {
        "reference_date": asof.isoformat(),
        "policy": {"senior_age": SENIOR_AGE},
        "eligible": sorted(eligible, key=lambda x: x["name"]),
        "ineligible": sorted(ineligible, key=lambda x: x["name"]),
        "reasons": reasons,
    }

### CHECK
# -----------------------------
# Check (harness)
# -----------------------------
# Test harness: verifies invariants and prints detailed diagnostics.
def run_harness(asof: dt.date, result: Dict[str, Any]) -> None:
    print("Check 1 — Dual age methods agree per person:")
    for p in DATASET:
        a = compute_age(asof, p.birthdate)
        b = compute_age_alt(asof, p.birthdate)
        print(f"  - {p.id} {p.name}: primary={a}, alt={b}")
        assert a == b

    print("Check 2 — Policy consistency on eligible/ineligible partitions:")
    ids_eligible = {e["id"] for e in result["eligible"]}
    ids_ineligible = {e["id"] for e in result["ineligible"]}
    print(f"  - Eligible IDs:   {sorted(ids_eligible)}")
    print(f"  - Ineligible IDs: {sorted(ids_ineligible)}")
    assert ids_eligible.isdisjoint(ids_ineligible)

    by_id = {p.id: p for p in DATASET}
    for e in result["eligible"]:
        calc_age = compute_age(asof, by_id[e["id"]].birthdate)
        print(f"  - {e['id']} must be ≥ {SENIOR_AGE}: computed age={calc_age}")
        assert calc_age >= SENIOR_AGE
    for e in result["ineligible"]:
        calc_age = compute_age(asof, by_id[e["id"]].birthdate)
        print(f"  - {e['id']} must be < {SENIOR_AGE}: computed age={calc_age}")
        assert calc_age < SENIOR_AGE

    print("Check 3 — Leap-day handling parity:")
    p4 = next(p for p in DATASET if p.id == "p4")
    assert compute_age(asof, p4.birthdate) == compute_age_alt(asof, p4.birthdate)

    print("Check 4 — Output schema contains required fields:")
    for field in ("reference_date", "policy", "eligible"):
        print(f"  - field '{field}':", "present" if field in result else "MISSING")
        assert field in result
    for e in result["eligible"]:
        for field in ("id", "name", "age"):
            assert field in e

def main():
    parser = argparse.ArgumentParser(description="Compute senior discount eligibility.")
    parser.add_argument("--out", default="./cases/bus/shop/eligible_customers.json", help="Output JSON path")
    parser.add_argument("--asof", default="2025-09-18", help="Reference date (YYYY-MM-DD)")
    args = parser.parse_args()

    asof = dt.date.fromisoformat(args.asof)
    result = solve(asof)

    print("# ANSWER")
    print(json.dumps({"eligible": result["eligible"]}, indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in result["reasons"]:
        print("-", r)

    with open(args.out, "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2, ensure_ascii=False)
    print(f"\nWrote {args.out}")

    print("\n# CHECK (harness) — detailed")
    run_harness(asof, result)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()
