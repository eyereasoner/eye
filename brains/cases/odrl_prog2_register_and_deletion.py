
#!/usr/bin/env python3
"""
P3 — Program 2: Processing Register & Deletion Schedule (ODRL-style)

Short story (header):
  The data steward prepares the register. Using the permitted list from Program 1,
  they compute per-request deletion dates and aggregate counts for planning.
"""
from __future__ import annotations

import argparse
import datetime as dt
import json
from typing import Dict, Any, List
from collections import Counter

### LOGIC
# Core logic: produce register rows + aggregates from permitted entries.
def compute_register(permitted_json: Dict[str, Any], asof: dt.date) -> Dict[str, Any]:
    per = permitted_json["permitted"]
    rows: List[Dict[str, Any]] = []
    for e in per:
        rdays = int(e["retention_days_effective"])
        deletion_date = asof + dt.timedelta(days=rdays)
        rows.append({
            "id": e["id"],
            "name": e["name"],
            "purpose": e["purpose"],
            "region": e["region"],
            "lawful_basis": e["lawful_basis"],
            "approved_fields": e["approved_fields"],
            "retention_days": rdays,
            "deletion_date": deletion_date.isoformat(),
        })
    by_purpose = Counter(e["purpose"] for e in rows)
    by_region = Counter(e["region"] for e in rows)

    reasons = [
        "Deletion date formula: as_of + retention_days_effective (from Program 1, already ≤ purpose cap).",
        "Register aggregates: simple counts by purpose and by region."
    ]
    if rows:
        reasons.append(f"Trace: {rows[0]['id']} → retention={rows[0]['retention_days']} days → deletion={rows[0]['deletion_date']}.")

    return {
        "as_of": asof.isoformat(),
        "rows": rows,
        "aggregates": {
            "by_purpose": dict(by_purpose),
            "by_region": dict(by_region)
        },
        "reference_date_from_p1": permitted_json.get("reference_date","N/A"),
        "reasons": reasons
    }

### CHECK
# Test harness: verifies invariants and prints detailed diagnostics.
def run_harness(permitted_json: Dict[str, Any], register: Dict[str, Any], asof: dt.date) -> None:
    permitted_ids = {e["id"] for e in permitted_json["permitted"]}

    print("Check 1 — Every row originates from Program 1 permitted set:")
    for row in register["rows"]:
        origin_ok = row["id"] in permitted_ids
        print(f"  - {row['id']} origin_ok={origin_ok}")
        assert origin_ok

    print("Check 2 — Deletion dates equal as_of + retention_days:")
    for row in register["rows"]:
        expected = (asof + dt.timedelta(days=int(row["retention_days"]))).isoformat()
        print(f"  - {row['id']} deletion={row['deletion_date']} expected={expected}")
        assert row["deletion_date"] == expected

    print("Check 3 — Aggregates match sums of rows:")
    by_purpose = Counter(r["purpose"] for r in register["rows"])
    by_region = Counter(r["region"] for r in register["rows"])
    print(f"  - by_purpose computed={dict(by_purpose)} vs reported={register['aggregates']['by_purpose']}")
    print(f"  - by_region  computed={dict(by_region)} vs reported={register['aggregates']['by_region']}")
    assert dict(by_purpose) == register["aggregates"]["by_purpose"]
    assert dict(by_region) == register["aggregates"]["by_region"]

    print("Check 4 — Schema essentials:")
    for field in ("rows","aggregates","as_of"):
        print(f"  - field '{field}':", "present" if field in register else "MISSING")
        assert field in register

def main():
    ### DATA — input from Program 1
    parser = argparse.ArgumentParser(description="Build processing register & deletion schedule from permitted set.")
    parser.add_argument("--in", dest="infile", default="./cases/bus/odrl/permitted_processing.json",
                        help="Input JSON path from Program 1")
    parser.add_argument("--asof", default="2025-09-18", help="Reference date (YYYY-MM-DD)")
    args = parser.parse_args()

    with open(args.infile, "r", encoding="utf-8") as f:
        permitted_json = json.load(f)

    asof = dt.date.fromisoformat(args.asof)
    register = compute_register(permitted_json, asof)

    print("# ANSWER")
    print(json.dumps({"aggregates": register["aggregates"], "rows": register["rows"]}, indent=2, ensure_ascii=False))

    print("\n# REASONS")
    for r in register["reasons"]:
        print("-", r)

    print("\n# CHECK (harness) — detailed")
    run_harness(permitted_json, register, asof)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()
