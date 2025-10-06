
#!/usr/bin/env python3
"""
P3 — Program 3: Individual Payment Schedule & Compliance Checklist

Short story (header):
  Individuals need clarity: when will payments arrive and what must they do weekly?
  The task is to outline a week-by-week schedule and the compliance checklist.

Overview:
  Consume ./cases/bus/unemp/eligible_claims.json and ./cases/bus/unemp/budget_projection.json. For each eligible person,
  generate a weekly payment plan with a waiting week, and list required weekly actions
  (e.g., certification, job search entries). Print Answer/Reason/Check; write
  ./cases/bus/unemp/individual_schedules.json.
"""
from __future__ import annotations

import argparse, json, datetime as dt
from typing import Dict, Any, List

### DATA
CHECKLIST = ["Weekly certification", "Log 2 job-search activities", "Maintain availability"]
PAYMENT_DAY = "Friday"  # simple assumption

### LOGIC
def make_schedule(claims_json: Dict[str,Any], asof: dt.date) -> Dict[str,Any]:
    schedules: List[Dict[str,Any]] = []
    for c in claims_json["eligible_claims"]:
        weeks = c["duration_weeks"]
        waiting = c["waiting_weeks"]
        start = dt.date.fromisoformat(c["start_date"])
        entries = []
        for i in range(weeks + waiting):
            week_start = start + dt.timedelta(weeks=i)
            paid = (i >= waiting)
            entries.append({
                "week": i+1,
                "week_start": week_start.isoformat(),
                "payment_due": (week_start + dt.timedelta(days=4)).isoformat(),  # Friday of that week
                "amount_eur": c["wba_eur"] if paid else 0.0,
                "checklist": CHECKLIST
            })
        schedules.append({"id": c["id"], "name": c["name"], "payment_entries": entries})
    reasons = [
        f"Waiting week policy: first {claims_json['policy']['waiting_week']} week(s) unpaid.",
        f"Payment day assumed: {PAYMENT_DAY} (week_start + 4 days)."
    ]
    return {"as_of": asof.isoformat(), "schedules": schedules, "reasons": reasons}

### CHECK
def run_harness(claims_json: Dict[str,Any], sched: Dict[str,Any]) -> None:
    print("Check 1 — Schedule length equals duration + waiting:")
    for c in claims_json["eligible_claims"]:
        sch = next(s for s in sched["schedules"] if s["id"] == c["id"])
        expected = c["duration_weeks"] + c["waiting_weeks"]
        print(f"  - {c['id']} expected={expected}, got={len(sch['payment_entries'])}")
        assert len(sch["payment_entries"]) == expected

    print("Check 2 — Waiting week has zero payment; others equal WBA:")
    for c in claims_json["eligible_claims"]:
        sch = next(s for s in sched["schedules"] if s["id"] == c["id"])
        waiting = c["waiting_weeks"]
        for i, entry in enumerate(sch["payment_entries"]):
            amt = entry["amount_eur"]
            if i < waiting:
                assert amt == 0.0
            else:
                assert amt == c["wba_eur"]

    print("Check 3 — Checklist present every week:")
    for s in sched["schedules"]:
        for e in s["payment_entries"]:
            assert e["checklist"] == CHECKLIST

def main():
    ap = argparse.ArgumentParser(description="Produce individual schedules & compliance checklist.")
    ap.add_argument("--in_claims", default="./cases/bus/unemp/eligible_claims.json")
    ap.add_argument("--in_budget", default="./cases/bus/unemp/budget_projection.json")
    ap.add_argument("--asof", default="2025-09-18")
    args = ap.parse_args()

    with open(args.in_claims,"r",encoding="utf-8") as f: claims = json.load(f)
    with open(args.in_budget,"r",encoding="utf-8") as f: budget = json.load(f)  # loaded for completeness

    asof = dt.date.fromisoformat(args.asof)
    sched = make_schedule(claims, asof)

    print("# ANSWER")
    # Print only counts + first schedule to keep output compact
    first = sched["schedules"][0] if sched["schedules"] else {}
    print(json.dumps({"n_schedules": len(sched["schedules"]), "first_schedule_preview": first}, indent=2, ensure_ascii=False))

    print("\n# REASONS")
    for r in sched["reasons"]:
        print("-", r)

    with open("./cases/bus/unemp/individual_schedules.json","w",encoding="utf-8") as f:
        json.dump(sched, f, indent=2, ensure_ascii=False)
    print("\nWrote ./cases/bus/unemp/individual_schedules.json")

    print("\n# CHECK (harness) — detailed")
    run_harness(claims, sched)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()
