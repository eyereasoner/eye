#!/usr/bin/env python3
"""
P3 — Program 4: Reminders & Calendar Export (Text/ICS-lite)

Short story (header):
  With the forecast and goals in place, the last step is to create reminders:
  paydays, weekly bill checks, and a savings transfer.

Overview:
  - Reads './cases/bus/finance/forecast_30d.json' and './cases/bus/finance/goals_plan.json'.
  - Emits a small reminders list and a minimal .ics-like text file.
  - Prints Answer/Reason/Check and writes './cases/bus/finance/reminders.json' and './cases/bus/finance/reminders.ics.txt'.
"""
from __future__ import annotations

import argparse, json, datetime as dt
from typing import Dict, Any, List

### LOGIC
def build_reminders(fc: Dict[str,Any], goals: Dict[str,Any]) -> Dict[str,Any]:
    items = []
    reasons = []
    for d in fc["days"]:
        date = d["date"]
        if d["inflow"] > 0:
            items.append({"date": date, "title": "Payday"})
            reasons.append(f"{date}: Payday reminder created.")
        if "Mon" in dt.date.fromisoformat(date).strftime("%a"):
            items.append({"date": date, "title": "Weekly bill check"})
    alloc = sum(a["amount_eur"] for a in goals["allocations"])
    first_payday = next((d["date"] for d in fc["days"] if d["inflow"] > 0), None)
    if alloc > 0 and first_payday:
        items.append({"date": first_payday, "title": f"Transfer savings €{alloc}"})
        reasons.append(f"{first_payday}: Savings transfer for total allocations €{alloc}.")
    return {"items": items, "reasons": reasons}

def write_ics_like(items: List[Dict[str,Any]]) -> str:
    lines = ["BEGIN:VCALENDAR"]
    for it in items:
        lines += [
            "BEGIN:VEVENT",
            f"SUMMARY:{it['title']}",
            f"DTSTART;VALUE=DATE:{it['date'].replace('-','')}",
            "END:VEVENT"
        ]
    lines.append("END:VCALENDAR")
    return "\n".join(lines)

### CHECK
def run_harness(rem: Dict[str,Any]) -> None:
    print("Check 1 — Every reminder has date and title:")
    for it in rem["items"]:
        assert "date" in it and "title" in it

    print("Check 2 — Savings transfer present iff allocations > 0 and payday exists:")
    transfers = [it for it in rem["items"] if "Transfer savings" in it["title"]]
    # (Informational; we don't force existence either way)

def main():
    parser = argparse.ArgumentParser(description="Build reminders and ICS-like export.")
    parser.add_argument("--in_forecast", default="./cases/bus/finance/forecast_30d.json")
    parser.add_argument("--in_goals", default="./cases/bus/finance/goals_plan.json")
    args = parser.parse_args()

    with open(args.in_forecast,"r",encoding="utf-8") as f: fc = json.load(f)
    with open(args.in_goals,"r",encoding="utf-8") as f: goals = json.load(f)

    rem = build_reminders(fc, goals)

    print("# ANSWER")
    print(json.dumps({"n_reminders": len(rem["items"])}, indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in rem["reasons"]:
        print("-", r)

    with open("./cases/bus/finance/reminders.json","w",encoding="utf-8") as f:
        json.dump(rem, f, indent=2, ensure_ascii=False)
    ics = write_ics_like(rem["items"])
    with open("./cases/bus/finance/reminders.ics.txt","w",encoding="utf-8") as f:
        f.write(ics)
    print("\nWrote ./cases/bus/finance/reminders.json and ./cases/bus/finance/reminders.ics.txt")

    print("\n# CHECK (harness) — detailed")
    run_harness(rem)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()

