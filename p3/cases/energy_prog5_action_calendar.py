#!/usr/bin/env python3
"""
P3 — Program 5: Action Calendar (Reminders & ICS-lite)

Short story:
  Turn the plan into simple reminders: run washer/dryer/dishwasher off-peak, and
  note a one-off task to buy efficient models when feasible.

Overview:
  - Reads './cases/bus/energy/baseline_daily.json' + './cases/bus/energy/savings_plan.json'.
  - Emits JSON reminders and an ICS-like text.
  - Prints Answer/Reasons/Check; writes './cases/bus/energy/actions.json' + './cases/bus/energy/actions.ics.txt'.
"""
from __future__ import annotations
import argparse, json, datetime as dt

def build_actions(base, plan, asof="2025-09-18"):
    offhours = sorted(set(base["tou_offpeak_hours"]))
    first_off = f"{offhours[0]:02d}:00" if offhours else "22:00"
    items = []
    # weekly reminders (Mon, Wed, Fri): run shiftable loads off-peak
    start = dt.date.fromisoformat(asof)
    for i in range(14):  # two weeks of reminders
        d = start + dt.timedelta(days=i)
        if d.weekday() in (0,2,4):
            items.append({"date": d.isoformat(), "title": f"Run shiftable loads after {first_off}"})
    # one-off: evaluate upgrades
    items.append({"date": start.isoformat(), "title": "Check efficient appliance options (Fridge/TV)"})
    reasons = [f"Off-peak hours: {offhours}", f"{len(items)} reminders generated."]
    # ICS-like
    lines = ["BEGIN:VCALENDAR"]
    for it in items:
        lines += ["BEGIN:VEVENT", f"SUMMARY:{it['title']}", f"DTSTART;VALUE=DATE:{it['date'].replace('-','')}", "END:VEVENT"]
    lines.append("END:VCALENDAR")
    return {"items": items, "ics_text": "\n".join(lines), "reasons": reasons}

def run_harness(a):
    print("Check — every reminder has date/title; ICS count matches:")
    assert all("date" in it and "title" in it for it in a["items"])
    assert a["ics_text"].count("BEGIN:VEVENT") == len(a["items"])

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--in_base", default="./cases/bus/energy/baseline_daily.json")
    ap.add_argument("--in_plan", default="./cases/bus/energy/savings_plan.json")
    args = ap.parse_args()

    base = json.load(open(args.in_base,"r",encoding="utf-8"))
    plan = json.load(open(args.in_plan,"r",encoding="utf-8"))
    a = build_actions(base, plan)

    print("# ANSWER"); print(json.dumps({"n_reminders": len(a["items"])}, indent=2))
    print("\n# REASONS"); [print("-", r) for r in a["reasons"]]

    json.dump({"items":a["items"]}, open("./cases/bus/energy/actions.json","w",encoding="utf-8"), indent=2)
    open("./cases/bus/energy/actions.ics.txt","w",encoding="utf-8").write(a["ics_text"])
    print("\nWrote ./cases/bus/energy/actions.json and ./cases/bus/energy/actions.ics.txt")

    print("\n# CHECK"); run_harness(a); print("✔ All checks passed.")

if __name__ == "__main__":
    main()

