#!/usr/bin/env python3
"""
P3 — Program 4: Move-Day Timeline & Checklist (ICS-lite)

Short story (header):
  It’s move day: load, drive, unload. This program turns van/trip info into a
  simple timeline and a checklist with reminders.

Overview:
  - Reads './bus/move/van_plan.json' and './bus/move/packing_plan.json'.
  - Creates a day timeline: load → drive → unload per trip.
  - Emits a minimal ICS-like text plus a JSON artifact './bus/move/move_schedule.json'.
  - Prints Answer/Reason/Check in EYE format.
"""
from __future__ import annotations
import argparse, json, datetime as dt

### LOGIC
def build_schedule(van_plan, packing, start_date="2025-09-25", start_time="09:00"):
    trips = van_plan["trips"]
    events = []
    start_dt = dt.datetime.fromisoformat(f"{start_date}T{start_time}")
    cur = start_dt
    reasons = [f"Start at {start_dt.isoformat()}; trips={trips}."]
    # rough timing: load 1h per trip + driving from plan + unload 1h per trip (split evenly)
    drive_total_h = float(van_plan["time_hours"])
    drive_per_trip = drive_total_h / trips if trips else 0.0
    for t in range(1, trips+1):
        load_dur = dt.timedelta(hours=1)
        drive_dur = dt.timedelta(hours=drive_per_trip)
        unload_dur = dt.timedelta(hours=1)
        events += [
            {"title": f"Trip {t}: Load",   "start": cur.isoformat(), "end": (cur+load_dur).isoformat()},
            {"title": f"Trip {t}: Drive",  "start": (cur+load_dur).isoformat(), "end": (cur+load_dur+drive_dur).isoformat()},
            {"title": f"Trip {t}: Unload", "start": (cur+load_dur+drive_dur).isoformat(), "end": (cur+load_dur+drive_dur+unload_dur).isoformat()},
        ]
        cur = cur + load_dur + drive_dur + unload_dur
        reasons.append(f"Trip {t}: ~1h load + {round(drive_per_trip,2)}h drive + 1h unload.")
    ics = ["BEGIN:VCALENDAR"]
    for e in events:
        dtstart = e["start"].replace("-", "").replace(":", "")
        dtend = e["end"].replace("-", "").replace(":", "")
        ics += ["BEGIN:VEVENT", f"SUMMARY:{e['title']}", f"DTSTART:{dtstart}", f"DTEND:{dtend}", "END:VEVENT"]
    ics.append("END:VCALENDAR")
    return {"events": events, "reasons": reasons, "ics_text": "\n".join(ics)}

### CHECK
def run_harness(van_plan, sched):
    print("Check 1 — Events present and ordered:")
    assert len(sched["events"]) == van_plan["trips"] * 3
    assert all(sched["events"][i]["end"] <= sched["events"][i+1]["start"] for i in range(len(sched["events"])-1))
    print("Check 2 — ICS has matching count:")
    assert sched["ics_text"].count("BEGIN:VEVENT") == len(sched["events"])

def main():
    ap = argparse.ArgumentParser(description="Build move-day schedule and ICS-like export.")
    ap.add_argument("--in_van", default="./bus/move/van_plan.json")
    ap.add_argument("--in_packing", default="./bus/move/packing_plan.json")
    ap.add_argument("--out", default="./bus/move/move_schedule.json")
    args = ap.parse_args()

    with open(args.in_van,"r",encoding="utf-8") as f: van_plan = json.load(f)
    with open(args.in_packing,"r",encoding="utf-8") as f: packing = json.load(f)

    sched = build_schedule(van_plan, packing)

    print("# ANSWER")
    print(json.dumps({"n_events": len(sched["events"])}, indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in sched["reasons"]: print("-", r)

    with open(args.out,"w",encoding="utf-8") as f: json.dump({"events":sched["events"]}, f, indent=2, ensure_ascii=False)
    with open("./bus/move/move_schedule.ics.txt","w",encoding="utf-8") as f: f.write(sched["ics_text"])
    print("\nWrote ./bus/move/move_schedule.json and ./bus/move/move_schedule.ics.txt")

    print("\n# CHECK (harness) — detailed")
    run_harness(van_plan, sched)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()

