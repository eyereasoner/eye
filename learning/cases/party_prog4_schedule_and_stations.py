#!/usr/bin/env python3
"""
EYE Learning — Program 4: Event Timeline & Activity Stations (ICS-lite)

Short story (header):
  Build a simple run-of-show: arrivals, icebreaker, food, games, cake, and wrap-up.
  Also list activity stations (coloring, space quiz, rocket craft).

Overview:
  - Reads './resources/guest_roster.json' (for date/time/duration & headcount).
  - Produces timeline blocks and activity stations.
  - Exports ICS-like text and './resources/run_of_show.json' for Program 5.
"""
from __future__ import annotations
import argparse, json, datetime as dt
from typing import Dict, Any, List

### LOGIC
def build_schedule(roster: Dict[str,Any]) -> Dict[str,Any]:
    date = roster["event"]["event_date"]; start = roster["event"]["start_time"]
    duration = roster["event"]["duration_hours"]
    start_dt = dt.datetime.fromisoformat(f"{date}T{start}")
    blocks = [
        ("Arrivals & Free Play", 0.0, 0.5),
        ("Icebreaker Game", 0.5, 1.0),
        ("Food & Drinks", 1.0, 1.7),
        ("Craft: Build a Paper Rocket", 1.7, 2.2),
        ("Cake & Happy Birthday", 2.2, 2.5),
        ("Group Game: Space Quiz", 2.5, duration)
    ]
    events = []
    reasons = []
    for title, s, e in blocks:
        st = start_dt + dt.timedelta(hours=s); en = start_dt + dt.timedelta(hours=e)
        events.append({"title": title, "start": st.isoformat(), "end": en.isoformat()})
        reasons.append(f"{title}: {st.time()}–{en.time()}")
    stations = ["Coloring Corner", "Space Quiz Table", "Rocket Craft Station"]
    # ICS-like
    lines = ["BEGIN:VCALENDAR"]
    for e in events:
        lines += ["BEGIN:VEVENT", f"SUMMARY:{e['title']}",
                  f"DTSTART:{e['start'].replace('-','').replace(':','')}",
                  f"DTEND:{e['end'].replace('-','').replace(':','')}", "END:VEVENT"]
    lines.append("END:VCALENDAR")
    return {"events": events, "stations": stations, "ics_text": "\n".join(lines), "reasons": reasons}

### CHECK
def run_harness(sched: Dict[str,Any]) -> None:
    print("Check — non-empty schedule and ICS count match:")
    assert len(sched["events"]) >= 3
    assert sched["ics_text"].count("BEGIN:VEVENT") == len(sched["events"])

def main():
    ap = argparse.ArgumentParser(description="Build party run-of-show and stations.")
    ap.add_argument("--infile", default="./resources/guest_roster.json")
    ap.add_argument("--out", default="./resources/run_of_show.json")
    args = ap.parse_args()

    roster = json.load(open(args.infile,"r",encoding="utf-8"))
    sched = build_schedule(roster)

    print("# ANSWER"); print(json.dumps({"n_events": len(sched["events"]), "stations": sched["stations"]}, indent=2))
    print("\n# REASONS"); [print("-", r) for r in sched["reasons"]]

    json.dump({"events": sched["events"], "stations": sched["stations"]}, open(args.out,"w",encoding="utf-8"), indent=2)
    open("./resources/party_schedule.ics.txt","w",encoding="utf-8").write(sched["ics_text"])
    print("\nWrote ./resources/run_of_show.json and ./resources/party_schedule.ics.txt")

    print("\n# CHECK"); run_harness(sched); print("✔ All checks passed.")

if __name__ == "__main__":
    main()

