#!/usr/bin/env python3
"""
P3 — Program 4: Event Timeline & Activity Stations (ICS-lite)

Short story (header):
  Build a simple run-of-show: arrivals, icebreaker, food, games, cake, wrap-up.
  Export an ICS-like text.

Sections map & intent:
  ### DATA
      - Block templates (relative hours from start)
  ### LOGIC
      - Convert to absolute datetimes from event date/time & duration
      - Create 3+ stations
  ### CHECK (harness)
      - Chronological order, non-overlap, last end ≈ start + duration
      - ICS event count equals JSON events count
"""

from __future__ import annotations
import argparse, json, datetime as dt
from typing import Dict, Any, List

### DATA -----------------------------------------------------------------------
BLOCKS = [
    ("Arrivals & Free Play", 0.0, 0.5),
    ("Icebreaker Game",      0.5, 1.0),
    ("Food & Drinks",        1.0, 1.7),
    ("Craft: Build a Paper Rocket", 1.7, 2.2),
    ("Cake & Happy Birthday",       2.2, 2.5),
    ("Group Game: Space Quiz",      2.5, None)  # 'None' means "until duration"
]
STATIONS = ["Coloring Corner", "Space Quiz Table", "Rocket Craft Station"]

### LOGIC ----------------------------------------------------------------------
def build_schedule(roster: Dict[str, Any]) -> Dict[str, Any]:
    date = roster["event"]["event_date"]
    start = roster["event"]["start_time"]
    duration = float(roster["event"]["duration_hours"])
    start_dt = dt.datetime.fromisoformat(f"{date}T{start}")

    events = []
    reasons = []
    for title, s, e in BLOCKS:
        s_h = float(s)
        e_h = duration if e is None else float(e)
        st = start_dt + dt.timedelta(hours=s_h)
        en = start_dt + dt.timedelta(hours=min(e_h, duration))
        events.append({"title": title, "start": st.isoformat(), "end": en.isoformat()})
        reasons.append(f"{title}: {st.time()}–{en.time()}")

    # ICS-like
    lines = ["BEGIN:VCALENDAR"]
    for e in events:
        lines += [
            "BEGIN:VEVENT",
            f"SUMMARY:{e['title']}",
            f"DTSTART:{e['start'].replace('-','').replace(':','')}",
            f"DTEND:{e['end'].replace('-','').replace(':','')}",
            "END:VEVENT"
        ]
    lines.append("END:VCALENDAR")

    return {
        "schema": "run_of_show.v1",
        "events": events,
        "stations": list(STATIONS),
        "ics_text": "\n".join(lines),
        "reasons": reasons
    }

### CHECK (harness) ------------------------------------------------------------
def run_harness(roster: Dict[str, Any], sched: Dict[str, Any]) -> None:
    print("Check 1 — Chronology & non-overlap:")
    times = [(e["title"], dt.datetime.fromisoformat(e["start"]), dt.datetime.fromisoformat(e["end"])) for e in sched["events"]]
    for i, (_, s, e) in enumerate(times):
        assert e > s, "event duration must be positive"
        if i > 0:
            assert s >= times[i-1][2], "events must not overlap and must be in order"

    print("Check 2 — Final end ≈ start + duration:")
    start = dt.datetime.fromisoformat(sched["events"][0]["start"])
    final_end = dt.datetime.fromisoformat(sched["events"][-1]["end"])
    expected_end = start + dt.timedelta(hours=float(roster["event"]["duration_hours"]))
    # allow ±5 minutes slack
    assert abs((final_end - expected_end).total_seconds()) <= 5 * 60, "end time deviates too much"

    print("Check 3 — ICS events count matches JSON events count & stations present:")
    assert sched["ics_text"].count("BEGIN:VEVENT") == len(sched["events"])
    assert len(sched["stations"]) >= 3

def main():
    ap = argparse.ArgumentParser(description="Build party run-of-show and stations.")
    ap.add_argument("--infile", default="./bus/party/guest_roster.json")
    ap.add_argument("--out", default="./bus/party/run_of_show.json")
    args = ap.parse_args()

    roster = json.load(open(args.infile, "r", encoding="utf-8"))
    sched = build_schedule(roster)

    print("# ANSWER")
    print(json.dumps({"n_events": len(sched["events"]), "stations": sched["stations"]}, indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in sched["reasons"]:
        print("-", r)

    json.dump({"events": sched["events"], "stations": sched["stations"]}, open(args.out, "w", encoding="utf-8"), indent=2, ensure_ascii=False)
    open("./bus/party/party_schedule.ics.txt", "w", encoding="utf-8").write(sched["ics_text"])
    print("\nWrote ./bus/party/run_of_show.json and ./bus/party/party_schedule.ics.txt")

    print("\n# CHECK (harness) — detailed")
    run_harness(roster, sched)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()

