#!/usr/bin/env python3
"""
EYE Learning — Program 1: Guest Roster & Dietary Counts

Short story:
  You’ve got a simple JSON with event details and an invite list. The task is to
  build the roster of attendees, check capacity, and summarize dietary needs.

Overview (chat-aligned):
  - Reads './resources/party_input_sample.json' (or your JSON via --infile).
  - Keeps only guests with rsvp=true and splits adults/kids.
  - Tallies diet tags (veg/vegan/gluten_free/nut_allergy).
  - Prints Answer/Reason/Check; writes './resources/guest_roster.json' for Program 2.

Sections map:  ### DATA / LOGIC / CHECK
"""
from __future__ import annotations
import argparse, json
from typing import Dict, Any, List
from collections import Counter

### DATA
DIET_TAGS = ["veg", "vegan", "gluten_free", "nut_allergy"]

### LOGIC
def build_roster(inp: Dict[str,Any]) -> Dict[str,Any]:
    attendees = [g for g in inp["guests"] if g.get("rsvp", False)]
    adults = [g for g in attendees if g["type"] == "adult"]
    kids   = [g for g in attendees if g["type"] == "child"]
    diet_counts = Counter()
    reasons = []
    for g in attendees:
        tags = [t for t in g.get("diet", []) if t in DIET_TAGS]
        for t in tags: diet_counts[t] += 1
        reasons.append(f"{g['name']}: type={g['type']}, diet={tags or 'none'}")
    total = len(attendees)
    capacity_ok = total <= inp["venue_capacity"]
    return {
        "event": {
            "host_name": inp["host_name"],
            "theme": inp["theme"],
            "event_date": inp["event_date"],
            "start_time": inp["start_time"],
            "duration_hours": inp["duration_hours"],
            "venue_capacity": inp["venue_capacity"],
            "host_budget_eur": inp["host_budget_eur"]
        },
        "attendees": attendees,
        "counts": {"total": total, "adults": len(adults), "kids": len(kids)},
        "diet_counts": dict(diet_counts),
        "capacity_ok": capacity_ok,
        "reasons": reasons
    }

### CHECK
def run_harness(inp: Dict[str,Any], roster: Dict[str,Any]) -> None:
    print("Check 1 — Counts match attendees:")
    assert roster["counts"]["total"] == len(roster["attendees"])
    print("Check 2 — Capacity flag consistent:")
    assert roster["capacity_ok"] == (roster["counts"]["total"] <= roster["event"]["venue_capacity"])

def main():
    ap = argparse.ArgumentParser(description="Build guest roster & diet counts from JSON.")
    ap.add_argument("--infile", default="./resources/party_input_sample.json")
    ap.add_argument("--out", default="./resources/guest_roster.json")
    args = ap.parse_args()

    inp = json.load(open(args.infile, "r", encoding="utf-8"))
    roster = build_roster(inp)

    print("# ANSWER"); print(json.dumps({"counts": roster["counts"], "capacity_ok": roster["capacity_ok"]}, indent=2))
    print("\n# REASONS"); [print("-", r) for r in roster["reasons"]]

    json.dump(roster, open(args.out, "w", encoding="utf-8"), indent=2)
    print(f"\nWrote {args.out}")

    print("\n# CHECK"); run_harness(inp, roster); print("✔ All checks passed.")

if __name__ == "__main__":
    main()

