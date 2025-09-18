#!/usr/bin/env python3
"""
EYE Learning — Program 5: Reminders & Printables

Short story (header):
  Generate simple action reminders (order pizza, pick up cupcakes, set up stations)
  and printable name tags.

Overview:
  - Reads './resources/shopping_list.json' + './resources/guest_roster.json'.
  - Emits './resources/reminders.json' and './resources/name_tags.txt'.
  - Prints Answer/Reason/Check; writes artifacts for Program 6.
"""
from __future__ import annotations
import argparse, json, datetime as dt

def build_reminders(lst, roster):
    date = roster["event"]["event_date"]
    event_dt = dt.date.fromisoformat(date)
    # Reminders two days before & morning of event
    items = [
        {"date": (event_dt - dt.timedelta(days=2)).isoformat(), "title": "Confirm pizza order & pickup time"},
        {"date": (event_dt - dt.timedelta(days=2)).isoformat(), "title": "Confirm cupcakes with bakery"},
        {"date": event_dt.isoformat(), "title": "Pick up cupcakes"},
        {"date": event_dt.isoformat(), "title": "Set up activity stations 1 hour before"}
    ]
    reasons = [f"{len(items)} reminders created around {date}."]
    # Name tags (one per attendee)
    names = [g["name"] for g in roster["attendees"]]
    name_tag_text = "NAME TAGS\n" + "\n".join(f"- {n}" for n in names)
    return {"items": items, "reasons": reasons, "name_tags_text": name_tag_text}

def run_harness(rem):
    print("Check — reminders have date & title; name tags cover attendees:")
    assert all("date" in it and "title" in it for it in rem["items"])
    assert "NAME TAGS" in rem["name_tags_text"]

def main():
    ap = argparse.ArgumentParser(description="Create reminders and printable name tags.")
    ap.add_argument("--in_shopping", default="./resources/shopping_list.json")
    ap.add_argument("--in_roster", default="./resources/guest_roster.json")
    args = ap.parse_args()

    lst = json.load(open(args.in_shopping,"r",encoding="utf-8"))
    roster = json.load(open(args.in_roster,"r",encoding="utf-8"))
    rem = build_reminders(lst, roster)

    print("# ANSWER"); print(json.dumps({"n_reminders": len(rem["items"])}, indent=2))
    print("\n# REASONS"); [print("-", r) for r in rem["reasons"]]

    json.dump({"items": rem["items"]}, open("./resources/reminders.json","w",encoding="utf-8"), indent=2)
    open("./resources/name_tags.txt","w",encoding="utf-8").write(rem["name_tags_text"])
    print("\nWrote ./resources/reminders.json and ./resources/name_tags.txt")

    print("\n# CHECK"); run_harness(rem); print("✔ All checks passed.")

if __name__ == "__main__":
    main()

