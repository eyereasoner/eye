#!/usr/bin/env python3
"""
P3 — Program 1: Guest Roster & Dietary Counts

Short story (header):
  You’ve got a simple JSON with event details and an invite list. The task is to
  build the roster of attendees, check capacity, and summarize dietary needs.

Sections map & intent:
  ### DATA
      - Input JSON schema (event fields + guests list)
      - Supported diet tags
  ### LOGIC
      - Filter RSVPs, split adults/kids, tally diet tags
      - Produce a compact, self-contained artifact for downstream steps
  ### CHECK (harness)
      - Strong schema validation (types, required fields)
      - Counts consistency (total = adults + kids)
      - Capacity correctness, unique guest names, diet tag validation
"""

from __future__ import annotations
import argparse, json, datetime as dt
from typing import Dict, Any, List
from collections import Counter

### DATA -----------------------------------------------------------------------
REQUIRED_EVENT_FIELDS = {
    "host_name": str, "theme": str, "event_date": str, "start_time": str,
    "duration_hours": (int, float), "venue_capacity": int, "host_budget_eur": (int, float)
}
SUPPORTED_DIETS = {"veg", "vegan", "gluten_free", "nut_allergy"}

def _require(cond: bool, msg: str) -> None:
    if not cond:
        raise AssertionError(msg)

def _parse_date(s: str) -> dt.date:
    return dt.date.fromisoformat(s)

def _parse_time(s: str) -> dt.time:
    # Accept HH:MM
    return dt.time.fromisoformat(s)

### LOGIC ----------------------------------------------------------------------
def build_roster(inp: Dict[str, Any]) -> Dict[str, Any]:
    # Validate event-level fields early (soft schema guard; more in CHECK)
    event = {k: inp[k] for k in REQUIRED_EVENT_FIELDS}
    attendees = [g for g in inp["guests"] if bool(g.get("rsvp", False))]
    adults = [g for g in attendees if g.get("type") == "adult"]
    kids   = [g for g in attendees if g.get("type") == "child"]

    # Tally diet tags with a clear whitelist
    diet_counts = Counter()
    reasons = []
    for g in attendees:
        tags = [t for t in g.get("diet", []) if t in SUPPORTED_DIETS]
        for t in tags:
            diet_counts[t] += 1
        reasons.append(f"{g['name']}: type={g['type']}, diet={tags or 'none'}")

    total = len(attendees)
    capacity_ok = total <= event["venue_capacity"]
    return {
        "schema": "guest_roster.v1",
        "event": event,
        "attendees": attendees,
        "counts": {"total": total, "adults": len(adults), "kids": len(kids)},
        "diet_counts": dict(diet_counts),
        "capacity_ok": capacity_ok,
        "reasons": reasons
    }

### CHECK (harness) ------------------------------------------------------------
def run_harness(inp: Dict[str, Any], roster: Dict[str, Any]) -> None:
    print("Check 1 — Event schema & types:")
    for k, typ in REQUIRED_EVENT_FIELDS.items():
        _require(k in roster["event"], f"missing event field {k}")
        _require(isinstance(roster["event"][k], typ), f"bad type for {k}")
    # Parse date/time to ensure format correctness
    _parse_date(roster["event"]["event_date"])
    _parse_time(roster["event"]["start_time"])
    _require(roster["event"]["duration_hours"] > 0, "duration must be positive")
    _require(roster["event"]["venue_capacity"] > 0, "capacity must be positive")
    _require(roster["event"]["host_budget_eur"] >= 0, "budget must be non-negative")

    print("Check 2 — Guest entries valid & diets within whitelist:")
    names = set()
    for g in roster["attendees"]:
        _require("name" in g and isinstance(g["name"], str) and g["name"].strip(), "guest name invalid")
        _require(g.get("type") in {"adult", "child"}, "guest type must be adult|child")
        _require(isinstance(g.get("rsvp", True), bool), "rsvp must be bool")
        # Unique names among attendees to avoid duplicates on name tags
        _require(g["name"] not in names, f"duplicate attendee name: {g['name']}")
        names.add(g["name"])
        # diet tags subset
        for t in g.get("diet", []):
            _require(t in SUPPORTED_DIETS, f"unsupported diet tag: {t}")

    print("Check 3 — Counts & capacity consistency:")
    c = roster["counts"]
    _require(c["total"] == c["adults"] + c["kids"], "total != adults + kids")
    _require(roster["capacity_ok"] == (c["total"] <= roster["event"]["venue_capacity"]), "capacity flag mismatch")

def main():
    ap = argparse.ArgumentParser(description="Build guest roster & diet counts.")
    ap.add_argument("--infile", default="./bus/party/party_input_sample.json")
    ap.add_argument("--out", default="./bus/party/guest_roster.json")
    args = ap.parse_args()

    inp = json.load(open(args.infile, "r", encoding="utf-8"))
    roster = build_roster(inp)

    print("# ANSWER")
    print(json.dumps({"counts": roster["counts"], "capacity_ok": roster["capacity_ok"]}, indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in roster["reasons"]:
        print("-", r)

    json.dump(roster, open(args.out, "w", encoding="utf-8"), indent=2, ensure_ascii=False)
    print(f"\nWrote {args.out}")

    print("\n# CHECK (harness) — detailed")
    run_harness(inp, roster)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()

