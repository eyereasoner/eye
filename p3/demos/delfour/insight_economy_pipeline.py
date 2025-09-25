#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
INSIGHT ECONOMY (multi-program pipeline) — P3 style

Answer / Reason / Check output, orchestrated over 6 tiny programs that
communicate via RDF/N3 files and use EYE for logic steps.
"""
from __future__ import annotations
import json, os, subprocess, sys, re
from datetime import datetime, timezone
from pathlib import Path
BASE   = Path(__file__).resolve().parent
PROG   = BASE / "programs"
DATA   = BASE / "data"
BUS    = DATA / "bus"
SOLID  = DATA / "solid" / "profile.ttl"
CAT    = DATA / "retailer" / "catalog.ttl"
INS_TTL= BUS / "insight.ttl"
RUNTIME= BUS / "runtime_out.json"
TLFILE = BUS / "timeline.log"

def run(prog):
    p = subprocess.run([sys.executable, str(PROG/prog)], capture_output=True, text=True)
    if p.returncode != 0:
        print(p.stdout); print(p.stderr, file=sys.stderr); sys.exit(p.returncode)
    return p.stdout.strip()

def private_reason() -> str:
    # Minimal, private-side “why”
    return ("Household requires low-sugar guidance (diabetes in POD). "
            "You are starting a self-scanner session at Delfour right now. "
            "Envelope is minimized and limited to this session.")

def check() -> list[tuple[str,bool,str]]:
    results = []
    # 1) Insight exists and contains no sensitive words
    ok = INS_TTL.exists()
    results.append(("insight_file_exists", ok, "bus/insight.ttl present"))
    if ok:
        txt = INS_TTL.read_text().lower()
        forbidden = ["diabetes", "medical"]
        results.append(("minimization_no_sensitive_terms", not any(w in txt for w in forbidden),
                        "insight.ttl is neutral"))
        # 2) Scope fields present
        results.append(("scope_has_device_event_expiry",
                        all(k in txt for k in ["scopeevent","scopedevice","expiresat"]),
                        "derived insight includes scope"))
    # 3) Behavior: runtime produced a banner + suggestion for high sugar
    ok2 = RUNTIME.exists()
    results.append(("runtime_banner_exists", ok2, "bus/runtime_out.json present"))
    if ok2:
        b = json.loads(RUNTIME.read_text())
        results.append(("behavior_suggests_on_high_sugar", bool(b.get("suggested_alternative")), "suggestion was present"))

    # 4) Pipeline order: both start and drop events logged
    evt = (DATA/"bus"/"events.ttl").read_text()
    results.append(("events_have_pickup_and_drop",
                    ("pick_up_scanner" in evt and "drop_scanner" in evt),
                    "start and end recorded"))
    return results

def main():
    os.makedirs(BUS, exist_ok=True)
    # start fresh timeline for this run
    try: TLFILE.unlink()
    except FileNotFoundError: pass

    steps = [
        "01_pickup_scanner.py",
        "02_capability_dialog.py",
        "03_desensitize_private.py",
        "04_derive_insight.py",
        "05_shopping_runtime.py",
        "06_drop_scanner.py",
    ]
    logs = [run(s) for s in steps]

    # ----- TIMELINE
    print("=== TIMELINE ===")
    if TLFILE.exists():
        # print as simple table: time | event | detail
        with open(TLFILE, "r") as f:
            for line in f:
                ts, ev, *rest = line.rstrip("\n").split("\t")
                detail = rest[0] if rest else ""
                print(f"{ts}  {ev:16}  {detail}")
    else:
        print("(no timeline)")

    # ----- ANSWER
    print("\n=== ANSWER — INSIGHT (to retailer) ===")
    print(INS_TTL.read_text() if INS_TTL.exists() else "(no insight)")

    print("\n=== ANSWER — RUNTIME PREVIEW (device banner) ===")
    print(json.dumps(json.loads(RUNTIME.read_text()), indent=2) if RUNTIME.exists() else "(no banner)")

    # ----- REASON WHY (private)
    print("\n=== REASON WHY (private) ===")
    print(private_reason())

    # ----- CHECK
    print("\n=== CHECKS ===")
    results = check()
    passed = sum(1 for _, ok, _ in results if ok)
    for name, ok, note in results:
        print(f"{'PASS' if ok else 'FAIL'} - {name}: {note}")
    print(f"Overall: {passed}/{len(results)} checks passed.")

if __name__ == "__main__":
    main()

