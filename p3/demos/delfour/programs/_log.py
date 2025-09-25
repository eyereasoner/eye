#!/usr/bin/env python3
from __future__ import annotations
import os, sys
from datetime import datetime, timezone
BASE = os.path.join(os.path.dirname(__file__), "..")
BUS  = os.path.join(BASE, "data", "bus")
os.makedirs(BUS, exist_ok=True)
TL   = os.path.join(BUS, "timeline.log")

def log(event: str, detail: str = ""):
    now = datetime.now(timezone.utc).isoformat()
    line = f"{now}\t{event}\t{detail}\n"
    with open(TL, "a") as f:
        f.write(line)
    # minimal on-screen echo
    print(f"[{now}] {event}" + (f" — {detail}" if detail else ""))

