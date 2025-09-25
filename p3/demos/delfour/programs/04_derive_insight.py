#!/usr/bin/env python3
import os, subprocess, sys
from pathlib import Path
from _log import log
BASE = os.path.join(os.path.dirname(__file__), "..")
BUS  = os.path.join(BASE, "data", "bus")
RULE = os.path.join(BASE, "rules", "derive_insight_from_need_and_context.n3")
need   = os.path.join(BUS, "need.ttl")
context= os.path.join(BUS, "context.ttl")

EYE = os.environ.get("EYE", "eye")

def main():
    if not (Path(need).exists() and Path(context).exists()):
        print("Missing need.ttl or context.ttl", file=sys.stderr); sys.exit(2)
    args = [EYE, "--quiet", "--nope", "--pass-only-new", need, context, RULE]
    res = subprocess.run(args, capture_output=True, text=True)
    if res.returncode != 0:
        print(res.stderr, file=sys.stderr); sys.exit(1)
    with open(os.path.join(BUS, "insight.ttl"), "w") as f:
        f.write(res.stdout)
    log("INSIGHT", "neutral low-sugar envelope emitted")

if __name__ == "__main__":
    main()

