#!/usr/bin/env python3
import os, subprocess, sys
from _log import log
BASE = os.path.join(os.path.dirname(__file__), "..")
BUS  = os.path.join(BASE, "data", "bus")
DATA = os.path.join(BASE, "data")
RULE = os.path.join(BASE, "rules", "desensitize_diabetes_to_need_low_sugar.n3")
PRIVATE = os.path.join(DATA, "solid", "profile.ttl")

EYE = os.environ.get("EYE", "eye")

def main():
    out = os.path.join(BUS, "need.ttl")
    args = [EYE, "--quiet", "--nope", "--pass-only-new", PRIVATE, RULE]
    res = subprocess.run(args, capture_output=True, text=True)
    if res.returncode != 0:
        print(res.stderr, file=sys.stderr); sys.exit(1)
    with open(out, "w") as f:
        f.write(res.stdout)
    log("DESENSITIZE", "need=needsLowSugar written")

if __name__ == "__main__":
    main()

