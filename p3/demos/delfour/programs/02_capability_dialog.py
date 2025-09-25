#!/usr/bin/env python3
import os, shutil
from _log import log
BASE = os.path.join(os.path.dirname(__file__), "..")
BUS  = os.path.join(BASE, "data", "bus")
PUB  = os.path.join(BASE, "data", "retailer")
os.makedirs(BUS, exist_ok=True)

def main():
    # Simulate agent ↔ agent exchange: copy the published capability file
    src = os.path.join(PUB, "delfour_capabilities.ttl")
    dst = os.path.join(BUS, "capabilities.ttl")
    shutil.copyfile(src, dst)
    log("AGENT-DIALOG", "capabilities received from Delfour")

if __name__ == "__main__":
    main()

