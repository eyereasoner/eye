#!/usr/bin/env python3
import os
from _log import log
BUS = os.path.join(os.path.dirname(__file__), "..", "data", "bus")
def main():
    with open(os.path.join(BUS, "events.ttl"), "a") as f:
        f.write(f'@prefix ev: <https://example.org/event#> .\n[] ev:type "drop_scanner".\n')
    log("DROP", "scanner returned; session closed")

if __name__ == "__main__":
    main()

