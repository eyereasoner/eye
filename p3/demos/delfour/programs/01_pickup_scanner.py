#!/usr/bin/env python3
import sys, os, json
from datetime import datetime, timezone, timedelta
from _log import log
BUS = os.path.join(os.path.dirname(__file__), "..", "data", "bus")
os.makedirs(BUS, exist_ok=True)

context_ttl = os.path.join(BUS, "context.ttl")
events_ttl  = os.path.join(BUS, "events.ttl")

def main():
    now = datetime.now(timezone.utc)
    expires = (now + timedelta(hours=2)).isoformat()
    ctx = f"""@prefix ctx: <https://example.org/context#> .
[] ctx:retailer "Delfour" ;
   ctx:device   "self-scanner" ;
   ctx:event    "pick_up_scanner" ;
   ctx:timestamp "{now.isoformat()}" ;
   ctx:expiresAt "{expires}" ."""
    with open(context_ttl, "w") as f:
        f.write(ctx)
    with open(events_ttl, "w") as f:
        f.write(f'@prefix ev: <https://example.org/event#> .\n[] ev:type "pick_up_scanner".\n')
    log("PICKUP", 'scanner="self-scanner" retailer="Delfour"')

if __name__ == "__main__":
    main()

