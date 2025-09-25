#!/usr/bin/env python3
import os, json, subprocess, sys, tempfile
from pathlib import Path
from _log import log
import re
BASE = os.path.join(os.path.dirname(__file__), "..")
BUS  = os.path.join(BASE, "data", "bus")
DAT  = os.path.join(BASE, "data", "retailer")
RULE = os.path.join(BASE, "rules", "shopping_suggest_alternative.n3")
insight = os.path.join(BUS, "insight.ttl")
catalog = os.path.join(DAT, "catalog.ttl")

EYE = os.environ.get("EYE", "eye")

def main():
    if not Path(insight).exists():
        print("No insight present; nothing to do."); return
    # Simulate a scan event, selecting a high-sugar product
    scan_ttl = os.path.join(BUS, "scan.ttl")
    with open(scan_ttl, "w") as f:
        f.write("""@prefix shop: <https://example.org/shop#> .
@prefix prod: <https://example.org/product#> .
[] shop:scannedProduct prod:BIS_001 .""")
    scanned_qname = "prod:BIS_001"
    # Run rule to possibly attach a suggestion
    args = [EYE, "--quiet", "--nope", "--pass-only-new", insight, catalog, scan_ttl, RULE]
    res = subprocess.run(args, capture_output=True, text=True)
    if res.returncode != 0:
        print(res.stderr, file=sys.stderr); sys.exit(1)

    # Convert minimal RDF result to a tiny JSON runtime banner (device-side)
    # (Here we do a simple string check; in production parse RDF.)
    suggested = "shop:suggestedAlternative" in res.stdout
    alt_qname = None
    m = re.search(r"shop:suggestedAlternative\s+([^\s]+)\s*\.", res.stdout)
    if m: alt_qname = m.group(1)

    # (very minimal) get human-friendly names from catalog.ttl
    def name_for(qname: str) -> str | None:
        """
        Look up schema:name for a given qname like 'prod:BIS_001' in catalog.ttl.
        This is a minimal grep (not a full RDF parse) but robust enough for our sample Turtle.
        """
        if not qname:
            return None
        txt = open(catalog, "r", encoding="utf-8").read()
        # find: [qname] ... schema:name "NAME"
        pat = rf"{re.escape(qname)}\b.*?schema:name\s+\"([^\"]+)\""
        m2 = re.search(pat, txt, flags=re.DOTALL)
        return m2.group(1) if m2 else None

    scanned_name = name_for(scanned_qname) or "Unknown"
    alt_name = name_for(alt_qname) if alt_qname else None

    banner = {
        "headline": "Track sugar per serving while you scan",
        "product_name": scanned_name,
        "note": "High sugar" if suggested else None,
        "suggested_alternative": (alt_name or "present") if suggested else None
    }
    with open(os.path.join(BUS, "runtime_out.json"), "w") as f:
        json.dump(banner, f, indent=2)
    if suggested:
        log("SCAN", f'product="{scanned_name}" → suggest="{alt_name}"')
    else:
        log("SCAN", f'product="{scanned_name}" (ok)')
    log("RUNTIME", "banner written")

if __name__ == "__main__":
    main()

