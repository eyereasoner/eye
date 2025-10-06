#!/usr/bin/env python3
"""
P3 — Program 1: Ingest & Normalize Claims (Elementary Number Theory)

Short story (header):
  You provide a JSON file of simple proof goals (divisibility, parity, gcd, mod).
  The task is to normalize each claim into a clear, machine-checkable shape.

Overview (what this file does, mirrored from this chat):
  - Reads './cases/bus/proof/proof_claims_sample.json' (or your file via --infile).
  - Validates schema and normalizes numeric types.
  - Emits './cases/bus/proof/normalized_claims.json' for downstream steps.
  - Prints Answer/Reason/Check in the EYE pattern.

Sections map:
  ### DATA  – input schema and supported claim types
  ### LOGIC – normalization and basic sanity
  ### CHECK – harness that re-validates shapes and types
"""
from __future__ import annotations
import argparse, json
from typing import Dict, Any, List

SUPPORTED = {
    "divisibility_transitive",
    "even_plus_even_is_even",
    "gcd_linear_combination",
    "coprime_product_divides_implies_divides_factor",
    "mod_addition_compatible",
}

def _int(x): 
    if isinstance(x, bool): return int(x)
    return int(x)

### LOGIC
def normalize(inp: Dict[str,Any]) -> Dict[str,Any]:
    claims = []
    reasons = []
    for raw in inp.get("claims", []):
        cid = str(raw["id"]); typ = str(raw["type"])
        if typ not in SUPPORTED:
            raise ValueError(f"Unsupported claim type {typ}")
        d = {k: _int(v) for (k,v) in raw["data"].items()}
        claims.append({"id":cid,"type":typ,"data":d})
        reasons.append(f"{cid}: type={typ}, data={d}")
    return {"claims": claims, "reasons": reasons}

### CHECK
def run_harness(out: Dict[str,Any]) -> None:
    print("Check — each claim has id/type/data and supported type:")
    for c in out["claims"]:
        assert set(c.keys())=={"id","type","data"} and c["type"] in SUPPORTED
        assert isinstance(c["data"], dict)

def main():
    ap = argparse.ArgumentParser(description="Normalize proof claims JSON.")
    ap.add_argument("--infile", default="./cases/bus/proof/proof_claims_sample.json")
    ap.add_argument("--out", default="./cases/bus/proof/normalized_claims.json")
    args = ap.parse_args()

    inp = json.load(open(args.infile,"r",encoding="utf-8"))
    out = normalize(inp)

    print("# ANSWER"); print(json.dumps({"n_claims": len(out["claims"])}, indent=2))
    print("\n# REASONS"); [print("-", r) for r in out["reasons"]]

    json.dump(out, open(args.out,"w",encoding="utf-8"), indent=2)
    print(f"\nWrote {args.out}")

    print("\n# CHECK"); run_harness(out); print("✔ All checks passed.")

if __name__ == "__main__":
    main()

