#!/usr/bin/env python3
"""
P3 — Program 3: Cost & CO₂ Calculator (with TOU multipliers)

Short story:
  Apply tariff and TOU multipliers to peak/off-peak energy to get daily cost and
  CO₂. This reveals savings from shifting load.

Overview:
  - Reads './cases/bus/energy/tou_split.json'.
  - Applies multipliers: cost = kWh × tariff × (peak|off-peak multiplier).
  - Prints Answer/Reasons/Check; writes './cases/bus/energy/bill_breakdown.json' for Program 4.
"""
from __future__ import annotations
import argparse, json

def compute(split):
    t = split["tariff_eur_per_kwh"]; c = split["co2_kg_per_kwh"]
    mp = split["tou_multiplier_peak"]; mo = split["tou_multiplier_offpeak"]
    peak_cost = round(split["peak_kwh"]*t*mp, 2)
    off_cost  = round(split["offpeak_kwh"]*t*mo, 2)
    total_cost = round(peak_cost + off_cost, 2)
    total_co2  = round((split["peak_kwh"]+split["offpeak_kwh"]) * c, 2)
    reasons = [
        f"Peak: {split['peak_kwh']}×€{t}×{mp} = €{peak_cost}",
        f"Off-peak: {split['offpeak_kwh']}×€{t}×{mo} = €{off_cost}"
    ]
    return {"peak_cost_eur":peak_cost,"offpeak_cost_eur":off_cost,"total_cost_eur":total_cost,"total_co2_kg":total_co2,"reasons":reasons}

def run_harness(b):
    print("Check — totals equal parts; non-negative:")
    assert b["total_cost_eur"] == round(b["peak_cost_eur"]+b["offpeak_cost_eur"],2)
    assert b["total_cost_eur"] >= 0 and b["total_co2_kg"] >= 0

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--in", default="./cases/bus/energy/tou_split.json")
    ap.add_argument("--out", default="./cases/bus/energy/bill_breakdown.json")
    args = ap.parse_args()

    in_path = getattr(args, "in")  # <-- fix
    with open(in_path, "r", encoding="utf-8") as f:
        split = json.load(f)
    b = compute(split)

    print("# ANSWER"); print(json.dumps({k:b[k] for k in ["total_cost_eur","total_co2_kg"]}, indent=2))
    print("\n# REASONS"); [print("-", r) for r in b["reasons"]]

    json.dump(b, open(args.out,"w",encoding="utf-8"), indent=2)
    print(f"\nWrote {args.out}")

    print("\n# CHECK"); run_harness(b); print("✔ All checks passed.")

if __name__ == "__main__":
    main()

