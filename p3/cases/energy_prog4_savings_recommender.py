#!/usr/bin/env python3
"""
P3 — Program 4: Savings Recommender (Shifts & Upgrades)

Short story:
  Now suggest practical changes: shift when possible and consider simple
  efficiency upgrades; show the estimated daily € and CO₂ savings.

Overview:
  - Reads './cases/bus/energy/baseline_daily.json' + './cases/bus/energy/tou_split.json' + './cases/bus/energy/bill_breakdown.json'.
  - Suggests: (a) shift all shiftable loads to off-peak; (b) 10% efficiency for fridge/TV.
  - Prints Answer/Reasons/Check; writes './cases/bus/energy/savings_plan.json' for Program 5.
"""
from __future__ import annotations
import argparse, json

EFF_UPGRADE_IDS = {"Fridge","TV"}  # apply 10% reduction to these if present

def recommend(base, split, bill):
    t = base["tariff_eur_per_kwh"]; c = base["co2_kg_per_kwh"]; mo = base["tou_multiplier_offpeak"]
    # Scenario A: shift all shiftable → everything off-peak
    shiftable_kwh = sum(a["kwh_per_day"] for a in base["appliances_daily"] if a["shiftable"])
    delta_cost_shift = round(shiftable_kwh * t * (mo - 1.0), 2)  # negative (savings)
    # Scenario B: 10% efficiency on selected ids by name
    eff_kwh = 0.0
    for a in base["appliances_daily"]:
        if a["name"] in EFF_UPGRADE_IDS:
            eff_kwh += a["kwh_per_day"] * 0.10
    delta_cost_eff = round(-eff_kwh * t * ((split["peak_kwh"]>0) and split["tou_multiplier_peak"] or mo), 2)
    delta_co2 = round(-(shiftable_kwh*0 + eff_kwh) * c, 2)  # CO2 drops only with efficiency, not timing (simplified)

    total_savings = round(delta_cost_shift + delta_cost_eff, 2)
    reasons = [
        f"Shift all shiftable ({round(shiftable_kwh,3)} kWh/day) → €{delta_cost_shift} per day.",
        f"10% efficiency on {', '.join(sorted(EFF_UPGRADE_IDS))} saves ~{round(eff_kwh,3)} kWh/day → €{delta_cost_eff} per day.",
        f"CO₂ reduction from efficiency: {delta_co2} kg/day."
    ]
    return {"daily_savings_eur": total_savings, "daily_co2_delta_kg": delta_co2,
            "components": {"shift_savings_eur": delta_cost_shift, "efficiency_savings_eur": delta_cost_eff},
            "reasons": reasons}

def run_harness(plan):
    print("Check — savings not positive due to sign convention:")
    # savings expressed as negative deltas (e.g., -0.35 €/day). Total_savings should be <= 0.
    assert plan["daily_savings_eur"] <= 0

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--in_base", default="./cases/bus/energy/baseline_daily.json")
    ap.add_argument("--in_split", default="./cases/bus/energy/tou_split.json")
    ap.add_argument("--in_bill", default="./cases/bus/energy/bill_breakdown.json")
    ap.add_argument("--out", default="./cases/bus/energy/savings_plan.json")
    args = ap.parse_args()

    base = json.load(open(args.in_base,"r",encoding="utf-8"))
    split = json.load(open(args.in_split,"r",encoding="utf-8"))
    bill  = json.load(open(args.in_bill,"r",encoding="utf-8"))
    plan  = recommend(base, split, bill)

    print("# ANSWER"); print(json.dumps({k:plan[k] for k in ["daily_savings_eur","daily_co2_delta_kg"]}, indent=2))
    print("\n# REASONS"); [print("-", r) for r in plan["reasons"]]

    json.dump(plan, open(args.out,"w",encoding="utf-8"), indent=2)
    print(f"\nWrote {args.out}")

    print("\n# CHECK"); run_harness(plan); print("✔ All checks passed.")

if __name__ == "__main__":
    main()

