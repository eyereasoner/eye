#!/usr/bin/env python3
"""
P3 — Program 3: Van Size & Trip Estimator

Short story (header):
  Now decide how big a van you need and how many trips it’ll take. The program
  uses total volume and distance to propose a sensible option and estimate time/cost.

Overview:
  - Reads './cases/bus/move/inventory_decisions.json' and uses kept volume/weight.
  - Chooses van class by capacity; estimates trips = ceil(total_volume / van_capacity).
  - Estimates fuel and time from a simple distance and speed assumption.
  - Prints Answer/Reason/Check and writes './cases/bus/move/van_plan.json'.
"""
from __future__ import annotations
import argparse, json, math
from typing import Dict, Any

### DATA
VANS = {
    "small":  {"capacity_m3": 3.0,  "fuel_l_per_100km": 7.5, "base_cost_eur": 60},
    "medium": {"capacity_m3": 8.0,  "fuel_l_per_100km": 9.0, "base_cost_eur": 80},
    "large":  {"capacity_m3": 12.0, "fuel_l_per_100km": 11.0,"base_cost_eur": 100}
}
AVG_SPEED_KMH = 40  # city mixed, including loading margin

### LOGIC
def choose_van(total_vol_m3: float) -> str:
    for name, spec in VANS.items():
        if total_vol_m3 <= spec["capacity_m3"]:
            return name
    return "large"

def plan_van(inv: Dict[str,Any], distance_km: float) -> Dict[str,Any]:
    vol = inv["totals"]["kept_volume_m3"]
    van = choose_van(vol)
    cap = VANS[van]["capacity_m3"]
    trips = max(1, math.ceil(vol / cap))
    # round trip distance for each trip (assume same distance both ways)
    total_km = distance_km * 2 * trips
    fuel_l = round(total_km * VANS[van]["fuel_l_per_100km"] / 100.0, 1)
    hours = round(total_km / AVG_SPEED_KMH, 2)
    cost = VANS[van]["base_cost_eur"] * trips + fuel_l * 1.8  # €1.8/L placeholder
    reasons = [
        f"total_volume={vol}m³; choose van={van} (cap={cap}m³).",
        f"trips=ceil({vol}/{cap})={trips}; distance total={total_km}km; fuel≈{fuel_l}L; time≈{hours}h."
    ]
    return {"van": van, "trips": trips, "total_km": total_km, "fuel_l": fuel_l,
            "time_hours": hours, "estimated_cost_eur": round(cost,2), "reasons": reasons}

### CHECK
def run_harness(inv: Dict[str,Any], plan: Dict[str,Any]) -> None:
    print("Check 1 — Trips cover volume:")
    cap = VANS[plan["van"]]["capacity_m3"]
    assert plan["trips"] * cap + 1e-9 >= inv["totals"]["kept_volume_m3"]
    print("Check 2 — Non-negative time/cost/fuel:")
    assert plan["time_hours"] >= 0 and plan["estimated_cost_eur"] >= 0 and plan["fuel_l"] >= 0

def main():
    ap = argparse.ArgumentParser(description="Estimate van size, trips, time and cost.")
    ap.add_argument("--in", dest="infile", default="./cases/bus/move/inventory_decisions.json")
    ap.add_argument("--distance_km", type=float, default=12.0, help="one-way km between homes")
    ap.add_argument("--out", default="./cases/bus/move/van_plan.json")
    args = ap.parse_args()

    with open(args.infile,"r",encoding="utf-8") as f: inv = json.load(f)
    plan = plan_van(inv, args.distance_km)

    print("# ANSWER")
    print(json.dumps({k:plan[k] for k in ["van","trips","estimated_cost_eur","time_hours"]}, indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in plan["reasons"]: print("-", r)

    with open(args.out,"w",encoding="utf-8") as f: json.dump(plan, f, indent=2, ensure_ascii=False)
    print(f"\nWrote {args.out}")

    print("\n# CHECK (harness) — detailed")
    run_harness(inv, plan)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()

