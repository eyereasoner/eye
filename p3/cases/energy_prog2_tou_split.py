#!/usr/bin/env python3
"""
P3 — Program 2: Time-of-Use Split (Peak vs Off-peak)

Short story:
  With a daily baseline, split usage into peak and off-peak portions using a
  simple hour model (typical_hour and shiftability).

Overview:
  - Reads './bus/energy/baseline_daily.json'.
  - Splits kWh/day into peak vs off-peak by each appliance.
  - Prints Answer/Reasons/Check; writes './bus/energy/tou_split.json' for Program 3.
"""
from __future__ import annotations
import argparse, json
from typing import Dict, Any, List, Tuple

def split_appliance(row: Dict[str,Any], offpeak_hours: List[int]) -> Tuple[float,float,str]:
    kwh = row["kwh_per_day"]; hour = row["typical_hour"]; shiftable = row["shiftable"]
    if kwh == 0: return 0.0, 0.0, "zero usage"
    if hour in offpeak_hours:
        return 0.0, kwh, "naturally off-peak"
    if shiftable:
        # assume 70% can be shifted to off-peak
        return round(kwh*0.30,3), round(kwh*0.70,3), "70% shifted off-peak"
    return kwh, 0.0, "not shiftable (peak)"

def build_tou(base: Dict[str,Any]) -> Dict[str,Any]:
    off = set(base["tou_offpeak_hours"])
    rows = []
    reasons = []
    for a in base["appliances_daily"]:
        p, o, why = split_appliance(a, list(off))
        rows.append({"id":a["id"], "name":a["name"], "peak_kwh":p, "offpeak_kwh":o})
        reasons.append(f"{a['name']}: peak={p}, off-peak={o} ({why})")
    peak = round(sum(r["peak_kwh"] for r in rows),3)
    offp = round(sum(r["offpeak_kwh"] for r in rows),3)
    return {"rows": rows, "peak_kwh": peak, "offpeak_kwh": offp, "reasons": reasons,
            "tou_multiplier_peak": base["tou_multiplier_peak"], "tou_multiplier_offpeak": base["tou_multiplier_offpeak"],
            "tariff_eur_per_kwh": base["tariff_eur_per_kwh"], "co2_kg_per_kwh": base["co2_kg_per_kwh"]}

def run_harness(t: Dict[str,Any], base: Dict[str,Any]) -> None:
    print("Check — peak+offpeak equals baseline total (±0.001):")
    total = round(t["peak_kwh"] + t["offpeak_kwh"], 3)
    print(f"  split={total} vs baseline={base['total_kwh_per_day']}")
    assert abs(total - base["total_kwh_per_day"]) <= 0.001

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--in", default="./bus/energy/baseline_daily.json")
    ap.add_argument("--out", default="./bus/energy/tou_split.json")
    args = ap.parse_args()

    in_path = getattr(args, "in")  # <-- fix
    with open(in_path, "r", encoding="utf-8") as f:
        base = json.load(f)
    t = build_tou(base)

    print("# ANSWER"); print(json.dumps({"peak_kwh":t["peak_kwh"],"offpeak_kwh":t["offpeak_kwh"]}, indent=2))
    print("\n# REASONS"); [print("-", r) for r in t["reasons"]]

    json.dump(t, open(args.out,"w",encoding="utf-8"), indent=2)
    print(f"\nWrote {args.out}")

    print("\n# CHECK"); run_harness(t, base); print("✔ All checks passed.")

if __name__ == "__main__":
    main()

