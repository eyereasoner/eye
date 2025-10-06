#!/usr/bin/env python3
"""
P3 — Program 1: Ingest & Baseline (kWh/day)

Short story:
  You export a small JSON of appliances and tariffs. The task is to compute a
  clear daily baseline in kWh and cost, and pass it to later steps.

Overview (matches this chat):
  - Reads './bus/energy/house_usage_sample.json' (or your file via --in).
  - Normalizes per-appliance usage to kWh/day.
  - Computes daily cost and daily CO₂.
  - Prints Answer/Reasons/Check; writes './bus/energy/baseline_daily.json' for Program 2.

Sections map:
  ### DATA  – input schema & TOU params
  ### LOGIC – normalize to kWh/day + totals
  ### CHECK – sum consistency & schema
"""
from __future__ import annotations
import argparse, json
from typing import Dict, Any, List

### DATA
def kwh_per_day(appl: Dict[str,Any]) -> float:
    if "hours_per_day" in appl:
        return appl["watt"] * appl["hours_per_day"] / 1000.0
    if "hours_per_week" in appl:
        return appl["watt"] * (appl["hours_per_week"]/7.0) / 1000.0
    return 0.0

### LOGIC
def build_baseline(cfg: Dict[str,Any]) -> Dict[str,Any]:
    rows = []
    for a in cfg["appliances"]:
        kwh = round(kwh_per_day(a), 3)
        rows.append({
            "id": a["id"], "name": a["name"], "kwh_per_day": kwh,
            "shiftable": bool(a.get("shiftable", False)),
            "typical_hour": int(a.get("typical_hour", 12))
        })
    total_kwh = round(sum(r["kwh_per_day"] for r in rows), 3)
    cost_eur = round(total_kwh * cfg["tariff_eur_per_kwh"], 2)
    co2_kg   = round(total_kwh * cfg["co2_kg_per_kwh"], 2)
    reasons = [f"{r['name']}: {r['kwh_per_day']} kWh/day" for r in rows]
    return {
        "household": cfg["household"],
        "tariff_eur_per_kwh": cfg["tariff_eur_per_kwh"],
        "co2_kg_per_kwh": cfg["co2_kg_per_kwh"],
        "tou_offpeak_hours": cfg["tou_offpeak_hours"],
        "tou_multiplier_peak": cfg["tou_multiplier_peak"],
        "tou_multiplier_offpeak": cfg["tou_multiplier_offpeak"],
        "appliances_daily": rows,
        "total_kwh_per_day": total_kwh,
        "daily_cost_eur": cost_eur,
        "daily_co2_kg": co2_kg,
        "reasons": reasons
    }

### CHECK
def run_harness(b: Dict[str,Any]) -> None:
    print("Check — totals equal sum of parts:")
    s = round(sum(r["kwh_per_day"] for r in b["appliances_daily"]), 3)
    print(f"  sum={s} vs total={b['total_kwh_per_day']}")
    assert s == b["total_kwh_per_day"]
    # quick schema presence
    for k in ["tariff_eur_per_kwh","co2_kg_per_kwh","appliances_daily"]: assert k in b

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--in", dest="infile", default="./bus/energy/house_usage_sample.json")
    ap.add_argument("--out", default="./bus/energy/baseline_daily.json")
    args = ap.parse_args()

    cfg = json.load(open(args.infile, "r", encoding="utf-8"))
    base = build_baseline(cfg)

    print("# ANSWER"); print(json.dumps({
        "total_kwh_per_day": base["total_kwh_per_day"],
        "daily_cost_eur": base["daily_cost_eur"],
        "daily_co2_kg": base["daily_co2_kg"]
    }, indent=2))
    print("\n# REASONS"); [print("-", r) for r in base["reasons"]]

    json.dump(base, open(args.out,"w",encoding="utf-8"), indent=2)
    print(f"\nWrote {args.out}")

    print("\n# CHECK"); run_harness(base); print("✔ All checks passed.")

if __name__ == "__main__":
    main()

