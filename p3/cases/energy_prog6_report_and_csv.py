#!/usr/bin/env python3
"""
P3 — Program 6: Final Report & CSV Export

Short story:
  Last step: put it all together—baseline, TOU split, costs, and savings—so you
  can share a simple CSV and a compact JSON report.

Overview:
  - Reads './bus/energy/baseline_daily.json' + './bus/energy/tou_split.json' + './bus/energy/bill_breakdown.json' + './bus/energy/savings_plan.json'.
  - Emits './bus/energy/energy_report.json' and './bus/energy/energy_report.csv'.
  - Prints Answer/Reasons/Check.
"""
from __future__ import annotations
import argparse, json, csv

def compose(base, split, bill, plan):
    report = {
        "household": base["household"],
        "total_kwh_per_day": base["total_kwh_per_day"],
        "peak_kwh": split["peak_kwh"],
        "offpeak_kwh": split["offpeak_kwh"],
        "daily_cost_eur": bill["total_cost_eur"],
        "daily_co2_kg": bill["total_co2_kg"],
        "savings_daily_eur": plan["daily_savings_eur"],
        "savings_daily_co2_kg": plan["daily_co2_delta_kg"],
        "components": plan["components"]
    }
    reasons = [
        "Includes baseline energy, TOU split, billed cost/CO₂, and savings estimates.",
        "CSV exported with the same key fields for spreadsheets."
    ]
    return report, reasons

def write_csv(report, path="energy_report.csv"):
    fields = ["household","total_kwh_per_day","peak_kwh","offpeak_kwh","daily_cost_eur","daily_co2_kg","savings_daily_eur","savings_daily_co2_kg"]
    with open(path,"w",newline="",encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fields); w.writeheader(); w.writerow({k:report[k] for k in fields})

def run_harness(report):
    print("Check — basic invariants:")
    assert report["total_kwh_per_day"] == round(report["peak_kwh"]+report["offpeak_kwh"],3)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--in_base", default="./bus/energy/baseline_daily.json")
    ap.add_argument("--in_split", default="./bus/energy/tou_split.json")
    ap.add_argument("--in_bill", default="./bus/energy/bill_breakdown.json")
    ap.add_argument("--in_plan", default="./bus/energy/savings_plan.json")
    args = ap.parse_args()

    base  = json.load(open(args.in_base,"r",encoding="utf-8"))
    split = json.load(open(args.in_split,"r",encoding="utf-8"))
    bill  = json.load(open(args.in_bill,"r",encoding="utf-8"))
    plan  = json.load(open(args.in_plan,"r",encoding="utf-8"))

    report, reasons = compose(base, split, bill, plan)

    print("# ANSWER"); print(json.dumps({k:report[k] for k in ["daily_cost_eur","savings_daily_eur"]}, indent=2))
    print("\n# REASONS"); [print("-", r) for r in reasons]

    json.dump(report, open("./bus/energy/energy_report.json","w",encoding="utf-8"), indent=2)
    write_csv(report, "./bus/energy/energy_report.csv")
    print("\nWrote ./bus/energy/energy_report.json and ./bus/energy/energy_report.csv")

    print("\n# CHECK"); run_harness(report); print("✔ All checks passed.")

if __name__ == "__main__":
    main()

