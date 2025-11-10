#!/usr/bin/env python3
"""
P3 — Program 5: Budget Summary & Address-Change Admin List

Short story (header):
  Last, you want to know the total cost and who to notify after the move.
  This program tallies costs and emits an address-change checklist.

Overview:
  - Reads './cases/bus/move/van_plan.json' and './cases/bus/move/packing_plan.json'.
  - Sums estimated van cost and basic packing materials cost.
  - Emits a simple list of admin tasks (utilities, internet, bank, employer, insurance).
  - Prints Answer/Reason/Check and writes './cases/bus/move/move_budget_and_admin.json'.
"""
from __future__ import annotations
import argparse, json, math

### DATA
MATERIAL_PRICES_EUR = {"tape_roll": 2.5, "bubble_wrap_per_m": 0.4}

### LOGIC
def budget_and_admin(van_plan, packing):
    mats = packing["materials"]
    materials_cost = round(mats["tape_rolls"]*MATERIAL_PRICES_EUR["tape_roll"] + mats["bubble_wrap_m"]*MATERIAL_PRICES_EUR["bubble_wrap_per_m"], 2)
    total = round(van_plan["estimated_cost_eur"] + materials_cost, 2)
    tasks = [
        "Update address: electricity/gas",
        "Update address: water",
        "Update address: internet/mobile",
        "Update address: bank & credit cards",
        "Update address: employer & payroll",
        "Update address: insurance (home/auto/health)",
        "Update address: subscriptions & online shopping"
    ]
    reasons = [
        f"Van cost ≈ €{van_plan['estimated_cost_eur']} (base + fuel).",
        f"Materials cost ≈ €{materials_cost} (tape + bubble wrap)."
    ]
    return {"materials_cost_eur": materials_cost, "van_cost_eur": van_plan["estimated_cost_eur"],
            "total_estimated_eur": total, "admin_tasks": tasks, "reasons": reasons}

### CHECK
def run_harness(res):
    print("Check 1 — Total equals parts:")
    assert round(res["van_cost_eur"] + res["materials_cost_eur"], 2) == res["total_estimated_eur"]
    print("Check 2 — Non-empty admin task list:")
    assert len(res["admin_tasks"]) >= 5

def main():
    ap = argparse.ArgumentParser(description="Summarize budget and address-change tasks.")
    ap.add_argument("--in_van", default="./cases/bus/move/van_plan.json")
    ap.add_argument("--in_packing", default="./cases/bus/move/packing_plan.json")
    ap.add_argument("--out", default="./cases/bus/move/move_budget_and_admin.json")
    args = ap.parse_args()

    with open(args.in_van,"r",encoding="utf-8") as f: van_plan = json.load(f)
    with open(args.in_packing,"r",encoding="utf-8") as f: packing = json.load(f)

    res = budget_and_admin(van_plan, packing)

    print("# ANSWER")
    print(json.dumps({k:res[k] for k in ["total_estimated_eur","van_cost_eur","materials_cost_eur"]}, indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in res["reasons"]: print("-", r)

    with open(args.out,"w",encoding="utf-8") as f: json.dump(res, f, indent=2, ensure_ascii=False)
    print(f"\nWrote {args.out}")

    print("\n# CHECK (harness) — detailed")
    run_harness(res)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()

