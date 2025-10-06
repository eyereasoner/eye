#!/usr/bin/env python3
"""
P3 — Program 3: Savings Goals Planner

Short story (header):
  If cashflow allows, where should surplus go first? The task is to allocate
  toward goals by priority (emergency fund → vacation → extra debt payment).

Overview:
  - Reads './bus/finance/forecast_30d.json' and './bus/finance/summary_month.json'.
  - Estimates monthly surplus and allocates by a simple priority rule.
  - Prints Answer/Reason/Check and writes './bus/finance/goals_plan.json' for Program 4.
"""
from __future__ import annotations

import argparse, json
from typing import Dict, Any, List

### DATA
GOALS = [
    {"id":"g1","name":"Emergency fund","target":1000.0},
    {"id":"g2","name":"Vacation","target":600.0},
    {"id":"g3","name":"Extra debt payment","target":300.0}
]

### LOGIC
def plan_goals(summary: Dict[str,Any], fc: Dict[str,Any]) -> Dict[str,Any]:
    monthly_surplus = summary["totals"]["net"]
    allocations = []
    reasons = [f"Monthly surplus estimated from Program 1: €{monthly_surplus}."]
    remaining = monthly_surplus if monthly_surplus > 0 else 0.0
    for g in GOALS:
        put = min(remaining, g["target"])
        allocations.append({"goal_id": g["id"], "goal": g["name"], "amount_eur": round(put,2)})
        remaining = round(remaining - put, 2)
        reasons.append(f"{g['name']}: allocate €{put}. Remaining surplus: €{remaining}.")
    return {"monthly_surplus_eur": round(monthly_surplus,2), "allocations": allocations, "reasons": reasons}

### CHECK
def run_harness(plan: Dict[str,Any]) -> None:
    print("Check 1 — Allocations do not exceed surplus:")
    total_alloc = round(sum(a["amount_eur"] for a in plan["allocations"]), 2)
    print(f"  - total_alloc={total_alloc} ≤ surplus={plan['monthly_surplus_eur']}")
    assert total_alloc <= plan["monthly_surplus_eur"]

    print("Check 2 — No allocation exceeds its target:")
    targets = {g["id"]: g["target"] for g in GOALS}
    for a in plan["allocations"]:
        assert a["amount_eur"] <= targets[a["goal_id"]]

def main():
    parser = argparse.ArgumentParser(description="Allocate monthly surplus to savings goals.")
    parser.add_argument("--in_forecast", default="./bus/finance/forecast_30d.json")
    parser.add_argument("--in_summary", default="./bus/finance/summary_month.json")
    parser.add_argument("--out", default="./bus/finance/goals_plan.json")
    args = parser.parse_args()

    with open(args.in_summary,"r",encoding="utf-8") as f: summary = json.load(f)
    with open(args.in_forecast,"r",encoding="utf-8") as f: fc = json.load(f)

    plan = plan_goals(summary, fc)

    print("# ANSWER")
    print(json.dumps({"monthly_surplus_eur": plan["monthly_surplus_eur"], "allocations": plan["allocations"]}, indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in plan["reasons"]:
        print("-", r)

    with open(args.out,"w",encoding="utf-8") as f:
        json.dump(plan, f, indent=2, ensure_ascii=False)
    print(f"\nWrote {args.out}")

    print("\n# CHECK (harness) — detailed")
    run_harness(plan)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()

