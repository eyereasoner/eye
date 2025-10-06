#!/usr/bin/env python3
"""
P3 — Program 2: Cashflow Forecast (Next 30 Days)

Short story (header):
  After summarizing the month, the next concern is runway. The task is to
  forecast the next 30 days using a simple model of paydays and bills.

Overview (matches the chat explanation):
  - Reads './bus/finance/summary_month.json' from Program 1.
  - Applies a simple rule for upcoming paydays (1st and 15th, from recent income).
  - Projects weekly bills from fixed categories and flags low-balance risks.
  - Prints Answer/Reason/Check and writes './bus/finance/forecast_30d.json' for Program 3.

Sections map: DATA / LOGIC / CHECK
"""
from __future__ import annotations

import argparse, json, datetime as dt
from typing import Dict, Any

### DATA
DEFAULT_START_BALANCE = 500.00  # can override via CLI

### LOGIC
def forecast_30d(summary: Dict[str,Any], start_balance: float, asof: dt.date) -> Dict[str,Any]:
    monthly_fixed = summary["totals"]["fixed"]
    weekly_fixed = round(monthly_fixed / 4.0, 2)
    income_month = summary["totals"]["income"]
    pay_per = round(income_month / 2.0, 2) if income_month > 0 else 0.0

    days = []
    balance = start_balance
    reasons = []
    for i in range(30):
        day = asof + dt.timedelta(days=i)
        inflow = 0.0
        outflow = 0.0
        if day.day in (1, 15):
            inflow += pay_per
        if day.weekday() == 0:  # Monday
            outflow += weekly_fixed
        balance = round(balance + inflow - outflow, 2)
        days.append({"date": day.isoformat(), "inflow": inflow, "outflow": outflow, "balance": balance})
        if inflow > 0: reasons.append(f"{day.isoformat()}: payday +€{inflow}")
        if outflow > 0: reasons.append(f"{day.isoformat()}: weekly fixed −€{outflow}")

    low_risk = any(d["balance"] < 100.0 for d in days)
    return {"as_of": asof.isoformat(), "start_balance": start_balance, "days": days, "low_balance_risk": low_risk, "reasons": reasons}

### CHECK
def run_harness(summary: Dict[str,Any], fc: Dict[str,Any]) -> None:
    print("Check 1 — Start balance applied correctly:")
    first = fc["days"][0]
    inflow0 = first["inflow"]; outflow0 = first["outflow"]
    expected0 = round(fc["start_balance"] + inflow0 - outflow0, 2)
    print(f"  - day0 balance={first['balance']} vs expected={expected0}")
    assert first["balance"] == expected0

    print("Check 2 — Weekly fixed allocation matches Mondays in the 30-day horizon:")
    monthly_fixed = summary["totals"]["fixed"]
    weekly_fixed = round(monthly_fixed / 4.0, 2)

    # Count Mondays in the 30-day horizon from fc["as_of"]
    asof = dt.date.fromisoformat(fc["as_of"])
    mondays = sum(1 for i in range(30) if (asof + dt.timedelta(days=i)).weekday() == 0)

    total_weekly = round(sum(d["outflow"] for d in fc["days"] if d["outflow"] > 0), 2)
    expected_total = round(weekly_fixed * mondays, 2)

    print(f"  - mondays={mondays}, weekly_fixed=€{weekly_fixed} "
          f"→ expected_outflow=€{expected_total}, reported=€{total_weekly}")

    # Allow tiny rounding drift
    assert abs(total_weekly - expected_total) <= 0.02

def main():
    parser = argparse.ArgumentParser(description="Forecast cashflow for 30 days.")
    parser.add_argument("--in", dest="infile", default="./bus/finance/summary_month.json")
    parser.add_argument("--out", default="./bus/finance/forecast_30d.json")
    parser.add_argument("--start_balance", type=float, default=DEFAULT_START_BALANCE)
    parser.add_argument("--asof", default="2025-09-18")
    args = parser.parse_args()

    with open(args.infile,"r",encoding="utf-8") as f:
        summary = json.load(f)

    asof = dt.date.fromisoformat(args.asof)
    fc = forecast_30d(summary, args.start_balance, asof)

    print("# ANSWER")
    print(json.dumps({"low_balance_risk": fc["low_balance_risk"], "start_balance": fc["start_balance"]}, indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in fc["reasons"][:8]:
        print("-", r)

    with open(args.out,"w",encoding="utf-8") as f:
        json.dump(fc, f, indent=2, ensure_ascii=False)
    print(f"\nWrote {args.out}")

    print("\n# CHECK (harness) — detailed")
    run_harness(summary, fc)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()

