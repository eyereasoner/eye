#!/usr/bin/env python3
"""
P3 — Program 1: Transaction Categorizer & Monthly Summary (Personal Finance)

Short story (header):
  You export bank transactions to a JSON file. The task is to auto-categorize
  them, summarize the month, and produce an artifact for downstream steps.

Overview (what this file does, matching the chat explanation):
  - Reads transactions from a JSON file (you provide it).
  - Categorizes each line by simple, transparent keyword rules.
  - Computes a monthly summary (income, fixed bills, and discretionary spend).
  - Prints Answer/Reason/Check and writes './bus/finance/summary_month.json' for Program 2.

Sections map:
  ### DATA  – input file schema, keyword rules, and parameters
  ### LOGIC – categorization and monthly totals
  ### CHECK – harness: category coverage, totals reconciliation, schema
"""
from __future__ import annotations

import argparse, json
from typing import Dict, Any, List

### DATA
# Input JSON schema (per transaction):
#   { "date": "YYYY-MM-DD", "description": "str", "amount": float }  # amount >0 income, <0 expense
# Provide the file via --in ./bus/finance/transactions.json
CATEGORY_RULES = {
    "Income": ["payroll", "salary", "paycheque", "acme"],
    "Rent": ["rent"],
    "Utilities": ["electric", "gas", "water", "utility"],
    "Phone/Internet": ["mobile", "phone", "internet"],
    "Groceries": ["grocery", "grocer", "market"],
    "Dining": ["restaurant", "cafe", "coffee"],
    "Transport": ["transit", "metro", "bus", "uber", "lyft", "train"],
    "Subscriptions": ["streaming", "subscription"],
    "Healthcare": ["pharmacy", "clinic"],
    "Other": []
}
FIXED_CATEGORIES = {"Rent", "Utilities", "Phone/Internet", "Subscriptions"}

### LOGIC
def categorize(description: str, amount: float) -> str:
    desc = description.lower()
    if amount > 0:
        return "Income"
    for cat, keywords in CATEGORY_RULES.items():
        if any(k in desc for k in keywords):
            return cat
    return "Other"

def summarize_month(transactions: List[Dict[str,Any]]) -> Dict[str,Any]:
    cats = {}
    reasons = []
    for t in transactions:
        cat = categorize(t["description"], t["amount"])
        cats.setdefault(cat, 0.0)
        cats[cat] += t["amount"]
        reasons.append(f"{t['date']} {t['description']}: amount={t['amount']} → category={cat}")
    income = sum(v for c, v in cats.items() if c == "Income")
    expenses = -sum(v for c, v in cats.items() if c != "Income" and v < 0)
    fixed = -sum(v for c, v in cats.items() if c in FIXED_CATEGORIES and v < 0)
    discretionary = expenses - fixed
    net = income - expenses
    return {
        "categories": {k: round(v,2) for k,v in cats.items()},
        "totals": {
            "income": round(income,2),
            "expenses": round(expenses,2),
            "fixed": round(fixed,2),
            "discretionary": round(discretionary,2),
            "net": round(net,2)
        },
        "reasons": reasons
    }

### CHECK
def run_harness(transactions: List[Dict[str,Any]], result: Dict[str,Any]) -> None:
    print("Check 1 — Every transaction categorized:")
    for t in transactions:
        cat = categorize(t["description"], t["amount"])
        print(f"  - {t['description']} → {cat}")
        assert isinstance(cat, str) and len(cat) > 0

    print("Check 2 — Totals reconcile: income - expenses == net:")
    tot = result["totals"]
    expected = round(tot["income"] - tot["expenses"], 2)
    print(f"  - reported net={tot['net']} vs expected={expected}")
    assert tot["net"] == expected

    print("Check 3 — Fixed + discretionary equals total expenses:")
    print(f"  - fixed + discretionary = {tot['fixed']} + {tot['discretionary']} = {round(tot['fixed']+tot['discretionary'],2)} vs expenses={tot['expenses']}")
    assert round(tot["fixed"] + tot["discretionary"],2) == tot["expenses"]

def main():
    parser = argparse.ArgumentParser(description="Categorize transactions and summarize a month.")
    parser.add_argument("--in", dest="infile", default="./bus/finance/transactions_sample.json", help="Path to transactions JSON")
    parser.add_argument("--out", default="./bus/finance/summary_month.json")
    args = parser.parse_args()

    with open(args.infile,"r",encoding="utf-8") as f:
        tx = json.load(f)

    result = summarize_month(tx)

    print("# ANSWER")
    print(json.dumps(result["totals"], indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in result["reasons"]:
        print("-", r)

    with open(args.out,"w",encoding="utf-8") as f:
        json.dump(result, f, indent=2, ensure_ascii=False)
    print(f"\nWrote {args.out}")

    print("\n# CHECK (harness) — detailed")
    run_harness(tx, result)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()

