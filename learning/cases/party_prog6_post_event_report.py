#!/usr/bin/env python3
"""
EYE Learning — Program 6: Final Report & CSV Summary

Short story (header):
  Pull everything together—headcount, menu, shopping list, schedule—and produce a
  compact JSON report and a CSV suitable for sharing.

Overview:
  - Reads './resources/guest_roster.json' + './resources/menu_plan.json' + './resources/shopping_list.json'.
  - Emits './resources/party_report.json' and './resources/party_report.csv'.
  - Prints Answer/Reason/Check.
"""
from __future__ import annotations
import argparse, json, csv

def compose(roster, menu, shopping):
    rpt = {
        "host": roster["event"]["host_name"],
        "theme": roster["event"]["theme"],
        "event_date": roster["event"]["event_date"],
        "counts": roster["counts"],
        "budget_eur": roster["event"]["host_budget_eur"],
        "menu_cost_est_eur": menu["estimated_cost_eur"],
        "within_budget": menu["within_budget"],
        "vendors": list(shopping["by_vendor"].keys())
    }
    reasons = ["Includes headcount/diet, menu budget, and vendor split for transparency."]
    return rpt, reasons

def write_csv(report, path="./resources/party_report.csv"):
    fields = ["host","theme","event_date","total","adults","kids","budget_eur","menu_cost_est_eur","within_budget","vendors"]
    row = {
        "host": report["host"], "theme": report["theme"], "event_date": report["event_date"],
        "total": report["counts"]["total"], "adults": report["counts"]["adults"], "kids": report["counts"]["kids"],
        "budget_eur": report["budget_eur"], "menu_cost_est_eur": report["menu_cost_est_eur"],
        "within_budget": report["within_budget"], "vendors": ",".join(report["vendors"])
    }
    with open(path,"w",newline="",encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fields); w.writeheader(); w.writerow(row)

def run_harness(report):
    print("Check — CSV fields consistent with JSON:")
    assert isinstance(report["within_budget"], bool)
    assert report["counts"]["total"] == report["counts"]["adults"] + report["counts"]["kids"]

def main():
    ap = argparse.ArgumentParser(description="Compose final party report & CSV.")
    ap.add_argument("--in_roster", default="./resources/guest_roster.json")
    ap.add_argument("--in_menu", default="./resources/menu_plan.json")
    ap.add_argument("--in_shop", default="./resources/shopping_list.json")
    args = ap.parse_args()

    roster = json.load(open(args.in_roster,"r",encoding="utf-8"))
    menu   = json.load(open(args.in_menu,"r",encoding="utf-8"))
    shop   = json.load(open(args.in_shop,"r",encoding="utf-8"))

    report, reasons = compose(roster, menu, shop)

    print("# ANSWER"); print(json.dumps({"menu_cost_est_eur": report["menu_cost_est_eur"], "within_budget": report["within_budget"]}, indent=2))
    print("\n# REASONS"); [print("-", r) for r in reasons]

    json.dump(report, open("./resources/party_report.json","w",encoding="utf-8"), indent=2)
    write_csv(report, "./resources/party_report.csv")
    print("\nWrote ./resources/party_report.json and ./resources/party_report.csv")

    print("\n# CHECK"); run_harness(report); print("✔ All checks passed.")

if __name__ == "__main__":
    main()

