#!/usr/bin/env python3
"""
P3 — Program 6: Final Report & CSV Summary

Short story (header):
  Pull everything together—headcount, menu, shopping list—and produce a compact
  JSON report and a CSV suitable for sharing.

Sections map & intent:
  ### DATA
      - Key fields to summarize (counts, budget, estimated menu cost, vendor list)
  ### LOGIC
      - Compose report object; write CSV with same essentials
  ### CHECK (harness)
      - Consistency of counts; within_budget boolean; vendors align with shopping list
      - CSV round-trip sanity for critical fields
"""

from __future__ import annotations
import argparse, json, csv, io

### LOGIC ----------------------------------------------------------------------
def compose(roster, menu, shopping):
    rpt = {
        "schema": "party_report.v1",
        "host": roster["event"]["host_name"],
        "theme": roster["event"]["theme"],
        "event_date": roster["event"]["event_date"],
        "counts": roster["counts"],
        "budget_eur": roster["event"]["host_budget_eur"],
        "menu_cost_est_eur": menu["estimated_cost_eur"],
        "within_budget": bool(menu["within_budget"]),
        "vendors": sorted(list(shopping["by_vendor"].keys()))
    }
    reasons = ["Includes headcount/diets summary, menu budget, and vendor split."]
    return rpt, reasons

def write_csv(report, path="party_report.csv"):
    fields = ["host","theme","event_date","total","adults","kids","budget_eur","menu_cost_est_eur","within_budget","vendors"]
    row = {
        "host": report["host"], "theme": report["theme"], "event_date": report["event_date"],
        "total": report["counts"]["total"], "adults": report["counts"]["adults"], "kids": report["counts"]["kids"],
        "budget_eur": report["budget_eur"], "menu_cost_est_eur": report["menu_cost_est_eur"],
        "within_budget": report["within_budget"], "vendors": ",".join(report["vendors"])
    }
    with open(path, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fields); w.writeheader(); w.writerow(row)

### CHECK (harness) ------------------------------------------------------------
def run_harness(report, shopping):
    print("Check 1 — Counts consistent & budget types ok:")
    c = report["counts"]
    assert c["total"] == c["adults"] + c["kids"]
    assert isinstance(report["within_budget"], bool)
    assert report["budget_eur"] >= 0 and report["menu_cost_est_eur"] >= 0

    print("Check 2 — Vendors align with shopping list keys:")
    assert sorted(report["vendors"]) == sorted(list(shopping["by_vendor"].keys()))

    print("Check 3 — CSV round-trip of key fields:")
    # Serialize to memory and read back to verify basic integrity
    fields = ["host","theme","event_date","total","adults","kids","budget_eur","menu_cost_est_eur","within_budget","vendors"]
    sio = io.StringIO()
    w = csv.DictWriter(sio, fieldnames=fields); w.writeheader()
    w.writerow({
        "host": report["host"], "theme": report["theme"], "event_date": report["event_date"],
        "total": report["counts"]["total"], "adults": report["counts"]["adults"], "kids": report["counts"]["kids"],
        "budget_eur": report["budget_eur"], "menu_cost_est_eur": report["menu_cost_est_eur"],
        "within_budget": report["within_budget"], "vendors": ",".join(report["vendors"])
    })
    sio.seek(0)
    r = list(csv.DictReader(sio))[0]
    assert r["host"] == report["host"] and r["theme"] == report["theme"]
    assert int(r["total"]) == report["counts"]["total"]

def main():
    ap = argparse.ArgumentParser(description="Compose final party report & CSV.")
    ap.add_argument("--in_roster", default="./cases/bus/party/guest_roster.json")
    ap.add_argument("--in_menu", default="./cases/bus/party/menu_plan.json")
    ap.add_argument("--in_shop", default="./cases/bus/party/shopping_list.json")
    args = ap.parse_args()

    roster = json.load(open(args.in_roster, "r", encoding="utf-8"))
    menu   = json.load(open(args.in_menu, "r", encoding="utf-8"))
    shop   = json.load(open(args.in_shop, "r", encoding="utf-8"))

    report, reasons = compose(roster, menu, shop)

    print("# ANSWER")
    print(json.dumps({"menu_cost_est_eur": report["menu_cost_est_eur"], "within_budget": report["within_budget"]}, indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in reasons:
        print("-", r)

    json.dump(report, open("./cases/bus/party/party_report.json", "w", encoding="utf-8"), indent=2, ensure_ascii=False)
    write_csv(report, "./cases/bus/party/party_report.csv")
    print("\nWrote ./cases/bus/party/party_report.json and ./cases/bus/party/party_report.csv")

    print("\n# CHECK (harness) — detailed")
    run_harness(report, shop)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()

