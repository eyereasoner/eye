
#!/usr/bin/env python3
"""
P3 — Program 2: Agency Budget Projection (Outlays vs Contributions)

Short story (header):
  The agency needs a quick forecast: if all eligible people certify weekly,
  what would the benefit outlay be, and how does that compare to contributions?

Overview:
  Consume ./cases/bus/unemp/eligible_claims.json. Project weekly and total outlays; compare with
  a simple contribution model. Print Answer/Reason/Check; write ./cases/bus/unemp/budget_projection.json.
"""
from __future__ import annotations

import argparse, json, dataclasses
from typing import Dict, Any, List

### DATA
CONTRIBUTION = {
    "employer_rate": 0.02,       # 2% of covered wage base (simplified)
    "covered_wage_base": 20000   # per worker per year for demo
}

### LOGIC
def project_budget(claims_json: Dict[str,Any]) -> Dict[str,Any]:
    elig = claims_json["eligible_claims"]
    total_weekly = round(sum(c["wba_eur"] for c in elig), 2)
    # Assume everyone draws full duration (ignoring waiting week for weekly outlay)
    total_weeks = sum(c["duration_weeks"] for c in elig)
    gross_outlay = round(total_weekly * (sum(1 for _ in elig) and (total_weeks / len(elig))), 2)

    # Contributions: pretend each worker (eligible+denied) contributes via employers at a fixed model
    population = len(claims_json["eligible_claims"]) + len(claims_json["denied_claims"])
    annual_contrib = round(population * CONTRIBUTION["covered_wage_base"] * CONTRIBUTION["employer_rate"], 2)

    reasons = [
        f"Weekly outlay = sum(WBA of eligible) = €{total_weekly}.",
        f"Total outlay = average duration × weekly outlay; assumes all weeks claimed.",
        f"Contributions = population × covered_wage_base × employer_rate = {population} × €{CONTRIBUTION['covered_wage_base']} × {CONTRIBUTION['employer_rate']:.2%}."
    ]

    return {
        "weekly_outlay_eur": total_weekly,
        "projected_total_outlay_eur": gross_outlay,
        "annual_contributions_eur": annual_contrib,
        "population_count": population,
        "reasons": reasons
    }

### CHECK
def run_harness(claims_json: Dict[str,Any], budget: Dict[str,Any]) -> None:
    print("Check 1 — Weekly outlay equals sum of WBA:")
    recompute = round(sum(c["wba_eur"] for c in claims_json["eligible_claims"]), 2)
    print(f"  - reported={budget['weekly_outlay_eur']} vs recompute={recompute}")
    assert recompute == budget["weekly_outlay_eur"]

    print("Check 2 — Total outlay uses average duration × weekly outlay:")
    total_weeks = sum(c["duration_weeks"] for c in claims_json["eligible_claims"])
    avg_weeks = (total_weeks / max(1, len(claims_json["eligible_claims"])))
    expected_total = round(budget["weekly_outlay_eur"] * avg_weeks, 2)
    print(f"  - reported={budget['projected_total_outlay_eur']} vs expected={expected_total}")
    assert expected_total == budget["projected_total_outlay_eur"]

    print("Check 3 — Contribution formula matches definition:")
    population = len(claims_json["eligible_claims"]) + len(claims_json["denied_claims"])
    expected_contrib = round(population * 20000 * 0.02, 2)
    print(f"  - reported={budget['annual_contributions_eur']} vs expected={expected_contrib}")
    assert expected_contrib == budget["annual_contributions_eur"]

def main():
    ap = argparse.ArgumentParser(description="Project unemployment agency budget.")
    ap.add_argument("--in", dest="infile", default="./cases/bus/unemp/eligible_claims.json")
    ap.add_argument("--out", default="./cases/bus/unemp/budget_projection.json")
    args = ap.parse_args()

    with open(args.infile,"r",encoding="utf-8") as f:
        claims = json.load(f)

    budget = project_budget(claims)

    print("# ANSWER")
    print(json.dumps({
        "weekly_outlay_eur": budget["weekly_outlay_eur"],
        "projected_total_outlay_eur": budget["projected_total_outlay_eur"],
        "annual_contributions_eur": budget["annual_contributions_eur"],
        "population_count": budget["population_count"]
    }, indent=2, ensure_ascii=False))

    print("\n# REASONS")
    for r in budget["reasons"]:
        print("-", r)

    with open(args.out,"w",encoding="utf-8") as f:
        json.dump(budget, f, indent=2, ensure_ascii=False)
    print(f"\nWrote {args.out}")

    print("\n# CHECK (harness) — detailed")
    run_harness(claims, budget)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()
