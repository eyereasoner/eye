
#!/usr/bin/env python3
"""
P3 — Program 2: Discount Impact Calculator

Short story (header):
  Later, the totals are reviewed. Using the eligibility list from Program 1
  and a fixed discount rate, the clerk calculates the overall discount cost
  and verifies the arithmetic step by step.
"""
from __future__ import annotations

import argparse
import dataclasses
import json
from typing import Dict, Any, List

### DATA
# -----------------------------
# Data needed in addition to Program 1 output
# -----------------------------
@dataclasses.dataclass(frozen=True)
class Purchase:
    person_id: str
    gross_spend_eur: float

PURCHASES: List[Purchase] = [
    Purchase("p1", 240.00),
    Purchase("p2", 120.50),
    Purchase("p3",  80.00),
    Purchase("p4", 300.00),
    Purchase("p5",  25.00),
    Purchase("p6",  60.00),
]
DEFAULT_RATE = 0.20

### LOGIC
# Core logic: compute per-user discount amounts and totals.
def compute_discounts(eligibility_json: Dict[str, Any], rate: float) -> Dict[str, Any]:
    eligible_ids = {e["id"] for e in eligibility_json["eligible"]}
    ref_date = eligibility_json.get("reference_date", "N/A")

    per_user = []
    total_cost = 0.0
    spend_by_id = {p.person_id: p.gross_spend_eur for p in PURCHASES}

    for pid, gross in spend_by_id.items():
        is_eligible = pid in eligible_ids
        discount = (gross * rate) if is_eligible else 0.0
        net = gross - discount
        per_user.append({
            "id": pid,
            "gross_spend_eur": round(gross, 2),
            "eligible": is_eligible,
            "discount_rate": rate,
            "discount_eur": round(discount, 2),
            "net_after_discount_eur": round(net, 2),
        })
        total_cost += discount

    per_user.sort(key=lambda x: x["id"])
    reasons = [
        f"Discount formula: discount = gross_spend × rate (rate={rate:.2%}); ineligible customers receive 0. Reference date from Program 1: {ref_date}."
    ]
    for row in per_user[:2]:
        reasons.append(
            f"Trace for {row['id']}: gross={row['gross_spend_eur']} × rate={rate:.2%} → discount={row['discount_eur']} (eligible={row['eligible']})."
        )

    return {
        "reference_date_from_p1": ref_date,
        "discount_rate": rate,
        "per_user": per_user,
        "total_discount_cost_eur": round(total_cost, 2),
        "reasons": reasons,
    }

### CHECK
# Test harness: verifies invariants and prints detailed diagnostics.
def run_harness(eligibility_json: Dict[str, Any], result: Dict[str, Any]) -> None:
    eligible_ids = {e["id"] for e in eligibility_json["eligible"]}
    per_user = result["per_user"]
    rate = result["discount_rate"]

    print("Check 1 — Eligibility gate on discounts:")
    for row in per_user:
        gate_ok = (row["id"] in eligible_ids) or (row["discount_eur"] == 0.0)
        print(f"  - {row['id']}: eligible={row['id'] in eligible_ids}, discount={row['discount_eur']} → gate_ok={gate_ok}")
        if row["id"] not in eligible_ids:
            assert row["discount_eur"] == 0.0

    print("Check 2 — Aggregate equals sum of parts:")
    total_from_parts = round(sum(r["discount_eur"] for r in per_user), 2)
    print(f"  - sum(per_user discounts) = {total_from_parts}")
    print(f"  - reported total          = {result['total_discount_cost_eur']}")
    assert abs(total_from_parts - result["total_discount_cost_eur"]) <= 0.01

    print("Check 3 — Vectorized inner-product recomputation:")
    spend_by_id = {p.person_id: p.gross_spend_eur for p in PURCHASES}
    eligible_spend = sum(spend_by_id.get(pid, 0.0) for pid in eligible_ids)
    vectorized_total = round(eligible_spend * rate, 2)
    print(f"  - eligible gross spend × rate = {eligible_spend:.2f} × {rate:.2%} = {vectorized_total}")
    print(f"  - reported total              = {result['total_discount_cost_eur']}")
    assert abs(vectorized_total - result["total_discount_cost_eur"]) <= 0.01

    print("Check 4 — Output schema essentials:")
    for field in ("per_user", "total_discount_cost_eur", "discount_rate"):
        print(f"  - field '{field}':", "present" if field in result else "MISSING")
        assert field in result

def main():
    parser = argparse.ArgumentParser(description="Compute discount impact from eligibility JSON.")
    parser.add_argument("--in", dest="infile", default="./bus/shop/eligible_customers.json",
                        help="Input JSON path from Program 1")
    parser.add_argument("--rate", type=float, default=DEFAULT_RATE, help="Discount rate (0..1)")
    args = parser.parse_args()

    with open(args.infile, "r", encoding="utf-8") as f:
        eligibility_json = json.load(f)

    result = compute_discounts(eligibility_json, args.rate)

    print("# ANSWER")
    print(json.dumps({
        "total_discount_cost_eur": result["total_discount_cost_eur"],
        "per_user": result["per_user"]
    }, indent=2, ensure_ascii=False))

    print("\n# REASONS")
    for r in result["reasons"]:
        print("-", r)

    print("\n# CHECK (harness) — detailed")
    run_harness(eligibility_json, result)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()
