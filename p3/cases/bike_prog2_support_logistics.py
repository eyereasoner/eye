
#!/usr/bin/env python3
"""
P3 — Program 2: Support Logistics Calculator (Cycling)

Short story (header):
  The organizer plans support for the hill climb. Using the eligibility list
  and a fixed route, they estimate ride times and supplies, then verify totals.
"""
from __future__ import annotations

import argparse
import dataclasses
import json
from typing import Dict, Any, List
from collections import Counter

### DATA
# -----------------------------
# Route & Supply model
# -----------------------------
DEFAULT_ROUTE_KM = 85.0
DEFAULT_ELEV_M = 1500

def speed_from_wkg(wkg: float) -> float:
    if wkg >= 3.5: return 29.0
    if wkg >= 3.0: return 26.0
    if wkg >= 2.5: return 23.0
    return 20.0

def supplies_for_hours(hours: float) -> Dict[str, float]:
    water_l = round(hours * 0.5, 2)
    kcal = int(round(hours * 250))
    return {"water_l": water_l, "kcal": kcal}

# -----------------------------
# Data classes
# -----------------------------
### DATA
@dataclasses.dataclass(frozen=True)
class Eligible:
    id: str
    name: str
    wkg: float

### LOGIC
# Core logic: build per-rider plan (ETA/supplies) and totals from eligibility.
def compute_plan(eligibility_json: Dict[str, Any], route_km: float, elev_m: int) -> Dict[str, Any]:
    elig = [Eligible(e["id"], e["name"], float(e["wkg"])) for e in eligibility_json["eligible"]]

    per_rider: List[Dict[str, Any]] = []
    total = {"water_l": 0.0, "kcal": 0, "eta_hours_avg": 0.0}

    for e in sorted(elig, key=lambda x: -x.wkg):
        flat_speed = speed_from_wkg(e.wkg)
        time_hours = (route_km / flat_speed) * (1 + (elev_m / 1000.0) * 0.10)
        supply = supplies_for_hours(time_hours)
        per_rider.append({
            "id": e.id,
            "name": e.name,
            "wkg": round(e.wkg, 3),
            "speed_kmh_assumed": round(flat_speed, 1),
            "eta_hours": round(time_hours, 2),
            "water_l": supply["water_l"],
            "kcal": supply["kcal"],
        })
        total["water_l"] += supply["water_l"]
        total["kcal"] += supply["kcal"]
        total["eta_hours_avg"] += time_hours

    n = max(1, len(per_rider))
    total["eta_hours_avg"] = round(total["eta_hours_avg"] / n, 2)
    total["water_l"] = round(total["water_l"], 2)

    reasons = [
        "Speed model by W/kg tiers: >=3.5→29 km/h, 3.0–3.49→26, 2.5–2.99→23.",
        "Climb adjustment: time × (1 + 0.10 per 1000 m).",
        "Supplies: 0.5 L water/hour and 250 kcal/hour."
    ]
    if per_rider:
        r0 = per_rider[0]
        reasons.append(
            f"Trace: {r0['name']} at {r0['wkg']} W/kg → speed {r0['speed_kmh_assumed']} km/h; "
            f"ETA {r0['eta_hours']} h; water {r0['water_l']} L; {r0['kcal']} kcal."
        )

    return {
        "route_km": route_km,
        "elev_m": elev_m,
        "per_rider": per_rider,
        "totals": total,
        "reasons": reasons,
        "reference_date_from_p1": eligibility_json.get("reference_date", "N/A")
    }

### CHECK
# Test harness: verifies invariants and prints detailed diagnostics.
def run_harness(eligibility_json: Dict[str, Any], plan: Dict[str, Any]) -> None:
    print("Check 1 — No ineligible riders included:")
    eligible_ids = {e["id"] for e in eligibility_json["eligible"]}
    for row in plan["per_rider"]:
        print(f"  - {row['id']} in eligible_ids: {row['id'] in eligible_ids}")
        assert row["id"] in eligible_ids

    print("Check 2 — Aggregates equal sum of parts:")
    water_sum = round(sum(r["water_l"] for r in plan["per_rider"]), 2)
    kcal_sum = sum(r["kcal"] for r in plan["per_rider"])
    print(f"  - sum water = {water_sum} vs totals.water_l = {plan['totals']['water_l']}")
    print(f"  - sum kcal  = {kcal_sum} vs totals.kcal = {plan['totals']['kcal']}")
    assert abs(water_sum - plan["totals"]["water_l"]) <= 0.01
    assert kcal_sum == plan["totals"]["kcal"]

    print("Check 3 — Monotonicity: higher W/kg → same or faster ETA (non-increasing with W/kg):")
    etas = [r["eta_hours"] for r in plan["per_rider"]]
    for i in range(1, len(etas)):
        print(f"  - compare rank {i-1} to {i}: {etas[i-1]} <= {etas[i]} ?")
        assert etas[i-1] <= etas[i], "ETA should not increase for stronger riders sorted by W/kg"

    print("Check 4 — Schema essentials:")
    for field in ("per_rider", "totals", "route_km", "elev_m"):
        print(f"  - field '{field}':", "present" if field in plan else "MISSING")
        assert field in plan

def main():
    parser = argparse.ArgumentParser(description="Compute support logistics for eligible cyclists.")
    parser.add_argument("--in", dest="infile", default="./bus/bike/eligible_riders.json",
                        help="Input JSON path from Program 1")
    parser.add_argument("--route_km", type=float, default=DEFAULT_ROUTE_KM, help="Route distance (km)")
    parser.add_argument("--elev_m", type=int, default=DEFAULT_ELEV_M, help="Elevation gain (m)")
    args = parser.parse_args()

    with open(args.infile, "r", encoding="utf-8") as f:
        eligibility_json = json.load(f)

    plan = compute_plan(eligibility_json, args.route_km, args.elev_m)

    print("# ANSWER")
    print(json.dumps({"totals": plan["totals"], "per_rider": plan["per_rider"]}, indent=2, ensure_ascii=False))

    print("\n# REASONS")
    for r in plan["reasons"]:
        print("-", r)

    print("\n# CHECK (harness) — detailed")
    run_harness(eligibility_json, plan)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()
