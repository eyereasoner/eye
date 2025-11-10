
#!/usr/bin/env python3
"""
P3 — Program 1: Hill Climb Event Eligibility (Cycling)

Short story (header):
  A ride organizer reviews a shortlist for a hill climb. The note is simple:
  “Eligible riders must meet training, power, and safety requirements.”
  The task is to decide who qualifies and record clear reasons.
"""
from __future__ import annotations

import argparse
import dataclasses
import datetime as dt
import json
from typing import List, Dict, Any

### DATA
# -----------------------------
# Data (embedded)
# -----------------------------
@dataclasses.dataclass(frozen=True)
class Rider:
    id: str
    name: str
    weight_kg: float
    best_20min_power_w: int
    recent_4wk_distance_km: float
    safety_course_passed: bool
    medical_form_on_file: bool

RIDERS: List[Rider] = [
    Rider("r1", "Ana Martins",    58.0, 210,  420.0, True,  True),
    Rider("r2", "Bram Peeters",   82.0, 250,  190.0, True,  True),
    Rider("r3", "Chi Ngu",        70.0, 210,  230.0, False, True),
    Rider("r4", "Dilan Kaya",     64.0, 180,  260.0, True,  True),
    Rider("r5", "Elena Popov",    55.0, 190,  300.0, True,  False),
    Rider("r6", "Farah Haddad",   66.0, 235,  250.0, True,  True),
    Rider("r7", "Gilles Laurent", 74.0, 260,  800.0, True,  True),
]

### LOGIC
ASOF_DEFAULT = "2025-09-18"
MIN_4WK_DISTANCE_KM = 200.0
MIN_POWER_WKG = 2.5

def wkg(power_w: int, weight_kg: float) -> float:
    return round(power_w / weight_kg, 3)

def is_eligible(r: Rider) -> (bool, Dict[str, Any]):
    w_per_kg = wkg(r.best_20min_power_w, r.weight_kg)
    ok = (
        r.recent_4wk_distance_km >= MIN_4WK_DISTANCE_KM and
        w_per_kg >= MIN_POWER_WKG and
        r.safety_course_passed and
        r.medical_form_on_file
    )
    details = {
        "wkg": w_per_kg,
        "recent_4wk_distance_km": r.recent_4wk_distance_km,
        "safety_course_passed": r.safety_course_passed,
        "medical_form_on_file": r.medical_form_on_file
    }
    return ok, details

# Primary driver: produces the Answer JSON used by downstream steps.
def solve(asof: dt.date) -> Dict[str, Any]:
    eligible, ineligible, reasons = [], [], []
    for r in RIDERS:
        ok, d = is_eligible(r)
        rec = {"id": r.id, "name": r.name, "wkg": d["wkg"], "recent_4wk_distance_km": round(d["recent_4wk_distance_km"], 1)}
        (eligible if ok else ineligible).append(rec)
        reasons.append(
            f"{r.name}: {d['wkg']} W/kg, {d['recent_4wk_distance_km']} km in 4 weeks, "
            f"safety={d['safety_course_passed']}, medical={d['medical_form_on_file']} → "
            f"{'eligible' if ok else 'not eligible'} (requires ≥{MIN_POWER_WKG} W/kg, ≥{MIN_4WK_DISTANCE_KM} km, safety & medical)."
        )
    return {
        "reference_date": asof.isoformat(),
        "policy": {"min_4wk_distance_km": MIN_4WK_DISTANCE_KM, "min_power_wkg": MIN_POWER_WKG},
        "eligible": sorted(eligible, key=lambda x: x["name"]),
        "ineligible": sorted(ineligible, key=lambda x: x["name"]),
        "reasons": reasons
    }

### CHECK
# Test harness: verifies invariants and prints detailed diagnostics.
def run_harness(result: Dict[str, Any]) -> None:
    print("Check 1 — Policy gating:")
    for r in RIDERS:
        ok, d = is_eligible(r)
        in_list = any(e["id"] == r.id for e in result["eligible"])
        print(f"  - {r.id} {r.name}: wkg={d['wkg']}, dist={d['recent_4wk_distance_km']}, safety={d['safety_course_passed']}, medical={d['medical_form_on_file']} → ok={ok}, in_list={in_list}")
        assert ok == in_list

    print("Check 2 — Threshold edges:")
    near = Rider("tmp", "Near Threshold", 60.0, int(2.5*60), 200.0, True, True)
    ok, d = is_eligible(near)
    print(f"  - Near-threshold: wkg={d['wkg']} (>=2.5), distance={d['recent_4wk_distance_km']} (>=200) → ok={ok}")
    assert ok
    below = Rider("tmp2", "Below Threshold", 60.0, int(2.49*60), 199.9, True, True)
    ok2, d2 = is_eligible(below)
    print(f"  - Below-threshold: wkg={d2['wkg']} (<2.5) or distance={d2['recent_4wk_distance_km']} (<200) → ok={ok2}")
    assert not ok2

    print("Check 3 — Output schema contains required fields:")
    for field in ("reference_date", "policy", "eligible"):
        print(f"  - field '{field}':", "present" if field in result else "MISSING")
        assert field in result

def main():
    parser = argparse.ArgumentParser(description="Compute cycling hill-climb eligibility.")
    parser.add_argument("--out", default="./cases/bus/bike/eligible_riders.json", help="Output JSON path")
    parser.add_argument("--asof", default=ASOF_DEFAULT, help="Reference date (YYYY-MM-DD)")
    args = parser.parse_args()

    asof = dt.date.fromisoformat(args.asof)
    result = solve(asof)

    print("# ANSWER")
    print(json.dumps({"eligible": result["eligible"]}, indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in result["reasons"]:
        print("-", r)

    with open(args.out, "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2, ensure_ascii=False)
    print(f"\nWrote {args.out}")

    print("\n# CHECK (harness) — detailed")
    run_harness(result)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()
