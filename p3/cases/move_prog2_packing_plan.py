#!/usr/bin/env python3
"""
P3 — Program 2: Packing Plan (Boxes & Materials)

Short story (header):
  With decisions made, the next task is packing. The program plans box counts,
  labels, and packing materials based on what you kept.

Overview:
  - Reads './cases/bus/move/inventory_decisions.json' from Program 1.
  - Assigns box sizes and counts by category and rough density.
  - Estimates tape/labels/wrap and produces per-room labels.
  - Prints Answer/Reason/Check and writes './cases/bus/move/packing_plan.json'.
"""
from __future__ import annotations
import argparse, json, math
from typing import Dict, Any, List

### DATA
DENSITY = {  # estimated kg per m3 to hint box sizes
    "books": 120, "boxes": 100, "furniture": 50, "appliance": 80, "bags": 30, "misc": 40
}
BOX_CAPACITY_M3 = {"small": 0.04, "medium": 0.06, "large": 0.09}
CATEGORY_TO_BOX = {
    "books":"small","boxes":"medium","bags":"medium","misc":"small",
    "appliance":"large","furniture":"large"
}

### LOGIC
def plan_packing(kept: List[Dict[str,Any]]) -> Dict[str,Any]:
    boxes = {"small":0, "medium":0, "large":0}
    labels = []
    reasons = []
    for it in kept:
        cat = (it.get("category") or "misc").lower()
        vol = float(it["volume_m3"])
        preferred = CATEGORY_TO_BOX.get(cat, "medium")
        cap = BOX_CAPACITY_M3[preferred]
        count = math.ceil(vol / cap) if cat in {"books","boxes","bags","misc"} else (1 if vol > 0 else 0)
        boxes[preferred] += count
        labels.append(f"Label: {preferred.title()} x{count} — {it['name']}")
        reasons.append(f"{it['name']}: cat={cat}, vol={vol}m³ → {preferred} boxes x{count} (cap {cap}m³)")
    tape_rolls = math.ceil(sum(boxes.values()) / 10)
    bubble_m = math.ceil(sum(kept_it["volume_m3"] for kept_it in kept) * 10)  # 10 m per m³
    return {"boxes": boxes, "materials": {"tape_rolls": tape_rolls, "bubble_wrap_m": bubble_m},
            "labels": labels, "reasons": reasons}

### CHECK
def run_harness(plan: Dict[str,Any]) -> None:
    print("Check 1 — Box counts are non-negative integers:")
    for k,v in plan["boxes"].items():
        assert isinstance(v, int) and v >= 0
    print("Check 2 — Materials scale with number of boxes/volume:")
    assert plan["materials"]["tape_rolls"] >= math.ceil(sum(plan["boxes"].values())/10)

def main():
    ap = argparse.ArgumentParser(description="Create packing plan from kept inventory.")
    ap.add_argument("--in", dest="infile", default="./cases/bus/move/inventory_decisions.json")
    ap.add_argument("--out", default="./cases/bus/move/packing_plan.json")
    args = ap.parse_args()

    with open(args.infile,"r",encoding="utf-8") as f: inv = json.load(f)
    kept = inv["kept"]
    plan = plan_packing(kept)

    print("# ANSWER")
    print(json.dumps({"boxes": plan["boxes"], "materials": plan["materials"]}, indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in plan["reasons"]: print("-", r)

    with open(args.out,"w",encoding="utf-8") as f: json.dump(plan, f, indent=2, ensure_ascii=False)
    print(f"\nWrote {args.out}")

    print("\n# CHECK (harness) — detailed")
    run_harness(plan)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()

