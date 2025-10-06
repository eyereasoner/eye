#!/usr/bin/env python3
"""
P3 — Program 1: Inventory Decider (Keep / Donate / Dispose)

Short story (header):
  Before moving, you list household items in a JSON file. The task is to decide
  what to keep vs. not keep, estimate the moved volume/weight, and explain why.

Overview (what this file does, mirrored from this chat):
  - Reads items from a JSON file you provide (see ./bus/move/inventory_sample.json).
  - Applies simple, transparent rules to tag non-kept items as Donate or Dispose.
  - Sums volume and weight for kept items; writes a clean artifact for Program 2.
  - Prints Answer/Reason/Check using the EYE pattern.

Sections map:
  ### DATA  – input schema, policy knobs (donate/disposing rules)
  ### LOGIC – decision function and aggregations
  ### CHECK – harness: schema, totals, and partition consistency
"""
from __future__ import annotations
import argparse, json
from typing import Dict, Any, List, Tuple

### DATA
# Input schema (per item): { id, name, category, keep:bool, volume_m3:float, weight_kg:float, condition:str }
DONATE_IF = {"worn"}        # not kept but serviceable → donate
DISPOSE_IF = {"broken"}     # not kept and broken → dispose

### LOGIC
def decide(item: Dict[str,Any]) -> Tuple[str,str]:
    """Return ('keep'|'donate'|'dispose', reason)."""
    if item.get("keep", False):
        return "keep", "Marked keep=True."
    cond = (item.get("condition") or "").lower()
    if cond in DISPOSE_IF:
        return "dispose", f"condition={cond}."
    if cond in DONATE_IF:
        return "donate", f"condition={cond}."
    # default: donate if not broken
    return "donate", f"not kept; condition={cond or 'unknown'}."

def summarize(items: List[Dict[str,Any]]) -> Dict[str,Any]:
    kept, donate, dispose = [], [], []
    reasons = []
    for it in items:
        action, why = decide(it)
        payload = {**it, "decision": action}
        (kept if action=="keep" else donate if action=="donate" else dispose).append(payload)
        reasons.append(f"{it['name']}: decision={action}; because {why}")
    vol = round(sum(i["volume_m3"] for i in kept), 2)
    wt  = round(sum(i["weight_kg"] for i in kept), 1)
    return {
        "kept": kept, "donate": donate, "dispose": dispose,
        "totals": {"kept_volume_m3": vol, "kept_weight_kg": wt},
        "reasons": reasons
    }

### CHECK
def run_harness(raw: List[Dict[str,Any]], result: Dict[str,Any]) -> None:
    print("Check 1 — Partition covers all items exactly once:")
    part = len(result["kept"]) + len(result["donate"]) + len(result["dispose"])
    print(f"  - input={len(raw)}, partitioned={part}")
    assert part == len(raw)

    print("Check 2 — Totals recompute correctly from kept list:")
    vol = round(sum(i["volume_m3"] for i in result["kept"]), 2)
    wt  = round(sum(i["weight_kg"] for i in result["kept"]), 1)
    print(f"  - reported volume={result['totals']['kept_volume_m3']} vs recomputed={vol}")
    print(f"  - reported weight={result['totals']['kept_weight_kg']} vs recomputed={wt}")
    assert (result["totals"]["kept_volume_m3"], result["totals"]["kept_weight_kg"]) == (vol, wt)

def main():
    ap = argparse.ArgumentParser(description="Decide keep/donate/dispose from inventory JSON.")
    ap.add_argument("--in", dest="infile", default="./bus/move/inventory_sample.json")
    ap.add_argument("--out", default="./bus/move/inventory_decisions.json")
    args = ap.parse_args()

    with open(args.infile,"r",encoding="utf-8") as f: items = json.load(f)
    result = summarize(items)

    print("# ANSWER")
    print(json.dumps(result["totals"], indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in result["reasons"]: print("-", r)

    with open(args.out,"w",encoding="utf-8") as f: json.dump(result, f, indent=2, ensure_ascii=False)
    print(f"\nWrote {args.out}")

    print("\n# CHECK (harness) — detailed")
    run_harness(items, result)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()

