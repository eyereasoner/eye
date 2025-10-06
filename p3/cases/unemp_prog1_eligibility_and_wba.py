
#!/usr/bin/env python3
"""
P3 — Program 1: Unemployment Eligibility & Weekly Benefit Amount (WBA)

Short story (header):
  People lose jobs; the agency must decide who qualifies and how much to pay,
  using simple public rules. The task is to determine eligibility, compute WBA,
  and explain each decision.

Overview:
  Evaluate a small roster under a simplified policy. Print Answer/Reason/Check; write
  ./bus/unemp/eligible_claims.json for Program 2.
"""
from __future__ import annotations

import argparse, json, dataclasses, datetime as dt
from typing import List, Dict, Any

### DATA
@dataclasses.dataclass(frozen=True)
class Worker:
    id: str
    name: str
    avg_weekly_wage: float     # average wage in the base period (€)
    weeks_worked: int          # weeks in base period
    separation_reason: str     # 'layoff' | 'quit' | 'fired_misconduct'
    residency: str             # 'in_state' | 'out_of_state'
    prior_overpayment_flag: bool
    seeking_work: bool         # currently able & actively seeking
    last_employed_date: dt.date

WORKERS: List[Worker] = [
    Worker("w1","Ana Silva",      820.0, 46, "layoff",           "in_state", False, True,  dt.date(2025,9,1)),
    Worker("w2","Bram Janssens",  450.0, 18, "layoff",           "in_state", False, True,  dt.date(2025,8,28)),
    Worker("w3","Chi N’Diaye",    600.0, 30, "quit",             "in_state", False, True,  dt.date(2025,9,10)),
    Worker("w4","Daria Novak",    510.0, 40, "fired_misconduct", "in_state", False, True,  dt.date(2025,9,5)),
    Worker("w5","Evan Müller",    300.0, 22, "layoff",           "out_of_state", False, True, dt.date(2025,8,20)),
    Worker("w6","Farah El‑Sayed", 950.0, 52, "layoff",           "in_state", True,  True,  dt.date(2025,9,3)),
    Worker("w7","Gita Verma",     720.0, 28, "layoff",           "in_state", False, False, dt.date(2025,9,12)),
]

POLICY = {
    "min_weeks_worked": 20,
    "residency_required": "in_state",
    "disqualify_reasons": {"quit", "fired_misconduct"},
    "require_seeking_work": True,
    "wba_rate": 0.5,                 # 50% of average weekly wage
    "wba_cap": 500.0,                # cap €
    "max_weeks": 26,
    "waiting_week": 1                # unpaid first week
}

AS_OF_DEFAULT = "2025-09-18"

### LOGIC
def compute_wba(avg_weekly_wage: float) -> float:
    return round(min(avg_weekly_wage * POLICY["wba_rate"], POLICY["wba_cap"]), 2)

def is_eligible(w: Worker) -> (bool, str):
    if w.weeks_worked < POLICY["min_weeks_worked"]:
        return False, f"Insufficient weeks ({w.weeks_worked} < {POLICY['min_weeks_worked']})."
    if w.residency != POLICY["residency_required"]:
        return False, f"Residency '{w.residency}' not '{POLICY['residency_required']}'."
    if w.separation_reason in POLICY["disqualify_reasons"]:
        return False, f"Separation reason '{w.separation_reason}' is disqualifying."
    if POLICY["require_seeking_work"] and not w.seeking_work:
        return False, "Must be able & actively seeking work."
    if w.prior_overpayment_flag:
        return False, "Prior overpayment unresolved."
    return True, "Meets weeks, residency, separation, seeking; no disqualifiers."

def build_claims(as_of: dt.date) -> Dict[str, Any]:
    eligible, denied, reasons = [], [], []
    for w in WORKERS:
        ok, why = is_eligible(w)
        wba = compute_wba(w.avg_weekly_wage) if ok else 0.0
        duration = POLICY["max_weeks"] if ok else 0
        waiting = POLICY["waiting_week"] if ok else 0
        start = max(as_of, w.last_employed_date)  # simple anchor
        exhaust = (start + dt.timedelta(weeks=duration+waiting)).isoformat() if ok else None
        entry = {
            "id": w.id, "name": w.name, "eligible": ok, "wba_eur": wba,
            "duration_weeks": duration, "waiting_weeks": waiting,
            "start_date": start.isoformat(), "projected_exhaustion_date": exhaust,
            "avg_weekly_wage": w.avg_weekly_wage
        }
        (eligible if ok else denied).append(entry)
        reasons.append(f"{w.name}: {why} WBA={wba}€, duration={duration}w, waiting={waiting}w.")
    return {
        "reference_date": as_of.isoformat(),
        "policy": {**POLICY, "disqualify_reasons": sorted(list(POLICY["disqualify_reasons"]))},
        "eligible_claims": sorted(eligible, key=lambda x: x["id"]),
        "denied_claims": sorted(denied, key=lambda x: x["id"]),
        "reasons": reasons
    }

### CHECK
def run_harness(result: Dict[str,Any]) -> None:
    print("Check 1 — Eligibility criteria enforced:")
    for w in WORKERS:
        ok, _ = is_eligible(w)
        in_list = any(c["id"] == w.id for c in result["eligible_claims"])
        print(f"  - {w.id} ok={ok}, in_list={in_list}")
        assert ok == in_list

    print("Check 2 — WBA cap and rate honored:")
    for c in result["eligible_claims"]:
        expected = round(min(c["avg_weekly_wage"]*POLICY["wba_rate"], POLICY["wba_cap"]),2)
        print(f"  - {c['id']} expected={expected} vs reported={c['wba_eur']}")
        assert expected == c["wba_eur"]

    print("Check 3 — Duration & waiting week nonnegative; exhaustion date present iff eligible:")
    for c in result["eligible_claims"]:
        assert c["duration_weeks"] == POLICY["max_weeks"] and c["waiting_weeks"] == POLICY["waiting_week"]
        assert c["projected_exhaustion_date"] is not None
    for c in result["denied_claims"]:
        assert c["duration_weeks"] == 0 and c["waiting_weeks"] == 0
        assert c["projected_exhaustion_date"] is None

def main():
    ap = argparse.ArgumentParser(description="Determine unemployment eligibility & WBA.")
    ap.add_argument("--out", default="./bus/unemp/eligible_claims.json")
    ap.add_argument("--asof", default=AS_OF_DEFAULT)
    args = ap.parse_args()

    as_of = dt.date.fromisoformat(args.asof)
    result = build_claims(as_of)

    print("# ANSWER")
    print(json.dumps({"eligible_claims": result["eligible_claims"]}, indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in result["reasons"]:
        print("-", r)

    with open(args.out,"w",encoding="utf-8") as f:
        json.dump(result,f,indent=2,ensure_ascii=False)
    print(f"\nWrote {args.out}")

    print("\n# CHECK (harness) — detailed")
    run_harness(result)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()
