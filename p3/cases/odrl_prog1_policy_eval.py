
#!/usr/bin/env python3
"""
P3 — Program 1: ODRL-style Policy Evaluation for Personal Data Processing

Short story (header):
  A data steward receives several processing requests. The note is concise:
  “Apply ODRL-style rules for purpose, consent, age, region, and retention.”
  The task is to decide which requests are permitted and explain why.
"""
from __future__ import annotations

import argparse
import dataclasses
import datetime as dt
import json
from typing import List, Dict, Any, Tuple, Set

### DATA
# -----------------------------
# Data (embedded) — Each entry is a "processing request"
# -----------------------------
@dataclasses.dataclass(frozen=True)
class Request:
    id: str
    subject_name: str
    region: str
    age: int
    parental_consent: bool
    lawful_basis: str
    is_processor: bool
    dpa_signed: bool
    purpose: str
    consent_marketing: bool
    requested_retention_days: int
    requested_fields: List[str]

REQUESTS: List[Request] = [
    Request("u1", "Ana Jensen",       "EU", 34, False, "consent",  False, True,  "marketing", True,  120, ["email"]),
    Request("u2", "Bram Costa",       "EU", 15, True,  "consent",  True,  True,  "analytics", False, 365, ["user_id"]),
    Request("u3", "Cleo Ibarra",      "US", 28, False, "legitimate_interest", False, False, "support", False, 400, ["email","ticket_id"]),
    Request("u4", "Daria Petrov",     "EU", 15, False, "consent",  False, True,  "marketing", True,  200, ["email","phone"]),
    Request("u5", "Evan O'Neil",      "EU", 22, False, "contract", True,  False, "support",  False, 730, ["email","ticket_id"]),
    Request("u6", "Fay Chen",         "US", 17, False, "consent",  False, True,  "analytics", True,  200, ["user_id","email"]),
    Request("u7", "Gita Verma",       "EU", 40, False, "consent",  True,  True,  "analytics", False, 180, ["user_id"]),
    Request("u8", "Hugo Müller",      "EU", 16, False, "consent",  False, True,  "marketing", True,  90,  ["email"]),
]

### LOGIC
ASOF_DEFAULT = "2025-09-18"
ALLOWED_PURPOSES: Set[str] = {"marketing","analytics","support"}
RETENTION_CAP = {"marketing": 180, "analytics": 365, "support": 730}
ALLOWED_FIELDS = {
    "marketing": {"email"},
    "analytics": {"user_id"},
    "support": {"email","phone","ticket_id"}
}
EU_MIN_AGE_FOR_CONSENT = 16

# Core policy evaluation: apply ODRL-style constraints to a single request.
def evaluate(req: Request) -> Tuple[bool, Dict[str, Any], str]:
    if req.purpose not in ALLOWED_PURPOSES:
        return False, {}, f"Purpose '{req.purpose}' is not allowed."
    if req.region == "EU":
        if req.purpose == "marketing" and req.lawful_basis != "consent":
            return False, {}, "EU marketing requires lawful basis = consent."
        if req.is_processor and not req.dpa_signed:
            return False, {}, "EU processor role requires a DPA to be signed."
        if req.age < EU_MIN_AGE_FOR_CONSENT and not req.parental_consent:
            return False, {}, f"EU data subject <{EU_MIN_AGE_FOR_CONSENT} needs parental consent."
        if req.lawful_basis not in {"consent","contract"} and req.purpose != "support":
            return False, {}, "EU non-support processing must rely on consent or contract."
    else:
        if req.purpose == "marketing" and req.lawful_basis != "consent":
            return False, {}, "US marketing requires consent in this policy set."
    if req.purpose == "marketing" and not req.consent_marketing:
        return False, {}, "Marketing requires explicit consent_marketing=True."
    allowed_fields = ALLOWED_FIELDS[req.purpose]
    fields_ok = set(req.requested_fields).issubset(allowed_fields)
    if not fields_ok:
        return False, {}, f"Requested fields {req.requested_fields} exceed allowed {sorted(allowed_fields)} for {req.purpose}."
    cap = RETENTION_CAP[req.purpose]
    adjusted_retention = min(req.requested_retention_days, cap)
    details = {
        "allowed_fields": sorted(allowed_fields),
        "approved_fields": sorted(req.requested_fields),
        "retention_days_effective": adjusted_retention,
        "retention_cap_days": cap
    }
    reason = (f"{req.subject_name} ({req.id}) → purpose={req.purpose}, region={req.region}, "
              f"basis={req.lawful_basis}, age={req.age}, parental_consent={req.parental_consent}, "
              f"processor={req.is_processor}, dpa_signed={req.dpa_signed}. "
              f"Fields ok: {fields_ok}. Retention capped at {adjusted_retention} days.")
    return True, details, reason

# Primary driver: produces the Answer JSON used by downstream steps.
def solve(asof: dt.date) -> Dict[str, Any]:
    permitted, denied, reasons = [], [], []
    for r in REQUESTS:
        ok, d, reason = evaluate(r)
        entry = {
            "id": r.id,
            "name": r.subject_name,
            "purpose": r.purpose,
            "region": r.region,
            "lawful_basis": r.lawful_basis,
            "approved_fields": d.get("approved_fields", []),
            "allowed_fields_for_purpose": d.get("allowed_fields", []),
            "retention_days_effective": d.get("retention_days_effective"),
        }
        if ok:
            permitted.append(entry)
        else:
            denied.append({**entry, "denied_reason": reason})
        reasons.append(reason)
    return {
        "reference_date": asof.isoformat(),
        "policy": {
            "allowed_purposes": sorted(ALLOWED_PURPOSES),
            "retention_cap": RETENTION_CAP,
            "allowed_fields": {k: sorted(v) for k, v in ALLOWED_FIELDS.items()},
            "eu_min_age_for_consent": EU_MIN_AGE_FOR_CONSENT
        },
        "permitted": sorted(permitted, key=lambda x: x["id"]),
        "denied": sorted(denied, key=lambda x: x["id"]),
        "reasons": reasons
    }

### CHECK
# Test harness: verifies invariants and prints detailed diagnostics.
def run_harness(result: Dict[str, Any]) -> None:
    print("Check 1 — Data minimization: approved_fields ⊆ allowed_fields_for_purpose:")
    for e in result["permitted"]:
        subset_ok = set(e["approved_fields"]).issubset(set(e["allowed_fields_for_purpose"]))
        print(f"  - {e['id']} subset_ok={subset_ok} ({e['approved_fields']} ⊆ {e['allowed_fields_for_purpose']})")
        assert subset_ok

    print("Check 2 — Retention caps respected:")
    for e in result["permitted"]:
        cap = RETENTION_CAP[e["purpose"]]
        effective = e["retention_days_effective"]
        print(f"  - {e['id']} effective={effective} ≤ cap={cap}")
        assert effective is not None and effective <= cap

    print("Check 3 — Denied list contains clear reasons:")
    for d in result["denied"]:
        has_reason = "denied_reason" in d and isinstance(d["denied_reason"], str) and len(d["denied_reason"]) > 0
        print(f"  - {d['id']} reason_present={has_reason}")
        assert has_reason

    print("Check 4 — Schema essentials:")
    for field in ("reference_date","policy","permitted","denied"):
        print(f"  - field '{field}':", "present" if field in result else "MISSING")
        assert field in result

def main():
    parser = argparse.ArgumentParser(description="Evaluate ODRL-style personal data processing requests.")
    parser.add_argument("--out", default="./bus/odrl/permitted_processing.json", help="Output JSON path")
    parser.add_argument("--asof", default="2025-09-18", help="Reference date (YYYY-MM-DD)")
    args = parser.parse_args()

    asof = dt.date.fromisoformat(args.asof)
    result = solve(asof)

    print("# ANSWER")
    print(json.dumps({"permitted": result["permitted"]}, indent=2, ensure_ascii=False))

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
