#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
scanner.py — retailer device (pure Python, stdlib only)

Flow:
  read envelope.json + envelope.sig.json
  -> verify HMAC-SHA256 (demo shared secret)
  -> authorize request (purpose=shopping_assist) + expiry guard
  -> simulate one scan + suggestion
  -> write audit.json, banner.json, checks.json

Usage:
  python scanner.py --session delfour
  python scanner.py --session delfour --bus ./bus
"""

from __future__ import annotations
import argparse, json, os, hmac, hashlib
from datetime import datetime, timezone

# ---------------- Configuration ----------------
DEMO_SHARED_SECRET = b"neutral-insight-demo-shared-secret"

# Demo catalog (could be loaded from a file)
CATALOG = [
    {"id": "prod:BIS_001", "name": "Classic Tea Biscuits",  "sku": "BIS-001",  "sugar": 12.0, "price": 2.10},
    {"id": "prod:BIS_101", "name": "Low-Sugar Tea Biscuits","sku": "BIS-101",  "sugar": 3.0,  "price": 2.60},
    {"id": "prod:CHOC_050","name": "Milk Chocolate Bar",    "sku": "CHOC-050", "sugar": 15.0, "price": 1.80},
    {"id": "prod:CHOC_150","name": "85% Dark Chocolate",    "sku": "CHOC-150", "sugar": 6.0,  "price": 2.20},
]

# Runtime request (typed)
REQUEST = {
    "type": "req:Request",
    "action": "odrl:use",
    "constraint": {"leftOperand": "odrl:purpose", "rightOperand": "shopping_assist"}
}

# ---------------- Helpers ----------------
def iso_now() -> str:
    return datetime.now(timezone.utc).isoformat()

def canonical_json(obj) -> str:
    return json.dumps(obj, sort_keys=True, separators=(",", ":"))

def read_json(path: str) -> dict:
    with open(path, "r") as f:
        return json.load(f)

def verify_signature(envelope: dict, sig: dict) -> bool:
    canonical = canonical_json(envelope).encode("utf-8")
    digest = hashlib.sha256(canonical).hexdigest()
    if str(sig.get("payloadHashSHA256", "")).lower() != digest.lower():
        return False
    expected = hmac.new(DEMO_SHARED_SECRET, canonical, hashlib.sha256).hexdigest()
    return hmac.compare_digest(expected, sig.get("signatureHMAC", ""))

def authorize(envelope: dict) -> tuple[bool, dict]:
    """Returns (allowed, audit_entry)."""
    ins = envelope["insight"]
    pol = envelope["policy"]
    #now = iso_now()
    now = "2025-10-05T20:35:48.907163+00:00"

    # expiry guard
    expired = now > ins["expiresAt"]

    # check policy permission matches request
    perm = pol.get("permission", {})
    matches = (
        REQUEST["action"] == perm.get("action") and
        perm.get("target") == ins["id"] and
        perm.get("constraint", {}).get("leftOperand") == "odrl:purpose" and
        perm.get("constraint", {}).get("rightOperand") == REQUEST["constraint"]["rightOperand"]
    )

    if expired or not matches:
        return (False, {
            "type": "act:Decision",
            "at": now,
            "request": REQUEST,
            "outcome": "Blocked",
            "reason": "expired" if expired else "policy_mismatch"
        })

    return (True, {
        "type": "act:Decision",
        "at": now,
        "request": REQUEST,
        "outcome": "Allowed",
        "target": ins["id"]
    })

def simulate_scan_and_suggest(insight: dict) -> dict:
    """Scan BIS_001; if sugar >= threshold, suggest a strictly lower-sugar alternative."""
    scanned = next(p for p in CATALOG if p["id"] == "prod:BIS_001")
    thr = float(insight["threshold"])
    note = None
    alt_name = None
    if scanned["sugar"] >= thr:
        note = "High sugar"
        # find best (lowest sugar) strictly lower than scanned
        candidates = [p for p in CATALOG if p["sugar"] < scanned["sugar"]]
        if candidates:
            alt = sorted(candidates, key=lambda p: p["sugar"])[0]
            alt_name = alt["name"]
    return {
        "headline": "Track sugar per serving while you scan" if note else "Scan complete",
        "product_name": scanned["name"],
        "note": note,
        "suggested_alternative": alt_name
    }

def compute_checks(envelope: dict, banner: dict, audit: list[dict]) -> list[list]:
    insight = envelope["insight"]
    insight_text = canonical_json(insight).lower()
    has_decision = any(e.get("type") == "act:Decision" for e in audit)
    return [
        ["insight_nonempty", bool(insight)],
        ["minimization_no_sensitive_terms", ("diabetes" not in insight_text and "medical" not in insight_text)],
        ["scope_has_device_event_expiry", all(k in insight for k in ("scopeDevice","scopeEvent","expiresAt"))],
        ["runtime_present", banner is not None and bool(banner.get("headline"))],
        ["behavior_suggests_on_high_sugar", banner.get("note") == "High sugar"],
        ["audit_has_decision", has_decision],
    ]

# ---------------- CLI ----------------
def parse_args():
    ap = argparse.ArgumentParser()
    ap.add_argument("--bus", default="./bus")
    ap.add_argument("--session", default="delfour")
    return ap.parse_args()

def main():
    a = parse_args()
    session_dir = os.path.join(a.bus, a.session)
    env_path = os.path.join(session_dir, "envelope.json")
    sig_path = os.path.join(session_dir, "envelope.sig.json")

    # 1) Load envelope + signature
    if not (os.path.exists(env_path) and os.path.exists(sig_path)):
        raise SystemExit(f"Missing envelope/signature in {session_dir}/")

    envelope = read_json(env_path)
    signature = read_json(sig_path)

    # 2) Verify signature
    if not verify_signature(envelope, signature):
        raise SystemExit("[scanner] Signature verification FAILED")

    # 3) Authorize
    allowed, decision = authorize(envelope)
    audit = [decision]

    # 4) Shopping step
    if allowed:
        banner = simulate_scan_and_suggest(envelope["insight"])
    else:
        banner = {
            "headline": "Policy blocked action",
            "product_name": None,
            "note": "Expired or prohibited",
            "suggested_alternative": None
        }

    # 5) Close-out duty (if expired now, emit delete_due)
    #now2 = iso_now()
    now2 = "2025-10-05T20:37:48.907163+00:00"
    if now2 > envelope["insight"]["expiresAt"]:
        audit.append({
            "type": "act:Duty",
            "at": now2,
            "duty": "delete_due",
            "target": envelope["insight"]["id"]
        })

    # 6) Checks (harness)
    checks = compute_checks(envelope, banner, audit)

    # 7) Write outputs
    with open(os.path.join(session_dir, "audit.json"), "w") as f:
        json.dump(audit, f, indent=2)
    with open(os.path.join(session_dir, "banner.json"), "w") as f:
        json.dump(banner, f, indent=2)
    with open(os.path.join(session_dir, "checks.json"), "w") as f:
        json.dump(checks, f, indent=2)

    print(f"[scanner] Verified envelope; wrote audit.json, banner.json, checks.json to {session_dir}/")

if __name__ == "__main__":
    main()

