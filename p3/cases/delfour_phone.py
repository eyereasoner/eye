#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
phone.py — shopper device (pure Python, stdlib only)

Flow:
  profile (sensitive) -> desensitize -> neutral Insight (scoped+expiring)
  -> Policy -> envelope (JSON) -> HMAC-SHA256 signature (demo)
  -> writes: envelope.json, envelope.sig.json, reason.txt

Usage:
  python phone.py --session delfour
  python phone.py --session delfour --bus ./bus --retailer Delfour --device self-scanner --event pick_up_scanner --ttl-hours 2
"""

from __future__ import annotations
import argparse, json, os, hmac, hashlib
from datetime import datetime, timedelta, timezone

# ---------------- Configuration ----------------
# Demo shared secret for HMAC. In production, DO NOT hardcode; use a secure pairing.
DEMO_SHARED_SECRET = b"neutral-insight-demo-shared-secret"

# ---------------- Helpers ----------------
def iso_now() -> str:
    return datetime.now(timezone.utc).isoformat()

def iso_in(hours: float) -> str:
    return (datetime.now(timezone.utc) + timedelta(hours=hours)).isoformat()

def ensure_dir(p: str):
    os.makedirs(p, exist_ok=True)

def canonical_json(obj) -> str:
    """Deterministic JSON string for signing & verification."""
    return json.dumps(obj, sort_keys=True, separators=(",", ":"))

# ---------------- Logic ----------------
def desensitize(profile: dict) -> dict:
    """Map sensitive condition -> neutral need."""
    conditions = set(map(str, profile.get("householdConditions", [])))
    needs_low_sugar = "Diabetes" in conditions
    return {"need:needsLowSugar": bool(needs_low_sugar)}

def derive_insight(need: dict, retailer: str, device: str, event: str, ttl_hours: float, session_id: str) -> dict:
    if not need.get("need:needsLowSugar"):
        # For completeness, still emit a minimal insight (off-by-default)
        thr = 10.0
    else:
        thr = 10.0
    return {
        "id": f"https://example.org/insight/{session_id}",
        "type": "ins:Insight",
        "metric": "sugar_g_per_serving",
        "threshold": thr,
        "suggestionPolicy": "lower_metric_first_higher_price_ok",
        "scopeDevice": device,
        "scopeEvent": event,
        "retailer": retailer,
        #"createdAt": iso_now(),
        "createdAt": "2025-10-05T20:33:48.907163+00:00",
        #"expiresAt": iso_in(ttl_hours)
        "expiresAt": "2025-10-05T22:33:48.907185+00:00"
    }

def policy_from_insight(insight: dict) -> dict:
    ins_id = insight["id"]
    return {
        "type": "odrl:Policy",
        "profile": "Delfour-Insight-Policy",
        "permission": {
            "action": "odrl:use",
            "target": ins_id,
            "constraint": {
                "leftOperand": "odrl:purpose",
                "operator": "odrl:eq",
                "rightOperand": "shopping_assist"
            }
        },
        "prohibition": {
            "action": "odrl:distribute",
            "target": ins_id,
            "constraint": {
                "leftOperand": "odrl:purpose",
                "operator": "odrl:eq",
                "rightOperand": "marketing"
            }
        },
        "duty": {
            "action": "odrl:delete",
            "constraint": {
                "leftOperand": "odrl:dateTime",
                "operator": "odrl:eq",
                "rightOperand": insight["expiresAt"]
            }
        }
    }

def sign_envelope(envelope: dict) -> dict:
    canonical = canonical_json(envelope).encode("utf-8")
    digest = hashlib.sha256(canonical).hexdigest()
    mac = hmac.new(DEMO_SHARED_SECRET, canonical, hashlib.sha256).hexdigest()
    return {
        "alg": "HMAC-SHA256",
        "keyid": "demo-shared-secret",
        #"created": iso_now(),
        "created": "2025-10-05T20:33:48.907163+00:00",
        "payloadHashSHA256": digest,
        "signatureHMAC": mac
    }

# ---------------- CLI ----------------
def parse_args():
    ap = argparse.ArgumentParser()
    ap.add_argument("--bus", default="./bus")
    ap.add_argument("--session", default="delfour")
    ap.add_argument("--retailer", default="Delfour")
    ap.add_argument("--device", default="self-scanner")
    ap.add_argument("--event", default="pick_up_scanner")
    ap.add_argument("--ttl-hours", type=float, default=2.0)
    return ap.parse_args()

def main():
    a = parse_args()
    session_dir = os.path.join(a.bus, a.session)
    ensure_dir(session_dir)

    # 1) Sensitive profile (LOCAL ONLY)
    profile = {"householdConditions": ["Diabetes"]}

    # 2) Desensitize -> neutral need
    need = desensitize(profile)

    # 3) Insight (neutral, scoped, expiring)
    insight = derive_insight(need, a.retailer, a.device, a.event, a.ttl_hours, a.session)

    # 4) Policy derived from Insight
    policy = policy_from_insight(insight)

    # 5) Envelope + signature
    envelope = {"insight": insight, "policy": policy}
    signature = sign_envelope(envelope)

    # 6) Write artifacts
    with open(os.path.join(session_dir, "envelope.json"), "w") as f:
        json.dump(envelope, f, indent=2)
    with open(os.path.join(session_dir, "envelope.sig.json"), "w") as f:
        json.dump(signature, f, indent=2)
    with open(os.path.join(session_dir, "reason.txt"), "w") as f:
        f.write(
            "Household requires low-sugar guidance (diabetes in POD). "
            f"A neutral Insight is scoped to device '{a.device}', event '{a.event}', "
            f"retailer '{a.retailer}', and expires soon; the policy confines use to shopping assistance.\n"
        )

    print(f"[phone] Wrote envelope.json, envelope.sig.json, reason.txt to {session_dir}/")

if __name__ == "__main__":
    main()

