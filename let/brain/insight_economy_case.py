#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
INSIGHT ECONOMY
===============

This file implements a small, end‑to‑end example of the "insight economy" idea [1]:
derive *actionable insights* from sensitive, private data and share only the
minimal instructions required to act — NOT the raw data itself.

Scenario
--------
- A person starts a self‑scanner session at retailer "Delfour".
- On the *private side*, we know something sensitive (e.g., "diabetes" in the household).
- Instead of sending that sensitive fact to the retailer, we compute a *contextual
  insight*: "show sugar grams per serving while scanning, and suggest a lower‑sugar
  alternative if the scanned product is high in sugar".
- The retailer only receives a small *insight envelope*: a UI instruction + a
  time‑bound, scope‑limited context in which to apply it. No medical terms, no IDs.

What this file demonstrates
---------------------------
1) **Derivation**: A private function computes a UI banner instruction using
   sensitive data without leaking it.
2) **Activation**: The retailer device executes the instruction at scan time.
3) **Explanation**: A human‑readable "reason" for the person (private side only).
4) **Harness**: Small checks for data minimization, context restriction, behavior,
   and time‑boundedness.

References
----------
[1] https://ruben.verborgh.org/blog/2025/08/12/inside-the-insight-economy/
"""

from __future__ import annotations

from dataclasses import dataclass, asdict
from datetime import datetime, timedelta, timezone
import json
import uuid
from typing import List, Optional, Dict, Any


# ---------------------------------------------------------------------------
# Domain model
# ---------------------------------------------------------------------------

@dataclass
class RawPersonalData:
    """
    PRIVATE‑SIDE structure. This never leaves the user's trusted environment.
    In production, this could live in a personal data pod or secure enclave.

    Notes on privacy:
    - Contains sensitive attributes (e.g., special‑category data under GDPR).
    - MUST NOT be serialized into insight envelopes or logs that leave the private side.
    """
    subject_pseudonym: str              # Rotating local pseudonym (not a stable ID)
    household_conditions: List[str]     # e.g. ["diabetes"]


@dataclass
class ShoppingContext:
    """
    PUBLIC context of the interaction; scoped to a single retail session/device.
    This is OK to share with the retailer.

    We model context explicitly to enforce "hot, contextual" insights rather
    than cold, cross‑context data dumps.
    """
    retailer: str                       # e.g., "Delfour"
    device: str                         # e.g., "self-scanner"
    event: str                          # e.g., "pick_up_scanner"
    timestamp: datetime                 # wall‑clock time in UTC
    location_hint: Optional[str] = None # e.g., "store-42"


@dataclass
class Product:
    """
    PUBLIC catalog item known to the retailer/device.
    """
    sku: str
    name: str
    sugar_g_per_serving: float
    price_eur: float


@dataclass
class InsightEnvelope:
    """
    What the retailer actually receives (and only this).

    Key principles:
    - **Data minimization**: No sensitive terms or identifiers.
    - **Scope**: Explicit device/event, store, and expiry to keep the insight hot.
    - **Actionability**: Payload that devices can render or execute, not raw data.
    - **Provenance**: Simple metadata to support audit without leaking secrets.
    """
    insight_id: str                     # Opaque random ID for traceability
    recipient: str                      # Intended recipient, e.g., "Delfour"
    scope: Dict[str, Any]               # Context limits (device/event/store/expiry)
    payload: Dict[str, Any]             # UI/action spec; no medical talk
    provenance: Dict[str, Any]          # Non‑sensitive origin metadata


# ---------------------------------------------------------------------------
# Example input data
# ---------------------------------------------------------------------------

# PRIVATE data known only on the user's side.
raw = RawPersonalData(
    subject_pseudonym="user-12345-rotating",
    household_conditions=["diabetes"],   # <- sensitive, stays private
)

# PUBLIC context of the current retail session (OK to share).
# now = datetime.now(timezone.utc)
now = datetime.fromisoformat("2025-08-12T12:52:50.456108+00:00")

ctx = ShoppingContext(
    retailer="Delfour",
    device="self-scanner",
    event="pick_up_scanner",
    timestamp=now,
    location_hint="store-42"
)

# PUBLIC retailer catalog. We include products with different sugar levels so
# the suggestion logic has something to choose from.
catalog: List[Product] = [
    Product(sku="BIS-001", name="Classic Tea Biscuits", sugar_g_per_serving=12.0, price_eur=2.10),
    Product(sku="BIS-101", name="Low-Sugar Tea Biscuits", sugar_g_per_serving=3.0, price_eur=2.60),
    Product(sku="CHOC-050", name="Milk Chocolate Bar", sugar_g_per_serving=15.0, price_eur=1.80),
    Product(sku="CHOC-150", name="85% Dark Chocolate", sugar_g_per_serving=6.0, price_eur=2.20),
]


# ---------------------------------------------------------------------------
# Private‑side: derive a contextual UI instruction (INSIGHT), not raw data
# ---------------------------------------------------------------------------

def derive_ui_banner_insight(
    raw: RawPersonalData,
    ctx: ShoppingContext,
    high_sugar_threshold: float = 10.0,
) -> Optional[Dict[str, Any]]:
    """
    Decide whether to emit a UI banner instruction for this *specific* context.

    Returns:
        payload (dict) suitable for an InsightEnvelope, or None if no insight
        should be emitted for this context.

    Privacy:
        - Uses sensitive info (household_conditions) but does NOT leak it.
        - Output mentions only metrics ("sugar_g_per_serving") and UI actions.
    """
    # Only at Delfour, at the scanner pickup moment, do we activate this banner.
    # This exemplifies a *hot* insight tied to a moment, not a reusable profile.
    if ctx.retailer != "Delfour":
        return None
    if ctx.device != "self-scanner" or ctx.event != "pick_up_scanner":
        return None

    # PRIVATE decision rule: determine if low‑sugar guidance is needed.
    # NOTE: we use this *only* to decide whether to create the banner;
    # the word "diabetes" will never appear outside the private side.
    needs_low_sugar_guidance = "diabetes" in [c.lower() for c in raw.household_conditions]
    if not needs_low_sugar_guidance:
        return None

    # The UI instruction itself is public and neutral: track a *metric*.
    # If sugar is high for a scanned product, suggest a lower‑sugar alternative.
    return {
        "ui_banner": {
            "title": "Track sugar per serving while you scan",
            "metrics": ["sugar_g_per_serving"],
            "actions": [
                {
                    # Declarative rule the device can enforce locally.
                    "type": "suggest_alternative_if_high_metric",
                    "metric": "sugar_g_per_serving",
                    "operator": ">=",
                    "threshold": high_sugar_threshold,
                    # For illustration: prefer lower sugar; higher price is acceptable
                    # to reflect a realistic business scenario.
                    "suggestion_strategy": "lower_metric_first_higher_price_ok",
                }
            ],
        }
    }


# ---------------------------------------------------------------------------
# Wrap the payload in a scoped, time‑bound envelope
# ---------------------------------------------------------------------------

def create_envelope(payload: Dict[str, Any], ctx: ShoppingContext) -> InsightEnvelope:
    """
    Create the envelope that will be sent to the retailer.

    Security & governance considerations:
    - Random one‑off ID (insight_id) supports auditing without correlating users.
    - Scope includes "this_session" and a 2h expiry — avoids long‑term reuse.
    - Provenance records generator version and that minimization occurred.
    """
    #insight_id = f"ins-{uuid.uuid4()}"
    insight_id = "ins-a4aa784b-20f0-4d33-af18-865e370a54dd"

    # Time‑box to keep the insight "hot". Adjust as needed; here we use 2 hours.
    expires_at = (ctx.timestamp + timedelta(hours=2)).isoformat()

    scope = {
        "device": ctx.device,
        "event": ctx.event,
        "valid_for": "this_session",
        "retailer_store": ctx.location_hint,
        "expires_at": expires_at,
    }
    provenance = {
        "created_at": ctx.timestamp.isoformat(),
        "generator": "private_insight_refiner_v1",
        "data_minimization": True,   # Claim: envelope excludes sensitive data
    }

    return InsightEnvelope(
        insight_id=insight_id,
        recipient=ctx.retailer,
        scope=scope,
        payload=payload,
        provenance=provenance,
    )


# ---------------------------------------------------------------------------
# Retailer‑side runtime: execute the envelope locally on the device
# ---------------------------------------------------------------------------

def retailer_runtime_render_banner(
    envelope: InsightEnvelope,
    scanned_product: Product,
    catalog: List[Product],
) -> Dict[str, Any]:
    """
    Simulate what the retailer's device would do when a product is scanned.

    It reads the envelope's declarative instruction and *locally* decides whether
    to show a warning and suggest an alternative with lower sugar.

    Returns a dict that represents the banner the device would render.
    """
    ui = envelope.payload["ui_banner"]            # Only neutral UI instructions
    sugar_value = scanned_product.sugar_g_per_serving

    # Parse the action and compute a suggestion if needed.
    threshold: Optional[float] = None
    alt: Optional[Product] = None
    for action in ui.get("actions", []):
        if action["type"] == "suggest_alternative_if_high_metric":
            threshold = action["threshold"]
            if sugar_value >= threshold:
                # Candidate alternatives are those strictly lower in sugar.
                candidates = [p for p in catalog if p.sugar_g_per_serving < sugar_value]
                if candidates:
                    # Strategy: sort by lower sugar first; break ties by *higher* price.
                    # Rationale: demonstrates that healthier alternatives can still
                    # be consistent with retailer margin reality.
                    candidates.sort(key=lambda p: (p.sugar_g_per_serving, -p.price_eur))
                    alt = candidates[0]
            break  # We only have one action in this simple example.

    # Compose a renderable banner. Note how the message stays metric‑focused.
    banner = {
        "headline": ui["title"],
        "product_name": scanned_product.name,
        "sugar_g_per_serving": round(sugar_value, 2),
        "note": None,                      # Set if threshold is crossed
        "suggested_alternative": None,     # Filled if an alternative was found
    }
    if threshold is not None and sugar_value >= threshold:
        banner["note"] = f"High sugar (≥ {threshold} g/serving)."
        if alt:
            banner["suggested_alternative"] = {
                "sku": alt.sku,
                "name": alt.name,
                "sugar_g_per_serving": round(alt.sugar_g_per_serving, 2),
                "price_eur": round(alt.price_eur, 2),
            }
    return banner


# ---------------------------------------------------------------------------
# Private‑side explanation for the person ("reason why")
# ---------------------------------------------------------------------------

def explain_reason(raw: RawPersonalData, ctx: ShoppingContext) -> str:
    """
    Create a short, human‑readable explanation.

    Audience:
        - The person (or their agent) on the private side.
        - Auditors/reviewers verifying that derivation followed policy.

    Important:
        - This explanation is NOT sent to the retailer.
        - It may mention sensitive facts because it never leaves the private side.
    """
    reasons: List[str] = []
    if "diabetes" in [c.lower() for c in raw.household_conditions]:
        reasons.append("Household requires low-sugar guidance.")
    if ctx.retailer == "Delfour" and ctx.device == "self-scanner" and ctx.event == "pick_up_scanner":
        reasons.append("You are starting a self-scanner session at Delfour right now.")
    reasons.append("The envelope minimizes data: it avoids medical terms and is limited to this session.")
    return " ".join(reasons)


# ---------------------------------------------------------------------------
# Harness — simple checks to keep us honest
# ---------------------------------------------------------------------------

def run_harness() -> List[tuple[str, bool]]:
    """
    Execute a handful of pragmatic checks.

    Checks:
      1) Derivation happens only in the intended Delfour pickup context.
      2) Envelope contains NO sensitive terms or local pseudonyms.
      3) Changing retailer prevents derivation (context restriction).
      4) Behavior: high-sugar -> suggests; low-sugar -> does not suggest.
      5) Scope: envelope expires within the expected time window.
    """
    results: List[tuple[str, bool]] = []

    # (1) Derivation in context
    payload = derive_ui_banner_insight(raw, ctx)
    results.append(("derive_in_context", payload is not None))

    # (2) No sensitive terms leaked
    if payload is not None:
        env = create_envelope(payload, ctx)
        env_json = json.dumps(asdict(env)).lower()
        forbidden = ["diabetes", "medical", raw.subject_pseudonym.lower()]
        results.append(("minimization_no_sensitive_terms", not any(f in env_json for f in forbidden)))
    else:
        results.append(("minimization_no_sensitive_terms", False))

    # (3) Context restriction: different retailer should yield no payload
    other_ctx = ShoppingContext(
        retailer="PharmacyX",
        device="self-scanner",
        event="pick_up_scanner",
        timestamp=ctx.timestamp,
        location_hint="pharm-9",
    )
    no_payload_elsewhere = derive_ui_banner_insight(raw, other_ctx)
    results.append(("no_derivation_outside_delfour", no_payload_elsewhere is None))

    # (4) Behavior test: high vs. low sugar paths
    behavior_ok = False
    if payload is not None:
        env = create_envelope(payload, ctx)
        high = next(p for p in catalog if p.sugar_g_per_serving >= 12.0)
        low  = next(p for p in catalog if p.sugar_g_per_serving <= 3.0)
        banner_high = retailer_runtime_render_banner(env, high, catalog)
        banner_low  = retailer_runtime_render_banner(env, low, catalog)
        behavior_ok = (banner_high["suggested_alternative"] is not None) and (banner_low["suggested_alternative"] is None)
    results.append(("behavior_alternative_logic", behavior_ok))

    # (5) Time‑bound envelope (<= 2 hours)
    expiry_ok = False
    if payload is not None:
        env = create_envelope(payload, ctx)
        expires_at = datetime.fromisoformat(env.scope["expires_at"])
        # Allow a small tolerance for wall‑clock differences.
        expiry_ok = 0 < (expires_at - ctx.timestamp).total_seconds() <= 2 * 3600 + 5
    results.append(("time_bound_envelope", expiry_ok))

    return results


# ---------------------------------------------------------------------------
# Program entry point
# ---------------------------------------------------------------------------

def main() -> None:
    """
    Orchestrate the flow and print three sections:
      1) INSIGHT (what the retailer gets)
      2) REASON WHY (private explanation)
      3) RUNTIME PREVIEW (what the device would show for one scanned item)
      4) HARNESS CHECKS (pass/fail summary)
    """
    payload = derive_ui_banner_insight(raw, ctx)
    if not payload:
        print("No insight derived for this context.")
        return

    envelope = create_envelope(payload, ctx)
    reason = explain_reason(raw, ctx)

    # Simulate scanning a high‑sugar product to preview the banner behavior.
    scanned = next(p for p in catalog if p.sugar_g_per_serving >= 12.0)
    banner = retailer_runtime_render_banner(envelope, scanned, catalog)

    print("=== INSIGHT (to retailer) ===")
    print(json.dumps(asdict(envelope), indent=2))

    print("\n=== REASON WHY (private) ===")
    print(reason)

    print("\n=== RUNTIME PREVIEW (device banner for a scanned product) ===")
    print(json.dumps(banner, indent=2))

    print("\n=== HARNESS CHECKS ===")
    results = run_harness()
    passed = sum(1 for _, ok in results if ok)
    total  = len(results)
    for name, ok in results:
        print(f"{'PASS' if ok else 'FAIL'} - {name}")
    print(f"Overall: {passed}/{total} checks passed.")


if __name__ == "__main__":
    main()
