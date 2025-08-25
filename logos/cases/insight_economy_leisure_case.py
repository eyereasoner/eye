#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
INSIGHT ECONOMY — LEISURE DOMAIN (THEME PARK)
=============================================

What this script shows
----------------------
A privacy-preserving pattern where we *act on* sensitive preferences without
ever sharing them. We compute a **contextual insight** privately and only send a
minimal, time-bound **insight envelope** to the venue device/app. The envelope
contains neutral UI instructions (what to do), not the sensitive "why".

Concrete scenario (LunaLand):
- A guest who prefers gentle experiences opens the LunaLand park app at entry.
- On the private side, we know that preference (sensitive).
- We emit an instruction for this session: highlight ride intensity in the UI and,
  if the guest taps a high-intensity attraction, *suggest a gentler nearby one*.
- The venue only receives the envelope (UI rules + scope), not the preference.

Program output
--------------
1) A — ANSWER: The **insight envelope** (what the venue gets)
2) R — REASON WHY: A **private explanation** (stays on the user's side)
3) Runtime preview: Example banner for a selected attraction
4) C — CHECK: A **harness** of sanity checks (minimization, scoping, behavior, expiry, schema)
"""

from __future__ import annotations

from dataclasses import dataclass, asdict
from datetime import datetime, timedelta, timezone
from typing import List, Optional, Dict, Any, Tuple
import json
import uuid


# =============================================================================
# DATA MODELS
# =============================================================================

@dataclass
class PrivateLeisureProfile:
    """
    PRIVATE-SIDE data — never leaves the user's trusted environment.

    subject_pseudonym:
        A rotating, local pseudonym; not stable across contexts. Included here to
        demonstrate *not* leaking it into the venue envelope.

    prefers_gentle_experiences:
        A sensitive preference (e.g., motion/vestibular sensitivity or anxiety
        could be *inferred*). We won't ever serialize this to the venue.
    """
    subject_pseudonym: str
    prefers_gentle_experiences: bool


@dataclass
class ParkAppContext:
    """
    PUBLIC context for this app session — safe to send to the venue.

    We model context explicitly to keep insights "hot" (time- and device-bound),
    rather than "cold" (reusable profiles).
    """
    venue: str                 # e.g., "LunaLand"
    device: str                # e.g., "park_app"
    event: str                 # e.g., "enter_park"
    timestamp: datetime        # UTC timestamp for scoping/expiry
    zone_hint: Optional[str] = None  # e.g., "north_gate" / "Central"


@dataclass
class Attraction:
    """
    PUBLIC attraction information known to the venue.
    """
    attraction_id: str
    name: str
    zone: str
    intensity_score: int       # Higher = more intense
    wait_minutes: int          # Estimated wait time


@dataclass
class InsightEnvelope:
    """
    The ONLY artifact that goes to the venue.

    Principles:
      - Data minimization: contains no sensitive terms or private labels.
      - Scope: short-lived, tied to device/event/zone to remain "hot".
      - Actionability: contains neutral UI instructions, not rationales.
      - Auditability: light provenance without revealing secrets.
    """
    insight_id: str            # Opaque, random ID (no personal identifiers)
    recipient: str             # The venue name (e.g., "LunaLand")
    scope: Dict[str, Any]      # Device/event/zone/expiry
    payload: Dict[str, Any]    # UI instructions the app can execute
    provenance: Dict[str, Any] # Non-sensitive origin metadata


# =============================================================================
# PRIVATE-SIDE: DERIVE A CONTEXTUAL UI INSTRUCTION (INSIGHT), NOT RAW DATA
# =============================================================================

def derive_leisure_ui_insight(
    guest: PrivateLeisureProfile,
    ctx: ParkAppContext,
    high_intensity_threshold: int = 6,
) -> Optional[Dict[str, Any]]:
    """
    Decide whether to emit a UI instruction given the current context.

    We only emit an instruction when:
      - The venue is LunaLand,
      - The device is the official park app,
      - The event is an app session start ("enter_park"),
      - The guest has a private preference for gentle experiences.

    The returned payload is neutral (metrics + actions); it never states "why".
    """
    # Context restriction: keep the insight "hot" and specific.
    if ctx.venue != "LunaLand":
        return None
    if ctx.device != "park_app" or ctx.event != "enter_park":
        return None

    # PRIVATE decision rule: use the sensitive preference ONLY to decide whether
    # to create a UI instruction. The preference itself is never exposed.
    if not guest.prefers_gentle_experiences:
        return None

    # The payload instructs the venue UI to highlight intensity, and if a chosen
    # attraction is high intensity, suggest a gentler alternative.
    return {
        "ui_banner": {
            "title": "Keep an eye on ride intensity during your visit",
            "metrics": ["intensity_score"],
            "actions": [{
                "type": "suggest_alternative_if_high_metric",
                "metric": "intensity_score",
                "operator": ">=",
                "threshold": high_intensity_threshold,
                # Strategy: prefer nearby, then lowest intensity, then shortest wait.
                "suggestion_strategy": "nearby_then_lowest_intensity_then_shortest_wait",
            }]
        }
    }


# =============================================================================
# ENVELOPE CONSTRUCTION: SCOPE + PAYLOAD + PROVENANCE (MINIMIZED)
# =============================================================================

def create_envelope(payload: Dict[str, Any], ctx: ParkAppContext) -> InsightEnvelope:
    """
    Wrap the UI payload in an envelope that limits who/where/when it applies.

    - insight_id: random opaque ID for tracing without identity correlation.
    - scope: device/event/zone + expiry; here, valid for up to 8 hours (park day).
    - provenance: declares generator + that minimization was applied.
    """
    # insight_id = f"ins-{uuid.uuid4()}"
    insight_id = "ins-da473a5a-7f04-4119-875b-990277c03807"

    # Keep the insight hot: a typical park session window (e.g., up to 8 hours).
    expires_at = (ctx.timestamp + timedelta(hours=8)).isoformat()

    scope = {
        "device": ctx.device,
        "event": ctx.event,
        "valid_for": "this_session",
        "venue_zone": ctx.zone_hint,   # optional hint (e.g., gate/area)
        "expires_at": expires_at,
    }
    provenance = {
        "created_at": ctx.timestamp.isoformat(),
        "generator": "private_leisure_refiner_v1",
        "data_minimization": True,     # Assertion: no sensitive terms in envelope
    }

    return InsightEnvelope(insight_id, ctx.venue, scope, payload, provenance)


# =============================================================================
# VENUE RUNTIME: APPLY THE ENVELOPE LOCALLY ON THE DEVICE/APP
# =============================================================================

def venue_runtime_render_banner(
    envelope: InsightEnvelope,
    selected: Attraction,
    catalog: List[Attraction],
    current_zone: Optional[str],
) -> Dict[str, Any]:
    """
    Given the envelope and a user-selected attraction, compute a banner that:
      - Shows intensity for the selected attraction,
      - If intensity is high, suggests a gentler alternative (prefer nearby zone,
        then lowest intensity, then shortest wait).

    The venue still doesn't know the private preference; it just follows the rule.
    """
    ui = envelope.payload["ui_banner"]
    intensity = selected.intensity_score

    threshold = None
    alternative = None

    # Parse the single action in this example: "suggest_alternative_if_high_metric".
    for action in ui.get("actions", []):
        if action["type"] == "suggest_alternative_if_high_metric":
            threshold = int(action["threshold"])

            # If current choice meets/exceeds the threshold, try to find alternatives.
            if intensity >= threshold:
                # Candidates are those with strictly lower intensity than the selected one.
                candidates = [a for a in catalog if a.intensity_score < intensity]

                # Sorting key: lowest intensity first, then shorter waits.
                def sort_key(a: Attraction) -> Tuple[int, int]:
                    return (a.intensity_score, a.wait_minutes)

                # If we know the user's current zone, prefer alternatives in the same zone.
                if current_zone:
                    same_zone = [a for a in candidates if a.zone == current_zone]
                    other_zone = [a for a in candidates if a.zone != current_zone]
                    same_zone.sort(key=sort_key)
                    other_zone.sort(key=sort_key)
                    candidates = same_zone + other_zone
                else:
                    candidates.sort(key=sort_key)

                # Pick the best candidate if any.
                if candidates:
                    alternative = candidates[0]
            break  # Only one action in this simple envelope

    # Construct the banner payload shown on the venue app/device.
    banner = {
        "headline": ui["title"],
        "attraction_name": selected.name,
        "intensity_score": intensity,
        "note": None,
        "suggested_alternative": None,
    }

    # If the selected ride is high intensity, present a neutral note and suggestion.
    if threshold is not None and intensity >= threshold:
        banner["note"] = f"High intensity (\u2265 {threshold})."
        if alternative:
            banner["suggested_alternative"] = {
                "attraction_id": alternative.attraction_id,
                "name": alternative.name,
                "zone": alternative.zone,
                "intensity_score": alternative.intensity_score,
                "wait_minutes": alternative.wait_minutes,
            }

    return banner


# =============================================================================
# (OPTIONAL) LIGHTWEIGHT ENVELOPE SCHEMA + VALIDATOR
# =============================================================================

ENVELOPE_SCHEMA: Dict[str, Any] = {
    "type": "object",
    "required": ["insight_id", "recipient", "scope", "payload", "provenance"],
    "properties": {
        "insight_id": {"type": "string"},
        "recipient": {"type": "string"},
        "scope": {
            "type": "object",
            "required": ["device", "event", "valid_for", "venue_zone", "expires_at"],
            "properties": {
                "device": {"type": "string"},
                "event": {"type": "string"},
                "valid_for": {"type": "string"},
                "venue_zone": {"type": ["string", "null"]},
                "expires_at": {"type": "string"},
            },
        },
        "payload": {
            "type": "object",
            "required": ["ui_banner"],
            "properties": {
                "ui_banner": {
                    "type": "object",
                    "required": ["title", "metrics", "actions"],
                    "properties": {
                        "title": {"type": "string"},
                        "metrics": {"type": "array", "items": {"type": "string"}},
                        "actions": {
                            "type": "array",
                            "items": {
                                "type": "object",
                                "required": ["type", "metric", "operator", "threshold"],
                                "properties": {
                                    "type": {"type": "string"},
                                    "metric": {"type": "string"},
                                    "operator": {"type": "string"},
                                    "threshold": {"type": ["number", "integer"]},
                                    "suggestion_strategy": {"type": "string"},
                                },
                            },
                        },
                    },
                },
            },
        },
        "provenance": {
            "type": "object",
            "required": ["created_at", "generator", "data_minimization"],
            "properties": {
                "created_at": {"type": "string"},
                "generator": {"type": "string"},
                "data_minimization": {"type": "boolean"},
            },
        },
    },
}


def validate_schema(data: Dict[str, Any], schema: Dict[str, Any]):
    """
    Very small JSON-schema-ish validator (no external deps).

    Returns:
        (ok: bool, errors: List[str])
    """
    errors: List[str] = []

    def _check(obj: Any, sch: Dict[str, Any], path: str = "$"):
        t = sch.get("type")

        if t == "object":
            if not isinstance(obj, dict):
                errors.append(f"{path}: expected object, got {type(obj).__name__}")
                return
            # Required keys present?
            for req in sch.get("required", []):
                if req not in obj:
                    errors.append(f"{path}: missing required key '{req}'")
            # Validate known properties (ignore extra keys for forward-compat)
            props = sch.get("properties", {})
            for k, v in obj.items():
                if k in props:
                    _check(v, props[k], f"{path}.{k}")

        elif t == "array":
            if not isinstance(obj, list):
                errors.append(f"{path}: expected array, got {type(obj).__name__}")
                return
            item_schema = sch.get("items")
            if item_schema:
                for i, it in enumerate(obj):
                    _check(it, item_schema, f"{path}[{i}]")

        elif t == "string":
            if not isinstance(obj, str):
                errors.append(f"{path}: expected string, got {type(obj).__name__}")

        elif t == "boolean":
            if not isinstance(obj, bool):
                errors.append(f"{path}: expected boolean, got {type(obj).__name__}")

        elif t in ("number", "integer"):
            if not isinstance(obj, (int, float)):
                errors.append(f"{path}: expected number, got {type(obj).__name__}")

        elif isinstance(t, list):
            # Permissive union types (e.g., ["string", "null"])
            if obj is None and "null" in t:
                return
            if "string" in t and isinstance(obj, str):
                return
            if "number" in t and isinstance(obj, (int, float)):
                return
            if "integer" in t and isinstance(obj, int):
                return
            errors.append(f"{path}: value type not in allowed {t}")

        # Else: no type specified → leave unchecked

    _check(data, schema, "$")
    return (len(errors) == 0, errors)


# =============================================================================
# PRIVATE-SIDE EXPLANATION ("REASON WHY")
# =============================================================================

def explain_reason(guest: PrivateLeisureProfile, ctx: ParkAppContext) -> str:
    """
    Human-readable explanation that stays PRIVATE.

    It can mention the sensitive rationale because it never leaves the device.
    """
    parts: List[str] = []
    if guest.prefers_gentle_experiences:
        parts.append("Your profile prefers gentle experiences.")
    if ctx.venue == "LunaLand" and ctx.device == "park_app" and ctx.event == "enter_park":
        parts.append("You're starting a LunaLand app session right now.")
    parts.append("We shared only a time-bound UI instruction; no sensitive details left your device.")
    return " ".join(parts)


# =============================================================================
# HARNESS — LIGHT TESTS TO KEEP US HONEST
# =============================================================================

def run_harness(guest: PrivateLeisureProfile, ctx: ParkAppContext, catalog: List[Attraction]):
    """
    Run a small suite of checks:
      - derive_in_context: only in LunaLand/park_app/enter_park
      - minimization_no_sensitive_terms: envelope contains no sensitive words/pseudonym
      - no_derivation_outside_venue: different venue → no payload
      - behavior_alternative_logic: high intensity → suggest; low → don’t suggest
      - time_bound_envelope: expiry window is within 8 hours
      - envelope_schema_valid: envelope matches our JSON-ish schema
    """
    results: List[Tuple[str, bool, Optional[List[str]]]] = []

    # 1) Context-specific derivation
    payload = derive_leisure_ui_insight(guest, ctx)
    results.append(("derive_in_context", payload is not None, None))

    # 2) Envelope should not contain sensitive words or the pseudonym
    if payload is not None:
        env = create_envelope(payload, ctx)
        text = json.dumps(asdict(env)).lower()
        forbidden = [
            "vestibular", "motion", "sickness", "anxiety", "afraid",
            guest.subject_pseudonym.lower()
        ]
        results.append(("minimization_no_sensitive_terms", not any(f in text for f in forbidden), None))
    else:
        results.append(("minimization_no_sensitive_terms", False, None))

    # 3) Different venue → no derivation
    other_ctx = ParkAppContext("OtherPark", "park_app", "enter_park", ctx.timestamp, "west_gate")
    other_payload = derive_leisure_ui_insight(guest, other_ctx)
    results.append(("no_derivation_outside_venue", other_payload is None, None))

    # 4) Behavior (runtime logic): high intensity suggests; low does not
    behavior_ok = False
    if payload is not None:
        env = create_envelope(payload, ctx)
        high = next(a for a in catalog if a.intensity_score >= 8)
        low  = next(a for a in catalog if a.intensity_score <= 3)
        banner_high = venue_runtime_render_banner(env, high, catalog, current_zone="North")
        banner_low  = venue_runtime_render_banner(env, low, catalog, current_zone="North")
        behavior_ok = (
            banner_high["suggested_alternative"] is not None and
            banner_low["suggested_alternative"] is None
        )
    results.append(("behavior_alternative_logic", behavior_ok, None))

    # 5) Time-bound envelope (<= 8 hours)
    expiry_ok = False
    if payload is not None:
        env = create_envelope(payload, ctx)
        from datetime import datetime as dt
        expires_at = dt.fromisoformat(env.scope["expires_at"])
        expiry_ok = 0 < (expires_at - ctx.timestamp).total_seconds() <= 8 * 3600 + 5
    results.append(("time_bound_envelope", expiry_ok, None))

    # 6) Schema validation
    if payload is not None:
        env = create_envelope(payload, ctx)
        ok, errs = validate_schema(asdict(env), ENVELOPE_SCHEMA)
        results.append(("envelope_schema_valid", ok, errs if not ok else None))
    else:
        results.append(("envelope_schema_valid", False, ["no payload -> no envelope"]))

    return results


# =============================================================================
# PROGRAM ENTRY POINT
# =============================================================================

def main():
    # --- PRIVATE side profile (sensitive) ---
    guest = PrivateLeisureProfile("guest-rot-5577", True)

    # --- PUBLIC app context (safe to share) ---
    # now = datetime.now(timezone.utc)
    now = datetime.fromisoformat("2025-08-12T20:03:21.942379+00:00")
    ctx = ParkAppContext("LunaLand", "park_app", "enter_park", now, "north_gate")

    # --- PUBLIC venue catalog ---
    catalog = [
        Attraction("CAR-001", "Carousel Dreams", "Central", 2, 5),
        Attraction("RIV-010", "River Boats", "North", 3, 10),
        Attraction("COA-020", "Coaster Thunder", "North", 8, 35),
        Attraction("DRO-030", "Drop Tower", "East", 9, 25),
        Attraction("FLI-040", "Flight Spinner", "Central", 7, 20),
    ]

    # --- Derive + build envelope ---
    payload = derive_leisure_ui_insight(guest, ctx)
    if not payload:
        print("No insight derived for this context.")
        return
    env = create_envelope(payload, ctx)

    # --- Private explanation ---
    reason = explain_reason(guest, ctx)

    # --- Runtime preview: pick a high-intensity attraction to trigger suggestion ---
    selected = next(a for a in catalog if a.intensity_score >= 8)
    banner = venue_runtime_render_banner(env, selected, catalog, current_zone="North")

    # --- Outputs ---
    print("=== A — ANSWER (insight to venue) ===")
    print(json.dumps(asdict(env), indent=2))

    print("\n=== R — REASON WHY (private) ===")
    print(reason)

    print("\n=== Runtime preview (selected attraction) ===")
    print(json.dumps(banner, indent=2))

    print("\n=== C — CHECK (harness) ===")
    results = run_harness(guest, ctx, catalog)
    passed = sum(1 for _, ok, _ in results if ok)
    total = len(results)
    for name, ok, extra in results:
        line = f"{'PASS' if ok else 'FAIL'} - {name}"
        if extra:
            line += f" — details: {extra}"
        print(line)
    print(f"Overall: {passed}/{total} checks passed.")


if __name__ == "__main__":
    main()

