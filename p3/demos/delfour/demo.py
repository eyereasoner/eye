#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Delfour Insight Economy — console demo (EYE CLI)

What this does — mirrors the browser demo (eye-js + Pyodide) but via the local EYE CLI:

Pipeline (high level)
1) DESENSITIZE   : profile + rule -> neutral "need" graph (no sensitive term leak)
2) INSIGHT       : need + context -> Insight envelope (device/event/retailer/expiry)
3) POLICY        : Insight -> ODRL policy (allow use@shopping_assist, prohibit share@marketing, duty delete@expiry)
4) AUTH          : {policy, request, now, expiry-guard, enforce} -> Allowed / Blocked + audit events
5) SHOPPING      : scan + Insight + product catalog -> suggestion if sugar ≥ threshold
6) CLOSE-OUT     : new "now" + expiry-guard + audit rules -> delete_due duty if expired

Console output (sections)
- Timeline
- Answer — Insight (to retailer)          : the Insight envelope (N3, emitted by EYE)
- Answer — Runtime Preview (device)       : banner JSON shown to the shopper
- Reason Why (private)                    : operator-facing explanation
- Checks                                  : quick “green ticks” for sanity
- Audit (derived)                         : decisions/duties derived by EYE (prefix lines removed for readability)

Assumptions
- The `eye` binary is available on your PATH.
- We pass `--quiet --nope --pass-only-new` to EYE so stdout contains only *derivations*.
- Each Turtle/N3 “piece” is self-prefixed. The runner simply ensures that a chunk
  that declares `@prefix xsd:` appears first to satisfy strict EYE builds.
"""

from __future__ import annotations
import subprocess, tempfile, os, re, json
from datetime import datetime, timedelta, timezone
from typing import List, Tuple, Optional

# ---------- tiny print utils ----------
def now_iso() -> str: return datetime.now(timezone.utc).isoformat()
def section(t): print(t); print("-"*len(t))
def show_timeline(tl: List[Tuple[str,str,str]]):
    section("Timeline")
    for ts,ev,de in tl: print(f"{ts}  {ev:16s} {de}")
    print()
def show_text(title, txt): section(title); print((txt or "").rstrip()); print()
def show_json(title, obj): section(title); print(json.dumps(obj, indent=2)); print()
def show_checks(rows): section("Checks"); [print(("✓" if ok else "✗")+f" {name}") for name,ok in rows]; print()

# ---------- data & rules ----------
# NOTE on comments: Turtle/N3 comments start with '#'. Everything below is exactly
# the same logic you had; we’ve just annotated the pieces for maintainability.

PROFILE = """# --- POD/household profile (private) ---
# Prefixes:
@prefix :       <https://example.org/person#> .
@prefix health: <https://example.org/health#> .

# Claim: Someone in the household has diabetes (sensitive fact).
# We will not carry this term forward; the DESENSITIZE rule maps it to a neutral 'need'.
:me  health:householdCondition  health:Diabetes .
"""

CATALOG = """# --- Retailer product catalog (tiny excerpt) ---
@prefix prod:   <https://example.org/product#> .
@prefix schema: <http://schema.org/> .
@prefix xsd:    <http://www.w3.org/2001/XMLSchema#> .

# A few products with sugar_g_per_serving (as xsd:decimal) and price.
prod:BIS_001 a schema:Product ; schema:name "Classic Tea Biscuits" ; schema:sku "BIS-001" ; schema:sugarContent "12.0"^^xsd:decimal ; schema:price "2.10"^^xsd:decimal .
prod:BIS_101 a schema:Product ; schema:name "Low-Sugar Tea Biscuits" ; schema:sku "BIS-101" ; schema:sugarContent "3.0"^^xsd:decimal  ; schema:price "2.60"^^xsd:decimal .
prod:CHOC_050 a schema:Product ; schema:name "Milk Chocolate Bar"   ; schema:sku "CHOC-050" ; schema:sugarContent "15.0"^^xsd:decimal ; schema:price "1.80"^^xsd:decimal .
prod:CHOC_150 a schema:Product ; schema:name "85% Dark Chocolate"   ; schema:sku "CHOC-150" ; schema:sugarContent "6.0"^^xsd:decimal  ; schema:price "2.20"^^xsd:decimal .
"""

DESENSITIZE_RULE = """# --- Desensitization: sensitive -> neutral need ---
@prefix health: <https://example.org/health#> .
@prefix need:   <https://example.org/need#> .

# If the POD says "householdCondition Diabetes", we only keep a neutral boolean:
# need:needsLowSugar true. No explicit medical term is carried forward.
{ ?p health:householdCondition health:Diabetes. } => { ?p need:needsLowSugar true. } .
"""

DERIVE_RULE = """# --- Derive the Insight envelope from need + current context ---
@prefix need: <https://example.org/need#> .
@prefix ctx:  <https://example.org/context#> .
@prefix ins:  <https://example.org/insight#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# GIVEN: a neutral need + retailer/device/event/expiry context …
# EMIT : a scoped, time-bounded Insight with a metric threshold and suggestion policy.
{
  ?p need:needsLowSugar true.
  ?c ctx:retailer "Delfour";
     ctx:device   "self-scanner";
     ctx:event    "pick_up_scanner";
     ctx:expiresAt ?exp.
}
=>
{
  _:ins a ins:Insight ;
    ins:metric           "sugar_g_per_serving" ;
    ins:threshold        "10.0"^^xsd:decimal ;
    ins:suggestionPolicy "lower_metric_first_higher_price_ok" ;
    ins:scopeDevice      "self-scanner" ;
    ins:scopeEvent       "pick_up_scanner" ;
    ins:retailer         "Delfour" ;
    ins:expiresAt        ?exp .
}.
"""

ODRL_FROM_INSIGHT = """# --- Derive an ODRL policy from the Insight itself ---
@prefix ins:  <https://example.org/insight#> .
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# Allow 'use' of this Insight only for purpose=shopping_assist on the device/event above.
# Prohibit 'share' for purpose=marketing. Duty to delete at expiry.
{
  ?ins a ins:Insight ;
       ins:expiresAt ?exp ;
       ins:retailer "Delfour" ;
       ins:scopeDevice "self-scanner" ;
       ins:scopeEvent  "pick_up_scanner" .
}
=>
{
  _:pol a odrl:Policy ;
        odrl:profile "Delfour-Insight-Policy" ;
        odrl:permission [
          odrl:action odrl:use ;
          odrl:target ?ins ;
          odrl:constraint [
            odrl:leftOperand odrl:purpose ;
            odrl:operator    odrl:eq ;
            odrl:rightOperand "shopping_assist"
          ]
        ] ;
        odrl:prohibition [
          odrl:action odrl:share ;
          odrl:target ?ins ;
          odrl:constraint [
            odrl:leftOperand odrl:purpose ;
            odrl:operator    odrl:eq ;
            odrl:rightOperand "marketing"
          ]
        ] ;
        odrl:duty [
          odrl:action odrl:delete ;
          odrl:constraint [
            odrl:leftOperand odrl:dateTime ;
            odrl:operator    odrl:eq ;
            odrl:rightOperand ?exp
          ]
        ] .
}.
"""

ODRL_ENFORCE = """# --- Enforcement: map policy + request to Allowed/Blocked ---
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .
@prefix ins:  <https://example.org/insight#> .
@prefix ex:   <https://example.org/enforce#> .

# Allowed if there exists a policy that permits odrl:use@shopping_assist over some Insight.
{
  ?pol a odrl:Policy ;
       odrl:permission [
         odrl:action odrl:use ;
         odrl:constraint [ odrl:leftOperand odrl:purpose ; odrl:operator odrl:eq ; odrl:rightOperand "shopping_assist" ] ;
         odrl:target ?ins
       ] .
  ?ins a ins:Insight .
  ?req odrl:action odrl:use ;
       odrl:constraint [ odrl:leftOperand odrl:purpose ; odrl:rightOperand "shopping_assist" ] .
}
=>
{ ?req a ex:Allowed ; ex:target ?ins . } .

# If the target Insight is expired, block the request (reason="expired").
{
  ?req a ex:Allowed ; ex:target ?ins .
  ?ins a ex:Expired .
}
=>
{ ?req a ex:Blocked ; ex:reason "expired" . } .
"""

EXPIRY_GUARD = """# --- Expiry guard: derive ex:Expired when exp < now ---
@prefix ins:  <https://example.org/insight#> .
@prefix ex:   <https://example.org/enforce#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .

{
  ?ins a ins:Insight ; ins:expiresAt ?exp .
  []   ex:now ?now .
  ?exp math:lessThan ?now .
}
=> { ?ins a ex:Expired . } .
"""

AUDIT_RULES = """# --- Audit trail: write activity events for decisions and duties ---
@prefix ex:   <https://example.org/enforce#> .
@prefix act:  <https://example.org/activity#> .

# Record Allowed decision with timestamp.
{ ?req a ex:Allowed . [] ex:now ?now . }
=> { _:a a act:Decision ; act:at ?now ; act:request ?req ; act:outcome "Allowed" . } .

# Record Blocked decision and reason.
{ ?req a ex:Blocked ; ex:reason ?r . [] ex:now ?now . }
=> { _:a a act:Decision ; act:at ?now ; act:request ?req ; act:outcome "Blocked" ; act:reason ?r . } .

# Record delete_due duty when an Insight is expired at "now".
{ ?ins a ex:Expired . [] ex:now ?now . }
=> { _:a a act:Duty ; act:type "delete_due" ; act:at ?now ; act:target ?ins . } .
"""

SHOPPING_RULE = """# --- Shopping-time suggestion based on Insight threshold ---
@prefix ins:   <https://example.org/insight#> .
@prefix schema: <http://schema.org/> .
@prefix shop:  <https://example.org/shop#> .
@prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
@prefix math:  <http://www.w3.org/2000/10/swap/math#> .

# If the scanned product's sugar >= threshold, flag "High sugar" and suggest any
# alternative with strictly lower sugar (price may be higher; policy allows it).
{
  ?ins a ins:Insight; ins:metric "sugar_g_per_serving"; ins:threshold ?thr.
  ?scan shop:scannedProduct ?p.
  ?p schema:name ?name; schema:sugarContent ?sug.
  ?sug math:notLessThan ?thr.
  ?alt schema:name ?an ; schema:sugarContent ?as ; schema:price ?ap .
  ?as math:lessThan ?sug.
}
=> { ?scan shop:note "High sugar" ; shop:suggestedAlternative ?alt . } .
"""

# context, now, request, scan — each with its own prefixes
def build_context_n3() -> str:
    """Session context — device/event/retailer and a 2-hour expiry window."""
    now = datetime.now(timezone.utc)
    exp = now + timedelta(hours=2)
    return f"""# --- Session context (scoped, short-lived) ---
@prefix ctx: <https://example.org/context#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

[] ctx:retailer "Delfour" ;
   ctx:device   "self-scanner" ;
   ctx:event    "pick_up_scanner" ;
   ctx:timestamp "{now.isoformat()}"^^xsd:dateTime ;
   ctx:expiresAt "{exp.isoformat()}"^^xsd:dateTime .
"""

def build_now_graph(ts_iso: str) -> str:
    """A tiny 'now' fact to compare against expiresAt and to timestamp audit events."""
    return f"""# --- Current time fact used by expiry_guard and audit_rules ---
@prefix ex:  <https://example.org/enforce#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

[] ex:now "{ts_iso}"^^xsd:dateTime .
"""

REQUEST_USE_SHOPPING = """# --- An incoming request we want to authorize (purpose = shopping_assist) ---
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .
@prefix req:  <https://example.org/request#> .

req:r1 odrl:action odrl:use ;
      odrl:constraint [ odrl:leftOperand odrl:purpose ; odrl:rightOperand "shopping_assist" ] .
"""

SCAN_GRAPH = """# --- Shopper scans a product (BIS_001) on the device ---
@prefix shop: <https://example.org/shop#> .
@prefix prod: <https://example.org/product#> .

[] shop:scannedProduct prod:BIS_001 .
"""

# ---------- EYE invocation ----------
EYE_CMD = os.environ.get("EYE_CMD", "eye")

def chunk_has_xsd_prefix(chunk: str) -> bool:
    """True if the piece declares @prefix xsd: (needed early for typed literals)."""
    return "@prefix xsd:" in chunk

def reorder_chunks(chunks: List[str]) -> List[str]:
    """Place a chunk declaring @prefix xsd: first (if any) to satisfy strict parsers."""
    if not chunks: return chunks
    if chunk_has_xsd_prefix(chunks[0]): return chunks
    for i, ch in enumerate(chunks):
        if chunk_has_xsd_prefix(ch):
            return [ch] + chunks[:i] + chunks[i+1:]
    return chunks  # none declare xsd: (fine if no typed literals)

def run_eye(n3_chunks: List[str]) -> str:
    """
    Write all chunks into one temporary .n3 and ask EYE to print derivations only.
    We keep a fixed, safe flag set (no auto-retry) to avoid policy-sensitive surprises.
    """
    # Ensure file begins with a self-prefixed chunk that declares xsd: if needed.
    ordered = reorder_chunks(n3_chunks)
    with tempfile.NamedTemporaryFile("w+", suffix=".n3", delete=False) as f:
        for part in ordered:
            f.write(part.strip()); f.write("\n\n")
        path = f.name

    args = [EYE_CMD] + ["--quiet", "--nope", "--pass-only-new"] + [path]
    proc = subprocess.run(args, capture_output=True, text=True)
    if proc.returncode == 0 and proc.stdout.strip():
        return proc.stdout.strip()

    # If EYE complains, surface the exact command + stderr (helps portability).
    if proc.returncode != 0:
        raise RuntimeError(f"EYE failed to produce output.\ncmd: {' '.join(args)}\n{proc.stderr}")

    return ""  # no derivations (legitimate in some steps)

# ---------- helpers ----------
def extract_insight_block(n3: str) -> str:
    """Pull just the ins:Insight blank-node block so 'Answer — Insight' is compact."""
    if not n3.strip(): return ""
    m = re.search(r"(_:[A-Za-z0-9_]+)\s+a\s+ins:Insight\s*;[\s\S]*?\.", n3)
    if m: return m.group(0).strip()
    # fallback: keep any lines mentioning 'ins:'
    lines = [ln for ln in n3.splitlines() if " ins:" in ln or "ins:" in ln]
    return "\n".join(lines).strip()

def extract_qname(n3: str, pred: str) -> Optional[str]:
    """Find the object (QName/blank) for the first occurrence of a given predicate."""
    m = re.search(rf"{re.escape(pred)}\s+([^\s;]+)\s*\.", n3)
    return m.group(1) if m else None

def name_for_qname(qname: str) -> Optional[str]:
    """Lookup a product's schema:name inside the CATALOG string."""
    if not qname: return None
    m = re.search(re.escape(qname) + r"""\b[\s\S]*?schema:name\s+"([^"]+)""" , CATALOG)
    return m.group(1) if m else None

def strip_prefixes(n3: str) -> str:
    """Remove any @prefix lines anywhere in the text (cosmetic for Audit section)."""
    if not n3:
        return n3
    return "\n".join(
        ln for ln in n3.splitlines()
        if not ln.lstrip().startswith("@prefix")
    ).strip()

# ---------- pipeline ----------
def main():
    timeline: List[Tuple[str,str,str]] = []
    def log(ev, de=""): timeline.append((now_iso(), ev, de))

    # 01 PICKUP + 02 DIALOG — the device is started; retailer sends capabilities
    log("PICKUP", 'scanner="self-scanner" retailer="Delfour"')
    log("AGENT-DIALOG", "capabilities received from Delfour")

    # 03 DESENSITIZE — profile + rule -> neutral need graph (no sensitive terms)
    need = run_eye([PROFILE, DESENSITIZE_RULE])
    log("DESENSITIZE", "need=needsLowSugar written")

    # 04 INSIGHT — need + context -> Insight envelope
    context = build_context_n3()
    insight = run_eye([need, context, DERIVE_RULE])  # (reordered so a chunk with xsd: leads)
    log("INSIGHT", "neutral low-sugar envelope emitted" if insight else "no insight emitted")

    # 04b POLICY — Insight -> ODRL policy (for logging/traceability; not printed)
    _policy = run_eye([insight, ODRL_FROM_INSIGHT])
    log("POLICY", "ODRL policy emitted (use@shopping_assist; share@marketing prohibited)")

    # 05 AUTH — evaluate request under policy, guard for expiry, write audit events
    now1 = now_iso()
    auth_out = run_eye([
        insight,
        ODRL_FROM_INSIGHT,
        build_now_graph(now1),
        EXPIRY_GUARD,
        ODRL_ENFORCE,
        AUDIT_RULES,
        REQUEST_USE_SHOPPING,
    ])

    expired = "ex:Expired" in auth_out
    has_decision = ("activity#Decision" in auth_out) or ("act:Decision" in auth_out)
    allowed = has_decision and ("Allowed" in auth_out) and not ("Blocked" in auth_out)
    blocked = has_decision and ("Blocked" in auth_out)

    if expired: log("EXPIRY", "insight expired at now()")
    if allowed: log("AUTH", "Allowed (use@shopping_assist)")
    elif blocked: log("AUTH", "Blocked (expired)" if "expired" in auth_out else "Blocked (prohibited)")
    else: log("AUTH", "No explicit decision (check policy/request)")

    # 05 SHOPPING — simulate a scan; suggest a lower-sugar alternative if needed
    if blocked and not allowed:
        banner = {"headline":"Policy blocked action","product_name":None,"note":"Expired or prohibited","suggested_alternative":None}
    else:
        sugg = run_eye([insight, CATALOG, SCAN_GRAPH, SHOPPING_RULE])
        alt_qn = extract_qname(sugg, "shop:suggestedAlternative")
        scanned_qn = "prod:BIS_001"
        banner = {
            "headline": "Track sugar per serving while you scan",
            "product_name": name_for_qname(scanned_qn) or "Unknown",
            "note": "High sugar" if alt_qn else None,
            "suggested_alternative": name_for_qname(alt_qn) if alt_qn else None
        }
        if alt_qn: log("SCAN", f'product="{banner["product_name"]}" \u2192 suggest="{banner["suggested_alternative"]}"')
        else:      log("SCAN", f'product="{banner["product_name"]}" (ok)')
        log("RUNTIME", "banner written")

    # 06 DROP + CLOSE-OUT — close session; if expiry has passed, derive a delete_due duty
    log("DROP", "scanner returned; session closed")
    now2 = now_iso()
    close_out = run_eye([insight, build_now_graph(now2), EXPIRY_GUARD, AUDIT_RULES])
    if close_out.strip():
        if ("act:Duty" in close_out and 'act:type "delete_due"' in close_out): log("AUDIT","close-out appended (delete_due)")
        else:                                                                  log("AUDIT","close-out appended")

    # ---- output sections ----
    show_timeline(timeline)
    show_text("Answer — Insight (to retailer)", insight)
    show_json("Answer — Runtime Preview (device)", banner)

    reason = ("Household requires low-sugar guidance (diabetes in POD). "
              "You are starting a self-scanner session at Delfour right now. "
              "Envelope is minimized and limited to this session.")
    show_text("Reason Why (private)", reason)

    ilc = (insight or "").lower()
    checks = [
        ("insight_nonempty", bool((insight or "").strip())),
        ("minimization_no_sensitive_terms", ("diabetes" not in ilc and "medical" not in ilc)),
        ("scope_has_device_event_expiry", all(k in ilc for k in ["scopedevice","scopeevent","expiresat"])),
        ("runtime_present", True),
        ("behavior_suggests_on_high_sugar", banner.get("suggested_alternative") is not None),
        ("audit_has_decision", has_decision),
    ]
    show_checks(checks)

    raw_audit = (auth_out.strip() + ("\n" + close_out.strip() if close_out.strip() else "")).strip()
    audit_full = strip_prefixes(raw_audit)  # cosmetic: remove any @prefix lines from the audit printout
    show_text("Audit (derived)", audit_full or "")

if __name__ == "__main__":
    try:
        main()
    except RuntimeError as e:
        section("Error"); print(str(e)); print()

