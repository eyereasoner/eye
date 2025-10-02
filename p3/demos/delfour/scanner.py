#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
scanner.py — retailer scanner (privacy-first)
- verifies signature of (insight.ttl + policy.ttl)
- authorizes runtime request, runs shopping rule
- writes audit.ttl, banner.json
- writes checks.json (harness) with pass/fail booleans

Usage:
  python scanner.py --init --session S123
  python scanner.py --session S123
"""

from __future__ import annotations
import argparse, os, sys, json, subprocess, tempfile, re, base64, hashlib
from datetime import datetime, timezone

# ---- crypto (PyNaCl) ----
try:
    from nacl.signing import VerifyKey
    from nacl.exceptions import BadSignatureError
except Exception as e:
    print("PyNaCl is required: pip install pynacl", file=sys.stderr)
    raise

# ---------- CLI ----------
def parse_args():
    ap = argparse.ArgumentParser()
    ap.add_argument("--bus", default="./bus", help="Databus root (default: ./bus)")
    ap.add_argument("--session", required=True, help="Session id (e.g., S123)")
    ap.add_argument("--init", action="store_true", help="Write catalog for this session and exit")
    return ap.parse_args()

EYE_CMD = os.environ.get("EYE_CMD", "eye")
EYE_FLAGS = ["--quiet", "--nope", "--pass-only-new"]

def now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()

def ensure_dir(p: str):
    os.makedirs(p, exist_ok=True)

def chunk_has_xsd_prefix(chunk: str) -> bool:
    return "@prefix xsd:" in chunk

def reorder_chunks(chunks: list[str]) -> list[str]:
    if not chunks: return chunks
    if chunk_has_xsd_prefix(chunks[0]): return chunks
    for i, ch in enumerate(chunks):
        if chunk_has_xsd_prefix(ch):
            return [ch] + chunks[:i] + chunks[i+1:]
    return chunks

def run_eye(chunks: list[str]) -> str:
    ordered = reorder_chunks(chunks)
    with tempfile.NamedTemporaryFile("w+", suffix=".n3", delete=False) as f:
        for c in ordered:
            f.write(c.strip()); f.write("\n\n")
        path = f.name
    args = [EYE_CMD] + EYE_FLAGS + [path]
    proc = subprocess.run(args, capture_output=True, text=True)
    if proc.returncode != 0:
        sys.stderr.write(f"EYE error\ncmd: {' '.join(args)}\n{proc.stderr}\n")
        sys.exit(1)
    return proc.stdout.strip()

def read_text(path: str) -> str:
    if not os.path.exists(path):
        sys.stderr.write(f"Missing {path}\n")
        sys.exit(1)
    return open(path, "r").read()

def canonicalize_ttl(text: str) -> str:
    lines = []
    for ln in text.splitlines():
        s = ln.strip()
        if not s or s.startswith("@prefix"):
            continue
        s = " ".join(s.split())
        lines.append(s)
    lines.sort()
    return "\n".join(lines) + ("\n" if lines else "")

def strip_prefixes(n3: str) -> str:
    return "\n".join(ln for ln in n3.splitlines() if not ln.lstrip().startswith("@prefix")).strip()

# ---------- local data & rules ----------
CATALOG = """@prefix prod:   <https://example.org/product#> .
@prefix schema: <http://schema.org/> .
@prefix xsd:    <http://www.w3.org/2001/XMLSchema#> .

prod:BIS_001 a schema:Product ; schema:name "Classic Tea Biscuits" ; schema:sku "BIS-001" ; schema:sugarContent "12.0"^^xsd:decimal ; schema:price "2.10"^^xsd:decimal .
prod:BIS_101 a schema:Product ; schema:name "Low-Sugar Tea Biscuits" ; schema:sku "BIS-101" ; schema:sugarContent "3.0"^^xsd:decimal  ; schema:price "2.60"^^xsd:decimal .
prod:CHOC_050 a schema:Product ; schema:name "Milk Chocolate Bar"   ; schema:sku "CHOC-050" ; schema:sugarContent "15.0"^^xsd:decimal ; schema:price "1.80"^^xsd:decimal .
prod:CHOC_150 a schema:Product ; schema:name "85% Dark Chocolate"   ; schema:sku "CHOC-150" ; schema:sugarContent "6.0"^^xsd:decimal  ; schema:price "2.20"^^xsd:decimal .
"""

ODRL_ENFORCE = """@prefix odrl: <http://www.w3.org/ns/odrl/2/> .
@prefix ins:  <https://example.org/insight#> .
@prefix ex:   <https://example.org/enforce#> .

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

{
  ?req a ex:Allowed ; ex:target ?ins .
  ?ins a ex:Expired .
}
=>
{ ?req a ex:Blocked ; ex:reason "expired" . } .
"""

EXPIRY_GUARD = """@prefix ins:  <https://example.org/insight#> .
@prefix ex:   <https://example.org/enforce#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .

{
  ?ins a ins:Insight ; ins:expiresAt ?exp .
  []   ex:now ?now .
  ?exp math:lessThan ?now .
}
=> { ?ins a ex:Expired . } .
"""

# Audit only typed requests (Option B)
AUDIT_RULES = """@prefix ex:   <https://example.org/enforce#> .
@prefix act:  <https://example.org/activity#> .
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .
@prefix req:  <https://example.org/request#> .

{ ?r a ex:Allowed . ?r a req:Request . [] ex:now ?now . }
=> { _:a a act:Decision ; act:at ?now ; act:request ?r ; act:outcome "Allowed" . } .

{ ?r a ex:Blocked ; ex:reason ?why . ?r a req:Request . [] ex:now ?now . }
=> { _:a a act:Decision ; act:at ?now ; act:request ?r ; act:outcome "Blocked" ; act:reason ?why . } .

{ ?ins a ex:Expired . [] ex:now ?now . }
=> { _:a a act:Duty ; act:type "delete_due" ; act:at ?now ; act:target ?ins . } .
"""

SHOPPING_RULE = """@prefix ins:   <https://example.org/insight#> .
@prefix schema: <http://schema.org/> .
@prefix shop:  <https://example.org/shop#> .
@prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
@prefix math:  <http://www.w3.org/2000/10/swap/math#> .

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

# Typed runtime request
REQUEST_USE = """@prefix odrl: <http://www.w3.org/ns/odrl/2/> .
@prefix req:  <https://example.org/request#> .

req:r1 a req:Request ;
      odrl:action odrl:use ;
      odrl:constraint [ odrl:leftOperand odrl:purpose ; odrl:rightOperand "shopping_assist" ] .
"""

def build_now(ts_iso: str) -> str:
    return f"""@prefix ex:  <https://example.org/enforce#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
[] ex:now "{ts_iso}"^^xsd:dateTime .
"""

def name_for_qname(qname: str, catalog: str) -> str | None:
    if not qname: return None
    m = re.search(re.escape(qname) + r"""\b[\s\S]*?schema:name\s+"([^"]+)""" , catalog)
    return m.group(1) if m else None

# ---------- init ----------
def write_catalog(session_dir: str):
    ensure_dir(session_dir)
    with open(os.path.join(session_dir, "catalog.ttl"), "w") as f:
        f.write(CATALOG.strip() + "\n")
    print(f"[scanner] Wrote {session_dir}/catalog.ttl")

# ---------- signature verification ----------
def verify_envelope_signature(session_dir: str):
    insight_path = os.path.join(session_dir, "insight.ttl")
    policy_path  = os.path.join(session_dir, "policy.ttl")
    sig_path     = os.path.join(session_dir, "envelope.sig.json")

    for p in (insight_path, policy_path, sig_path):
        if not os.path.exists(p):
            sys.stderr.write(f"Missing {p}\n")
            sys.exit(1)

    insight_txt = open(insight_path, "r").read()
    policy_txt  = open(policy_path, "r").read()
    payload     = canonicalize_ttl(insight_txt) + canonicalize_ttl(policy_txt)
    digest      = hashlib.sha256(payload.encode("utf-8")).hexdigest()

    sig_obj = json.load(open(sig_path, "r"))
    pk_b64  = sig_obj.get("publicKeyBase64")
    sig_b64 = sig_obj.get("signatureBase64")
    rec_hash= sig_obj.get("payloadHashSHA256")
    if not (pk_b64 and sig_b64 and rec_hash):
        sys.stderr.write("Signature file incomplete.\n")
        sys.exit(1)

    if rec_hash.lower() != digest.lower():
        sys.stderr.write("Hash mismatch: envelope content changed.\n")
        sys.exit(1)

    try:
        vk = VerifyKey(base64.b64decode(pk_b64))
        vk.verify(payload.encode("utf-8"), base64.b64decode(sig_b64))
    except BadSignatureError:
        sys.stderr.write("Bad signature: verification failed.\n")
        sys.exit(1)

    print("[scanner] Signature OK.")

# ---------- run ----------
def run_session(session_dir: str):
    ensure_dir(session_dir)

    # 0) verify signature first
    verify_envelope_signature(session_dir)

    # Load envelope (now trusted)
    insight = read_text(os.path.join(session_dir, "insight.ttl"))
    policy  = read_text(os.path.join(session_dir, "policy.ttl"))

    # Catalog
    cat_path = os.path.join(session_dir, "catalog.ttl")
    catalog = read_text(cat_path) if os.path.exists(cat_path) else CATALOG

    # --- Authorization (+ audit)
    now1 = now_iso()
    auth_out = run_eye([policy, insight, build_now(now1), EXPIRY_GUARD, ODRL_ENFORCE, AUDIT_RULES, REQUEST_USE])

    allowed = ("Allowed" in auth_out) and not ("Blocked" in auth_out)
    blocked = ("Blocked" in auth_out)

    # --- Shopping step (simulate one scan)
    banner = {"headline": None, "product_name": None, "note": None, "suggested_alternative": None}
    alt_qn = None
    if blocked and not allowed:
        banner["headline"] = "Policy blocked action"
    else:
        scan = """@prefix shop: <https://example.org/shop#> .
@prefix prod: <https://example.org/product#> .
[] shop:scannedProduct prod:BIS_001 .
"""
        sugg = run_eye([insight, catalog, scan, SHOPPING_RULE])
        m = re.search(r"shop:suggestedAlternative\s+([^\s]+)\s*\.", sugg)
        alt_qn = m.group(1) if m else None

        scanned_qn = "prod:BIS_001"
        banner["headline"] = "Track sugar per serving while you scan"
        banner["product_name"] = name_for_qname(scanned_qn, catalog) or "Unknown"
        banner["note"] = "High sugar" if alt_qn else None
        banner["suggested_alternative"] = name_for_qname(alt_qn, catalog) if alt_qn else None

    # --- Close-out (+ duty)
    now2 = now_iso()
    close_out = run_eye([insight, build_now(now2), EXPIRY_GUARD, AUDIT_RULES])

    # --- Write outputs
    with open(os.path.join(session_dir, "audit.ttl"), "w") as f:
        f.write(strip_prefixes((auth_out.strip() + ("\n" + close_out.strip() if close_out.strip() else "")).strip()) + "\n")
    with open(os.path.join(session_dir, "banner.json"), "w") as f:
        json.dump(banner, f, indent=2)

    # --- Checks (harness)
    insight_lc = (insight or "").lower()
    audit_txt = strip_prefixes((auth_out.strip() + ("\n" + close_out.strip() if close_out.strip() else "")).strip())
    checks = [
        ["insight_nonempty", bool(insight.strip())],
        ["minimization_no_sensitive_terms", ("diabetes" not in insight_lc and "medical" not in insight_lc)],
        ["scope_has_device_event_expiry", all(k in insight_lc for k in ["scopedevice", "scopeevent", "expiresat"])],
        ["runtime_present", True],  # we always write a banner (Allowed or Blocked)
        ["behavior_suggests_on_high_sugar", bool(alt_qn)],
        ["audit_has_decision", ("activity#Decision" in audit_txt) or ("act:Decision" in audit_txt)],
    ]
    with open(os.path.join(session_dir, "checks.json"), "w") as f:
        json.dump(checks, f, indent=2)

    print(f"[scanner] Wrote {session_dir}/audit.ttl, banner.json, checks.json")

def main():
    a = parse_args()
    session_dir = os.path.join(a.bus, a.session)
    if a.init:
        write_catalog(session_dir)
        return
    run_session(session_dir)

if __name__ == "__main__":
    main()

