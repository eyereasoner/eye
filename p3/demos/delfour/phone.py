#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
phone.py — shopper device (privacy-first)
- derives neutral Insight + ODRL policy
- writes .ttl files to BUS/SESSION
- signs (Ed25519) the canonicalized content of insight.ttl + policy.ttl
- writes a human "Reason Why" as reason.txt  (demo-only; keep on device in prod)

Usage:
  python phone.py --session S123
  python phone.py --bus ./mybus --session S123
  python phone.py --session S123 --keydir ./keys   # optional persistent key storage

Env:
  EYE_CMD=/path/to/eye  (optional; default: 'eye')
"""

from __future__ import annotations
import argparse, os, subprocess, tempfile, sys, json, base64, hashlib
from datetime import datetime, timedelta, timezone

# ---- crypto (PyNaCl) ----
try:
    from nacl.signing import SigningKey
except Exception as e:
    print("PyNaCl is required: pip install pynacl", file=sys.stderr)
    raise

# ---------- CLI ----------
def parse_args():
    ap = argparse.ArgumentParser()
    ap.add_argument("--bus", default="./bus", help="Databus root (default: ./bus)")
    ap.add_argument("--session", required=True, help="Session id (e.g., S123)")
    ap.add_argument("--retailer", default="Delfour")
    ap.add_argument("--device", default="self-scanner")
    ap.add_argument("--event", default="pick_up_scanner")
    ap.add_argument("--ttl-hours", type=float, default=2.0, help="Expiry window in hours")
    ap.add_argument("--keydir", default=None, help="Directory to store/load Ed25519 keypair (default: ephemeral)")
    return ap.parse_args()

EYE_CMD = os.environ.get("EYE_CMD", "eye")
EYE_FLAGS = ["--quiet", "--nope", "--pass-only-new"]  # derivations only

# ---------- utils ----------
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

def canonicalize_ttl(text: str) -> str:
    """Demo-grade canonicalization: drop @prefix, trim/collapse spaces, sort lines."""
    lines = []
    for ln in text.splitlines():
        s = ln.strip()
        if not s or s.startswith("@prefix"):
            continue
        s = " ".join(s.split())
        lines.append(s)
    lines.sort()
    return "\n".join(lines) + ("\n" if lines else "")

def now_iso_z() -> str:
    return datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")

# ---------- data & rules ----------
PROFILE = """@prefix :       <https://example.org/person#> .
@prefix health: <https://example.org/health#> .

:me  health:householdCondition  health:Diabetes .
"""

DESENSITIZE_RULE = """@prefix health: <https://example.org/health#> .
@prefix need:   <https://example.org/need#> .

{ ?p health:householdCondition health:Diabetes. } => { ?p need:needsLowSugar true. } .
"""

DERIVE_RULE_TPL = """@prefix need: <https://example.org/need#> .
@prefix ctx:  <https://example.org/context#> .
@prefix ins:  <https://example.org/insight#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

{
  ?p need:needsLowSugar true.
  ?c ctx:retailer ?ret ;
     ctx:device   ?dev ;
     ctx:event    ?ev  ;
     ctx:expiresAt ?exp .
}
=>
{
  %(INS_IRI)s a ins:Insight ;
    ins:metric           "sugar_g_per_serving" ;
    ins:threshold        "10.0"^^xsd:decimal ;
    ins:suggestionPolicy "lower_metric_first_higher_price_ok" ;
    ins:scopeDevice      ?dev ;
    ins:scopeEvent       ?ev ;
    ins:retailer         ?ret ;
    ins:expiresAt        ?exp .
}.
"""

ODRL_FROM_INSIGHT_TPL = """@prefix ins:  <https://example.org/insight#> .
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

{
  %(INS_IRI)s a ins:Insight ;
              ins:expiresAt ?exp ;
              ins:retailer ?ret ;
              ins:scopeDevice ?dev ;
              ins:scopeEvent  ?ev .
}
=>
{
  _:pol a odrl:Policy ;
        odrl:profile "Delfour-Insight-Policy" ;
        odrl:permission [
          odrl:action odrl:use ;
          odrl:target %(INS_IRI)s ;
          odrl:constraint [
            odrl:leftOperand  odrl:purpose ;
            odrl:operator     odrl:eq ;
            odrl:rightOperand "shopping_assist"
          ]
        ] ;
        odrl:prohibition [
          odrl:action odrl:share ;
          odrl:target %(INS_IRI)s ;
          odrl:constraint [
            odrl:leftOperand  odrl:purpose ;
            odrl:operator     odrl:eq ;
            odrl:rightOperand "marketing"
          ]
        ] ;
        odrl:duty [
          odrl:action odrl:delete ;
          odrl:constraint [
            odrl:leftOperand  odrl:dateTime ;
            odrl:operator     odrl:eq ;
            odrl:rightOperand ?exp
          ]
        ] .
}.
"""

def build_context(retailer: str, device: str, event: str, ttl_hours: float) -> str:
    now = datetime.now(timezone.utc)
    exp = now + timedelta(hours=ttl_hours)
    return f"""@prefix ctx: <https://example.org/context#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

[] ctx:retailer "{retailer}" ;
   ctx:device   "{device}" ;
   ctx:event    "{event}" ;
   ctx:timestamp "{now.isoformat()}"^^xsd:dateTime ;
   ctx:expiresAt "{exp.isoformat()}"^^xsd:dateTime .
"""

def insight_iri(session_id: str) -> str:
    return f"<https://example.org/insight/{session_id}>"

def load_or_create_signing_key(keydir: str|None) -> SigningKey:
    if not keydir:
        return SigningKey.generate()
    ensure_dir(keydir)
    sk_path = os.path.join(keydir, "ed25519.sk")
    if os.path.exists(sk_path):
        raw = open(sk_path, "rb").read()
        return SigningKey(raw)
    sk = SigningKey.generate()
    with open(sk_path, "wb") as f:
        f.write(bytes(sk))
    pk_path = os.path.join(keydir, "ed25519.pk")
    with open(pk_path, "wb") as f:
        f.write(bytes(sk.verify_key))
    return sk

# ---------- main ----------
def main():
    a = parse_args()
    session_dir = os.path.join(a.bus, a.session)
    ensure_dir(session_dir)

    INS_IRI = insight_iri(a.session)

    # 1) profile + rule -> need
    need = run_eye([PROFILE, DESENSITIZE_RULE])

    # 2) need + context -> insight (named IRI)
    context = build_context(a.retailer, a.device, a.event, a.ttl_hours)
    derive_rule = DERIVE_RULE_TPL % {"INS_IRI": INS_IRI}
    insight = run_eye([context, need, derive_rule])

    # 3) policy from insight (same IRI)
    policy_rule = ODRL_FROM_INSIGHT_TPL % {"INS_IRI": INS_IRI}
    policy = run_eye([policy_rule, insight])

    # 4) write .ttl files
    insight_path = os.path.join(session_dir, "insight.ttl")
    policy_path  = os.path.join(session_dir, "policy.ttl")
    envelope_path= os.path.join(session_dir, "envelope.ttl")

    with open(insight_path, "w") as f:
        f.write(insight.strip() + "\n")
    with open(policy_path, "w") as f:
        f.write(policy.strip() + "\n")
    with open(envelope_path, "w") as f:
        f.write(insight.strip() + "\n\n" + policy.strip() + "\n")

    # 5) sign (Ed25519) over canonicalized (insight + policy)
    insight_txt = open(insight_path, "r").read()
    policy_txt  = open(policy_path, "r").read()
    payload = canonicalize_ttl(insight_txt) + canonicalize_ttl(policy_txt)

    digest = hashlib.sha256(payload.encode("utf-8")).hexdigest()
    sk = load_or_create_signing_key(a.keydir)
    sig = sk.sign(payload.encode("utf-8")).signature
    sig_b64 = base64.b64encode(sig).decode("ascii")
    pk_b64  = base64.b64encode(bytes(sk.verify_key)).decode("ascii")

    sig_obj = {
        "alg": "Ed25519",
        "keyid": "ed25519:default",
        "created": now_iso_z(),
        "publicKeyBase64": pk_b64,
        "payloadHashSHA256": digest,
        "signatureBase64": sig_b64
    }
    with open(os.path.join(session_dir, "envelope.sig.json"), "w") as f:
        json.dump(sig_obj, f, indent=2)

    # 6) write Reason Why (demo-only; would remain local in production)
    reason = ("Household requires low-sugar guidance (diabetes in POD). "
              "A neutral Insight is derived and scoped to this device & event at "
              f"{a.retailer}, expiring soon; the policy confines use to shopping assistance.")
    with open(os.path.join(session_dir, "reason.txt"), "w") as f:
        f.write(reason + "\n")

    print(f"[phone] Wrote and signed envelope in {session_dir}/")

if __name__ == "__main__":
    main()

