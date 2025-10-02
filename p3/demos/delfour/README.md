# Delfour Insight Economy — Dual Demos (Browser & Python Split)

This repo contains **two runnable demos** of the same privacy-first idea:

1. **Browser [demo.html](https://eyereasoner.github.io/eye/p3/demos/delfour/demo.html)** — everything runs client-side:

   * Orchestration in **Python (Pyodide)**
   * Reasoning by **eye-js**
   * Signing & verifying with **TweetNaCl**
     No installs. Open one file and click **Run Demo**.

2. **Device split (`phone.py` + `scanner.py`)** — two Python programs that mimic a **phone ↔ scanner** deployment:

   * Reasoning by **EYE** (CLI)
   * Signing & verifying with **PyNaCl (Ed25519)**
   * Exchange via a simple **file “databus”** (`./bus/<SESSION>/…`)

Both variants demonstrate: **desensitize → derive Insight → ODRL policy → authorize → runtime suggestion → audit**, with a signed, session-scoped **Insight envelope** (no sensitive terms leave the phone).

---

## Contents

```
demo.html          # Browser demo (Pyodide + eye-js + TweetNaCl)
phone.py           # Shopper device: derive Insight + Policy, sign, write to bus
scanner.py         # Retailer device: verify, authorize, suggest, audit
```

Artifacts produced (Python split) under `./bus/<SESSION>/`:

```
insight.ttl        # Neutral Insight (named IRI per session)
policy.ttl         # ODRL policy derived from the Insight
envelope.ttl       # insight + policy (for humans)
envelope.sig.json  # Ed25519 signature + public key (over canonicalized envelope)
catalog.ttl        # Product catalog (seeded by scanner --init)
audit.ttl          # Decisions (Allowed/Blocked) and duties (delete_due)
banner.json        # Shopper-facing runtime banner preview
```

---

## 1) Browser Demo (`demo.html`)

### What it shows

* **Phone** (left):
  profile → desensitize → **Insight** (scoped to device/event/retailer with expiry) → **Policy** → **sign** (Ed25519)
* **Scanner** (right):
  **verify signature** → **authorize** (`purpose=shopping_assist`, expiry guard) → **shopping rule** (lower sugar suggestion) → **audit**

### How to run

* Open `demo.html` in a modern browser (Chrome/Edge/Firefox/Safari).
* Click **Run Demo**.
  You’ll see timelines, the derived `insight.ttl`, `policy.ttl`, a `envelope.sig.json`, a `banner.json` preview, and `audit.ttl`.

### Under the hood

* **Reasoner:** [eye-js] runs the N3 rules in the browser.
* **Python orchestration:** [Pyodide] executes the same flow you see in the Python split.
* **Sign/verify:** [TweetNaCl]. For the demo, the keypair is ephemeral (per page load).
* **Canonicalization (demo-grade):** drop `@prefix` lines, trim & collapse spaces, sort lines, then sign the concatenation of `insight.ttl` + `policy.ttl`. (Production: use URDNA2015 over N-Quads.)

No network calls; everything is client-side.

---

## 2) Device Split (`phone.py` + `scanner.py`)

### Requirements

* **Python 3.9+**
* **EYE** CLI in `PATH` (or set `EYE_CMD=/path/to/eye`)
* **PyNaCl** for Ed25519:

  ```bash
  pip install pynacl
  ```

### Quick start

```bash
# 1) Seed a session catalog on the scanner
python scanner.py --init --session S1

# 2) Produce and SIGN the envelope on the phone
python phone.py --session S1 --keydir ./keys
# (keydir optional; makes the key persistent across runs)

# 3) Verify, authorize, suggest, and audit on the scanner
python scanner.py --session S1

# Inspect:
tree ./bus/S1
cat bus/S1/envelope.sig.json
cat bus/S1/audit.ttl
cat bus/S1/banner.json
```

### CLI reference

**phone.py**

```
python phone.py --session <ID> [--bus ./bus]
                [--retailer Delfour] [--device self-scanner]
                [--event pick_up_scanner] [--ttl-hours 2.0]
                [--keydir ./keys]
```

Writes: `insight.ttl`, `policy.ttl`, `envelope.ttl`, `envelope.sig.json`.

**scanner.py**

```
python scanner.py --init    --session <ID> [--bus ./bus]
python scanner.py           --session <ID> [--bus ./bus]
```

Writes: `catalog.ttl` (init), `audit.ttl`, `banner.json`.

### Data flow (Python split)

```
[Phone] profile + rule ──► need
        need + context ──► insight.ttl       (IRI: https://example.org/insight/<SESSION>)
        insight ─────────► policy.ttl        (targets the same IRI)
        insight+policy ──► sign (Ed25519)    → envelope.sig.json
                           ▼
[Bus/SESSION]  insight.ttl, policy.ttl, envelope.sig.json
                           ▼
[Scanner] verify signature ✔
         now + request + rules ──► Allowed/Blocked → audit.ttl
         scan + catalog + Insight ──► suggestion → banner.json
         expiry guard ───────────────► duty(delete_due) (on close-out)
```

### Key design points

* **Privacy:** The phone holds the sensitive fact (`health:Diabetes`). Only a **neutral Insight** (metric/threshold + scope + expiry) reaches the scanner.
* **Scope & expiry:** Insight contains `retailer`, `device`, `event`, and `expiresAt`. The scanner enforces a **delete duty** at expiry.
* **Named insight IRI:** The Insight uses a stable IRI per session, not a blank node, so the policy can refer to it across files.
* **Typed requests & clean audit:** The runtime request is `req:Request`; audit rules record **only** typed requests (no policy internals).
* **Sign & verify:** The phone signs the canonicalized envelope; the scanner verifies hash + signature **before** any authorization.

---

## Rules overview (both demos)

* **Desensitize (phone):**
  `health:Diabetes` ⇒ `need:needsLowSugar true`.
* **Derive Insight (phone):**
  Create `ins:Insight` with `metric`, `threshold`, `scopeDevice`, `scopeEvent`, `retailer`, `expiresAt`.
* **Policy from Insight (phone):**
  Permit `odrl:use` with `purpose="shopping_assist"`; prohibit `share@marketing`; duty `delete@expiresAt`.
* **Enforce (scanner):**
  Join policy target ⇔ `ins:Insight`; allow/deny; mark expired by guard.
* **Audit (scanner):**
  Record `act:Decision` for typed requests, and `act:Duty "delete_due"` on close-out.
* **Shopping rule (scanner):**
  If scanned product’s sugar ≥ threshold, suggest a strictly lower-sugar alternative from `catalog.ttl`.

---

## Troubleshooting

* **`eye: command not found`** (Python split)
  Install EYE or set `EYE_CMD`:

  ```bash
  EYE_CMD=/usr/local/bin/eye python phone.py --session S1
  ```

* **Prefix errors (EYE)**
  Each rule/data chunk is **self-prefixed** and the runner places a chunk with `@prefix xsd:` first when typed literals are used. If you add your own chunks, ensure `@prefix xsd:` is defined where needed.

* **Empty `audit.ttl`**
  Usually caused by **blank-node insights** or a policy that targets a different node. This demo mints a **session IRI** and targets it consistently.

* **Signature/Hash mismatch (scanner)**
  Don’t edit `insight.ttl` or `policy.ttl` after signing. Re-run `phone.py` to regenerate a matching signature.

* **Browser demo button does nothing**
  Make sure you’re opening `demo.html` locally without CSP blockers; try a modern browser. The page loads Pyodide and eye-js from CDNs.

---

## Production notes (beyond the demo)

* Switch signing to **RDF Dataset Canonicalization (URDNA2015)** over **N-Quads**, then sign the canonical serialization (e.g., JWS).
* Pin/whitelist **public keys** by issuer (DID/domain) and/or use **mutual TLS** during pairing (QR/NFC + secure channel).
* Replace the file bus with a local transport (WebRTC/DIDComm) and add **hash-chained audit logs** for non-repudiation.
* Move catalogs to edge storage, but keep **policy enforcement local** to the scanner.

