# Delfour Insight Economy — N3/EYE Demos

Two equivalent demos of the Delfour case:

1. **CLI (bash + EYE)** — runs the phone → scanner pipeline on your machine.
2. [**Browser (eye-js)** — runs the same N3 rules *entirely client-side* in your browser](https://eyereasoner.github.io/eye/reasoning/delfour/demo.html).

Both produce a **neutral insight + ODRL policy**, then **authorize** a use request and (when applicable) **suggest** a lower-sugar alternative. They also show an **audit decision** trail.

---

## Contents

```
.
├─ phone.n3                # PHONE-side rules: profile/context → neutral Insight + ODRL Policy
├─ scanner.n3              # SCANNER-side rules: expiry/auth/audit + shopping suggestion
├─ demo.html               # Browser demo that runs both rule packs with eye-js
├─ test                    # Bash script that runs the full CLI demo (phone → scanner)
└─ bus/                    # (created by the CLI script) outputs per session
   └─ S1/                  # example session folder
```

---

## 1) CLI demo (bash + EYE)

### Prerequisites

* **EYE** installed and available on your `PATH` (native build).
  Check: `eye --version`
* `bash` + either GNU `date` or Python 3 (the script falls back to Python for ISO timestamps).

### Run

```bash
# default session id: S1
./test

# custom session id:
./test S42
```

The script will:

1. Write six input graphs to `./bus/<SESSION>/`:

   * `profile.ttl` (POD data with a household condition)
   * `context.ttl` (retailer/device/event + created/expiry)
   * `now.ttl` (current time)
   * `request.ttl` (use@shopping_assist)
   * `catalog.ttl` (tiny product list)
   * `scan.ttl` (scanned product)

2. Run **phone.n3** to derive:

   * `insight_policy.ttl` — **neutral** `ins:Insight` + `odrl:Policy`

3. Run **scanner.n3** with the insight/policy + remaining inputs to derive:

   * `scanner_out.n3` — **authorization** marks, **audit** decisions/duties, and any **suggested alternative**

### Where to look

```
bus/<SESSION>/insight_policy.ttl   # Insight + Policy (derived)
bus/<SESSION>/scanner_out.n3       # Decision/Audit/Suggestion (derived)
```

Typical lines in `scanner_out.n3`:

```
_:a a <https://example.org/activity#Decision> ;
    <https://example.org/activity#outcome> "Allowed" .
[] <https://example.org/shop#suggestedAlternative> <https://example.org/product#BIS_101> .
```

If an alternative is found, it’s because the scanned item’s sugar is ≥ the threshold and there exists an item with a smaller `schema:sugarContent`.

---

## 2) Browser demo (eye-js)

### What it is

[demo.html](https://eyereasoner.github.io/eye/reasoning/delfour/demo.html) runs the **same** logic inside your browser via **eye-js** (EYE compiled to WASM). It shows the six input TTL “files”, the derived outputs for each phase, and a small JSON summary (decision + suggested alternative name).

### What you’ll see

* **Step 1 — PHONE inputs:** `profile.ttl`, `context.ttl`
* **Step 2 — PHONE derived:** `insight_policy.ttl`
* **Step 3 — SCANNER inputs:** `now.ttl`, `request.ttl`, `catalog.ttl`, `scan.ttl`
* **Step 4 — SCANNER derived:** `scanner_out.n3`
* **Step 5 — Summary:** decision (Allowed/Blocked), whether the insight is marked expired, and the friendly name of the suggested alternative (if any)

All reasoning happens locally in the page; no network calls are made beyond fetching the **eye-js** module.

