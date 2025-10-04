# Delfour Insight Economy — N3/EYE Demos

Two equivalent demos of the Delfour case:

1. **CLI (bash + EYE)** — runs the phone → scanner pipeline on your machine.
2. **Browser (eye-js)** — runs the same N3 rules *entirely client-side* in your browser.

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

### Troubleshooting (CLI)

* `eye: command not found`
  Install EYE and ensure it’s on your `PATH`.

* `** ERROR ** ... no_prefix_directive(xsd,after_line(...))`
  The rule packs are **self-contained** (local prefixes) and tested with current EYE.
  If you edited files, verify each Turtle/N3 block begins with its `@prefix` lines.

* “No explicit outcome found” in the summary
  Check `now.ttl` vs `context.ttl` expiry; if `ex:now` is beyond `ins:expiresAt`, the request will be **Blocked** due to expiry.

---

## 2) Browser demo (eye-js)

### What it is

`demo.html` runs the **same** logic inside your browser via **eye-js** (EYE compiled to WASM). It shows the six input TTL “files”, the derived outputs for each phase, and a small JSON summary (decision + suggested alternative name).

### Run it

Use any static web server (so the browser can import the eye-js module):

```bash
# from the folder containing demo.html
npx http-server .   # or python3 -m http.server 8080
```

Open the printed URL (e.g., `http://127.0.0.1:8080/`) and click **Run Demo**.

### What you’ll see

* **Step 1 — PHONE inputs:** `profile.ttl`, `context.ttl`
* **Step 2 — PHONE derived:** `insight_policy.ttl`
* **Step 3 — SCANNER inputs:** `now.ttl`, `request.ttl`, `catalog.ttl`, `scan.ttl`
* **Step 4 — SCANNER derived:** `scanner_out.n3`
* **Step 5 — Summary:** decision (Allowed/Blocked), whether the insight is marked expired, and the friendly name of the suggested alternative (if any)

All reasoning happens locally in the page; no network calls are made beyond fetching the **eye-js** module.

### Troubleshooting (browser)

* **Blank outputs / import errors**
  Make sure you’re serving the file (not opening `file://`). Use a local server like `http-server` or `python -m http.server`.

* **“eye-js n3reasoner not available”**
  Your browser failed to load the dynamic import. Check the console; if you’re offline, you can vendor the module (ask if you want a self-hosted variant).

---

## How it maps (parity between CLI and browser)

| Phase              | CLI (EYE native)                                                                                              | Browser (eye-js)                                                     |
| ------------------ | ------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------- |
| PHONE derivation   | `eye --pass-only-new profile.ttl context.ttl phone.n3 > insight_policy.ttl`                                   | `n3reasoner([profile, context, PHONE_RULES])`                        |
| SCANNER derivation | `eye --pass-only-new insight_policy.ttl now.ttl request.ttl catalog.ttl scan.ttl scanner.n3 > scanner_out.n3` | `n3reasoner([phoneOut, now, request, catalog, scan, SCANNER_RULES])` |

