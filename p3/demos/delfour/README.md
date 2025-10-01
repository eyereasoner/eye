# Delfour Insight Economy

A **fully client-side** prototype of the Delfour “insight economy” pipeline.
Reasoning runs in your browser via **eye-js** (EYE in WebAssembly); orchestration is written in **Python** running in **Pyodide**. No servers, no build.

---

## Quick start

1. Open [demo.html](https://potr-knows.github.io/Delfour-Insight-Economy/demo.html) in a modern browser (Chromium, Firefox, Safari).
2. Click **Run Demo**.
3. Watch the **Timeline**, **Insight**, **Runtime**, **Reason Why**, **Checks**, and **Audit** panels populate.

---

## What this demo shows

* **Desensitization:** POD fact (e.g., Diabetes) → neutral **need** (low sugar) via N3 rule.
* **Neutral INSIGHT envelope:** metric/threshold/scope/expiry with **no medical terms**.
* **ODRL policy derivation:** permission `use@shopping_assist`, prohibition `share@marketing`, duty `delete@expiresAt`.
* **Pure-rule authorization:** Given a request (`use@shopping_assist`) + policy + “now”, rules derive **Allowed/Blocked**; **expiry** is enforced.
* **Audit trail:** Rules emit `act:Decision` (Allowed/Blocked) and a close-out `act:Duty "delete_due"` when expired.
* **Runtime behavior:** If Allowed, a rule suggests a lower-sugar alternative for the scanned product.

---

## Architecture

### Components in the browser

* **eye-js** — evaluates N3 rules and RDF data; returns **derivations only** (like EYE `--pass-only-new`).
* **Pyodide** — runs a small Python orchestrator (the “programs”/pipeline) entirely in the page.
* **UI** — simple Tailwind UI rendering the current state.

### The “bus” (integration contract)

The original Python pipeline uses a file bus (`data/bus/`). In this browser demo, we mimic the same contract **in memory** as strings passed to `eye-js`. Conceptually, the bus “files” are:

| “Bus file” (concept) | Purpose                                                                       |
| -------------------- | ----------------------------------------------------------------------------- |
| `events.ttl`         | Human-readable timeline events (pickup, auth, scan, audit, drop).             |
| `insight.ttl`        | The **retailer-facing** neutral INSIGHT envelope.                             |
| `odrl.ttl`           | Policy derived from the INSIGHT (shown via Timeline; not physically written). |
| `now.ttl`            | `[] ex:now "…ISO…" .` — string time fact for rule comparisons.                |
| `request.ttl`        | `req:r1 odrl:action odrl:use; … purpose "shopping_assist".`                   |
| `scan.ttl`           | The simulated scanned product `[] shop:scannedProduct prod:… .`               |
| `runtime_out.json`   | Device banner JSON (headline, note, suggested alternative).                   |
| `audit.ttl`          | Derived `act:Decision` and close-out `act:Duty` entries.                      |

> In the browser we don’t write files; we compose these strings and feed them to `eye-js` runs. The UI shows their content (Insight, Audit, Banner).

### Pipeline (steps inside `demo.html`)

1. **PICKUP** — Build `ctx:` context (retailer, device, event, timestamp, **expiresAt**).
2. **AGENT-DIALOG** — Stub capability exchange.
3. **DESENSITIZE** — Rule: `health:Diabetes ⇒ need:needsLowSugar true.`
4. **DERIVE INSIGHT** — From need + context → `ins:Insight` (metric/threshold/scope/expiry).
5. **POLICY** — Derive ODRL from the Insight (permission/prohibition/duty).
6. **AUTHORIZATION BUNDLE** — Single `eye-js` run combining:

   * the **Insight**
   * the **policy-from-insight rule** (policy is derived inside the same run)
   * **now** (`ex:now`)
   * **request** (`use@shopping_assist`)
   * **expiry guard** (marks `ex:Expired` when `expiresAt < now`)
   * **enforcement rules** (Allowed/Blocked)
   * **audit rules** (emit `act:Decision`, and at close `act:Duty "delete_due"`)
7. **SHOPPING** — If Allowed, rule suggests an alternative with lower sugar (`math:` built-ins; typed decimals).
8. **DROP & CLOSE-OUT** — Re-evaluate expiry at the end and append a `delete_due` duty to the audit if applicable.

---

## Data & rules inside the page

* **Profile:** a tiny Turtle with `health:householdCondition health:Diabetes` (never shown to retailer).
* **Catalog:** a few `schema:Product` items with `schema:sugarContent` and `schema:price` typed as `xsd:decimal`.
* **Rules:**

  * `desensitize` → `need:needsLowSugar true`
  * `derive_insight` → neutral `ins:Insight` with `ins:expiresAt`
  * `policy_from_insight` → ODRL permission/prohibition/duty targeting the Insight
  * `expiry_guard` → mark `ex:Expired` if `expiresAt < now` (uses `math:lessThan`)
  * `odrl_enforce` → `ex:Allowed` / `ex:Blocked` from policy + request (+ expired)
  * `audit_rules` → `act:Decision` and close-out `act:Duty "delete_due"`
  * `shopping_rule` → when Allowed and scanned product’s sugar ≥ threshold, attach note and alternative

**Prefix map** used throughout:

```
ins:    https://example.org/insight#
ctx:    https://example.org/context#
need:   https://example.org/need#
health: https://example.org/health#
shop:   https://example.org/shop#
ex:     https://example.org/enforce#
act:    https://example.org/activity#
odrl:   http://www.w3.org/ns/odrl/2/
math:   http://www.w3.org/2000/10/swap/math#
str:    http://www.w3.org/2000/10/swap/string#
xsd:    http://www.w3.org/2001/XMLSchema#
schema: http://schema.org/
```

---

## Panels (what to expect)

* **Timeline** — `PICKUP`, `POLICY`, `AUTH (Allowed/Blocked)`, `EXPIRY` (if any), `SCAN`, `RUNTIME`, `AUDIT`, `DROP`.
* **Answer — Insight** — Minimal N3/Turtle (neutral; scope + expiry).
* **Answer — Runtime Preview** — Banner JSON (headline, product, note, suggestion).
* **Reason Why (private)** — Human explanation (never shown to retailer).
* **Checks** — Quick pass/fail sanity assertions (neutrality, scope, runtime present, suggestion present, audit decision present).
* **Audit (derived)** — `act:Decision` and (on close-out) `act:Duty "delete_due"` entries.

---

## Customize the demo

* **Change the scanned product**
  In the Python code string near the end, change:

  ```py
  scan_ttl = "... [] shop:scannedProduct prod:BIS_001 ."
  ```
* **Adjust the threshold**
  In `derive_insight`, set `ins:threshold` to another `xsd:decimal`.
* **Test expiry**
  In `build_context()`, change `expiresAt` to `now + timedelta(seconds=5)`; rerun after a few seconds to see `AUTH: Blocked (expired)` and a `delete_due` duty on close.
* **Policy semantics**
  Edit `policy_from_insight` to tweak actions (`odrl:use`, `odrl:share`), purposes, or duties.

---

## How to host it

* **Open from disk** — Works offline once loaded (Pyodide and eye-js are fetched on first load).
* **GitHub Pages** — Put `demo.html` anywhere inside your Pages-served branch; link to it directly.
* **Any static host** — No build step; the page loads everything via CDNs.

---

## Troubleshooting

* **Blank page** — Ensure network access for the Pyodide and eye-js CDNs; open DevTools Console for errors.
* **`illegal_token` parsing errors** — Usually a prefix typo (ensure `@prefix schema: <…> .` has a space) or SPARQL `FILTER` (not used here; we rely on `math:`/`str:` built-ins).

---

## Why neutrality matters (privacy)

Retailer sees **only** the neutral INSIGHT and its ODRL policy.
The “Reason Why” (diabetes → needs low sugar) remains private for the user/agent UI.
ODRL constrains purpose (`shopping_assist`), prohibits sharing for `marketing`, and requires deletion at expiry.

