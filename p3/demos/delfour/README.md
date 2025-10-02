# Delfour Insight Economy — Demo

This [demo](https://eyereasoner.github.io/eye/p3/demos/delfour/demo.html) shows how a **privacy-preserving, policy-governed “insight”** can be derived from sensitive household data, used **only** for a scoped retail session, and then audited/expired — all with **logic rules** you can read and tweak.

You get two runtimes:

* **`demo.py`** — Console app that calls the local **EYE** reasoner (CLI).
* **`demo.html`** — Fully client-side browser app that runs the same Python orchestration in **Pyodide** and calls **eye-js** for reasoning.

Both produce the same sections:

* **Timeline** — high-level events (pickup, insight derivation, policy, auth, scan, close-out).
* **Answer — Insight (to retailer)** — the neutral “insight envelope” (as N3).
* **Answer — Runtime Preview (device)** — the banner JSON a shopper could see.
* **Reason Why (private)** — short operator-facing rationale.
* **Checks** — quick sanity ticks (minimization, scope, behavior, audit).
* **Audit (derived)** — decisions/duties derived by the rules (Allowed/Blocked, delete_due).

---

## What this demonstrates

* **Desensitization:** from a sensitive fact (`health:Diabetes`) to a neutral need (`need:needsLowSugar true`).
* **Scoped Insight:** the need + live context → an `ins:Insight` that is bound to **device**, **event**, **retailer**, and **expiry**.
* **Policy from Insight:** derive an **ODRL policy** that permits only `use@shopping_assist`, prohibits `share@marketing`, and imposes a **delete duty** at expiry.
* **Authorization & Audit:** evaluate a request under the policy + expiry guard, record decisions/duties.
* **Runtime Behavior:** when scanning a product, suggest a **lower-sugar** alternative if above threshold.

---

## Repo layout

```
demo.py        # Console version (EYE CLI)
demo.html      # Browser version (Pyodide + eye-js)
```

> All Turtle/N3 pieces are **self-prefixed** and heavily commented inside both files for readability.

---

## Quick start — Python (EYE CLI)

### Prerequisites

* Python 3.9+
* [EYE](https://github.com/eyereasoner/eye) installed and `eye` on your `PATH`

  * If `eye` is named differently or not on PATH, set `EYE_CMD`, e.g. `EYE_CMD=/usr/local/bin/eye`.

### Run

```bash
python demo.py
```

You’ll see the sections printed to the console. The **Audit** block contains the derived decisions/duties; the script removes `@prefix` lines for readability.

### Flags

The script invokes EYE with a **fixed, safe** set of flags:

```
--quiet --nope --pass-only-new
```

That means: don’t print input facts, only **new derivations**.

---

## Quick start — Browser (Pyodide + eye-js)

### How to run

* Open **`demo.html`** in a modern browser (Chrome/Edge/Firefox/Safari).
* Click **Run Demo**.

Everything runs **client-side**:

* Python orchestrates the pipeline in Pyodide.
* Reasoning is done by **eye-js** (the browser build of EYE).

> No servers, no data leaves your machine.

---

## How it works (logic pipeline)

1. **DESENSITIZE**

   * Rule: `health:householdCondition health:Diabetes` ⇒ `need:needsLowSugar true`.
   * Result is neutral (no medical term) → safe to share with the retailer.

2. **INSIGHT** (need + context → envelope)

   * Context records: `retailer=Delfour`, `device=self-scanner`, `event=pick_up_scanner`, `expiresAt`.
   * Derived `ins:Insight` carries: metric (`sugar_g_per_serving`), threshold (`10.0`), suggestion policy, device/event/retailer, expiry.

3. **POLICY** (ODRL from Insight)

   * Permit: `use` **only** for `purpose=shopping_assist` targeting the Insight.
   * Prohibit: `share` for `purpose=marketing`.
   * Duty: `delete` at `expiresAt`.

4. **AUTH** (+ **AUDIO/EXPIRY**)

   * Add a `now` fact; guard marks an Insight `ex:Expired` if `expiresAt < now`.
   * Enforcement rules map (policy + request) to **Allowed** or **Blocked**.
   * Audit rules emit `act:Decision` and `act:Duty`.

5. **SHOPPING** (behavior)

   * On scan, if product sugar ≥ threshold, annotate “High sugar” and propose any alternative with strictly lower sugar (price may be higher; policy allows it).

6. **CLOSE-OUT**

   * New `now` triggers a `delete_due` duty if the Insight is expired at close.

---

## Data & rules (included, self-prefixed)

* **Profile (POD)** — minimal, contains a sensitive health condition.
* **Desensitization rule** — maps to `need:needsLowSugar true`.
* **Context** — session info with ISO timestamps (`xsd:dateTime`).
* **Insight rule** — creates the envelope from need+context.
* **Policy rule** — derives ODRL permissions/prohibitions/duty.
* **Enforcement** — Allowed/Blocked mapping based on policy and request.
* **Expiry guard** — marks `ex:Expired` when `exp < now`.
* **Audit rules** — write decisions and duties.
* **Catalog** — tiny product set with sugar and price.
* **Shopping rule** — emits alternative suggestions when appropriate.

---

## Expected output (abbreviated)

* **Timeline** shows events like:

  ```
  …  PICKUP            scanner="self-scanner" retailer="Delfour"
  …  DESENSITIZE       need=needsLowSugar written
  …  INSIGHT           neutral low-sugar envelope emitted
  …  POLICY            ODRL policy emitted (use@shopping_assist; share@marketing prohibited)
  …  AUTH              Allowed (use@shopping_assist)
  …  SCAN              product="Classic Tea Biscuits" → suggest="Low-Sugar Tea Biscuits"
  …  RUNTIME           banner written
  …  DROP              scanner returned; session closed
  …  AUDIT             close-out appended (delete_due)
  ```

* **Answer — Insight** is a compact N3 block with the `ins:Insight` blank node.

* **Runtime Preview** resembles:

  ```json
  {
    "headline": "Track sugar per serving while you scan",
    "product_name": "Classic Tea Biscuits",
    "note": "High sugar",
    "suggested_alternative": "Low-Sugar Tea Biscuits"
  }
  ```

* **Checks**: all green when the pipeline succeeds.

* **Audit (derived)**: `act:Decision`(Allowed/Blocked) and optionally `act:Duty`(`"delete_due"`).

---

## Troubleshooting

* **`eye: command not found` (Python mode):**
  Install EYE or set `EYE_CMD` to its path:

  ```bash
  EYE_CMD=/path/to/eye python demo.py
  ```

* **`no_prefix_directive(xsd, …)`**
  Your EYE/eye-js build is strict about seeing `@prefix xsd:` **before** any typed literal.
  This demo keeps each piece **self-prefixed** and orders chunks so a block with `xsd:` appears first when needed. If you’ve modified the pieces, make sure:

  * Every chunk that uses `xsd:` (incl. `^^xsd:…`) declares `@prefix xsd: …`.
  * The first chunk in each run declares `@prefix xsd:` when typed literals appear.

* **Empty “Insight”**
  Check that `DESENSITIZE` emitted `need:needsLowSugar true` and that `build_context` creates a valid `expiresAt` in the future.

* **No suggestion at scan**
  The suggestion appears only if the scanned product’s sugar is **≥ threshold** and there exists a catalog item with **strictly lower** sugar.

---

## Design notes

* **Minimization by construction:** the **retailer-facing insight** contains only neutral terms (e.g., no “Diabetes” string). Checks assert this.
* **Rule transparency:** all rules are short N3/Turtle blocks you can read and edit.
* **Deterministic behavior:** both runtimes share the **same rules and data**, differing only in the reasoner binding (EYE CLI vs eye-js).
* **Client-side browser:** the HTML version uses Pyodide and eye-js in the page; no networking is required after load.

---

## Customize

* **Threshold:** change `ins:threshold "10.0"^^xsd:decimal`.
* **Scope:** modify `ctx:device`, `ctx:event`, `ctx:retailer`, time window.
* **Policy:** adjust ODRL permission/prohibition/duty to fit your governance.
* **Catalog:** add products; suggestion rule keeps working (lower sugar, any price).
* **Additional safeguards:** extend `ODRL_ENFORCE` with more constraints (e.g., location, role).

---

## FAQ

**Is this production code?**
It’s an educational demo that shows the *shape* of a privacy-preserving, policy-governed pipeline using explicit logic rules.

**Why not call an LLM for suggestions?**
We use **declarative rules** on structured facts so that behavior is **auditable**, **deterministic**, and easy to reason about.

**Does the browser version send data anywhere?**
No. After loading the page assets, all computation is client-side (Pyodide + eye-js).

