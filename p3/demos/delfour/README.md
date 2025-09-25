# Delfour Insight Economy — README

A tiny, reproducible demo of **privacy-preserving retail insights** built as a **multi-program pipeline** that talks via RDF/N3 files and uses the **EYE reasoner**. It follows the P3 pattern (**Answer / Reason / Check**) and simulates a self-scanner shopping session that derives a **neutral “low-sugar” insight** from sensitive POD data—without leaking medical terms.

---

## What this shows

* **Multiple programs** (shellable Python scripts) that each do one job and **communicate via files**.
* **EYE** runs N3 rules to:

  * de-sensitize private health data → `need:needsLowSugar true`
  * derive a neutral **insight envelope** scoped to device/event/time
  * suggest a lower-sugar alternative at scan time
* **Data minimization** by emitting only new, non-sensitive triples with `--pass-only-new`.
* **P3 output**: Answer (publishable), Reason (private), Check (verifications).
* A **timeline** of what happened during the session.

---

## Directory layout

```
delfour/
├── data
│   ├── retailer
│   │   ├── catalog.ttl                  # sample products
│   │   └── delfour_capabilities.ttl     # public device/feature facts
│   └── solid
│       └── profile.ttl                  # private POD data (sensitive)
├── insight_economy_pipeline.py          # orchestrator (P3: answer/reason/check + timeline)
├── programs
│   ├── _log.py
│   ├── 01_pickup_scanner.py
│   ├── 02_capability_dialog.py
│   ├── 03_desensitize_private.py
│   ├── 04_derive_insight.py
│   ├── 05_shopping_runtime.py
│   └── 06_drop_scanner.py
└── rules
    ├── derive_insight_from_need_and_context.n3
    ├── desensitize_diabetes_to_need_low_sugar.n3
    └── shopping_suggest_alternative.n3
```

During a run, a working “bus” folder `delfour/data/bus/` is created for inter-program files.

---

## Prerequisites

* **Python** 3.8+ (tested with 3.10+)
* **EYE reasoner** on your `PATH` (command `eye`)

  * If not globally installed, set an env var: `export EYE=/path/to/eye`
* POSIX shell recommended (macOS/Linux/WSL). Windows works via WSL/PowerShell with Python.

---

## Quick start

1. (Optional) Inspect/edit the **private** POD file (sensitive term appears here only):

```turtle
# delfour/data/solid/profile.ttl
@prefix : <https://example.org/person#> .
@prefix health: <https://example.org/health#> .
:me  health:householdCondition  health:Diabetes .
```

2. Run the pipeline:

```bash
python delfour/insight_economy_pipeline.py
```

You’ll see:

* a **TIMELINE** of the session
* **ANSWER — INSIGHT** (publishable, neutral)
* **ANSWER — RUNTIME PREVIEW** (what the device shows)
* **REASON WHY** (private summary)
* **CHECKS** (verifications; should all PASS)

### Example output (abridged)

```
=== TIMELINE ===
2025-09-25T13:23:05Z  PICKUP           scanner="self-scanner" retailer="Delfour"
2025-09-25T13:23:05Z  AGENT-DIALOG     capabilities received from Delfour
2025-09-25T13:23:05Z  DESENSITIZE      need=needsLowSugar written
2025-09-25T13:23:05Z  INSIGHT          neutral low-sugar envelope emitted
2025-09-25T13:23:05Z  SCAN             product="Classic Tea Biscuits" → suggest="Low-Sugar Tea Biscuits"
2025-09-25T13:23:05Z  RUNTIME          banner written
2025-09-25T13:23:05Z  DROP             scanner returned; session closed

=== ANSWER — INSIGHT (to retailer) ===
@prefix ins: <https://example.org/insight#>.
@prefix xsd: <http://www.w3.org/2001/XMLSchema#>.
_:sk_0 a ins:Insight ;
      ins:metric "sugar_g_per_serving" ;
      ins:threshold "10.0"^^xsd:decimal ;
      ins:suggestionPolicy "lower_metric_first_higher_price_ok" ;
      ins:scopeDevice "self-scanner" ;
      ins:scopeEvent "pick_up_scanner" ;
      ins:retailer "Delfour" ;
      ins:expiresAt "2025-09-25T15:23:05Z" .

=== ANSWER — RUNTIME PREVIEW (device banner) ===
{
  "headline": "Track sugar per serving while you scan",
  "product_name": "Classic Tea Biscuits",
  "note": "High sugar",
  "suggested_alternative": "Low-Sugar Tea Biscuits"
}

=== CHECKS ===
PASS ... (6/6 checks passed)
```

---

## How it works (step by step)

Each step is a small program with minimal screen output and a log entry (`delfour/data/bus/timeline.log`).

1. **Pick up scanner** → writes session context (`context.ttl`) and event.
2. **Agent dialog** → copies public `delfour_capabilities.ttl` into bus.
3. **Desensitize** (EYE + `desensitize_diabetes_to_need_low_sugar.n3`)

   * **Input (private)**: `solid/profile.ttl` (mentions “Diabetes”).
   * **Output (bus)**: `need.ttl` → `need:needsLowSugar true`.
   * **Flag**: `--pass-only-new` prevents copying sensitive input to bus.
4. **Derive neutral insight** (EYE + `derive_insight_from_need_and_context.n3`)

   * Combines `need.ttl` + `context.ttl` → **insight envelope** in `insight.ttl`.
   * Envelope has **no medical terms**, scoped to device/event/expiry.
5. **Shopping runtime** (EYE + `shopping_suggest_alternative.n3`)

   * Simulates scanning a product; joins `catalog.ttl`.
   * If sugar ≥ threshold, suggests a lower-sugar alternative and emits a **banner** JSON.
6. **Drop scanner** → logs the end event.

The **P3 orchestrator** prints:

* **Answer**: the public insight + the device banner
* **Reason**: a short private explanation
* **Check**: automated verifications (minimization, scope, behavior, lifecycle)

---

## Important flags (EYE)

We deliberately use:

* `--nope` — no proof output
* `--pass-only-new` — **emit only newly derived facts**, never inputs
  (critical for keeping the bus free of sensitive data)

You’ll see this in:

* `03_desensitize_private.py`
* `04_derive_insight.py`
* `05_shopping_runtime.py`

---

## Reproducing & inspecting

Run:

```bash
python delfour/insight_economy_pipeline.py
```

Peek at the bus:

```bash
# created during the run
ls -1 delfour/data/bus
cat delfour/data/bus/need.ttl
cat delfour/data/bus/insight.ttl
cat delfour/data/bus/runtime_out.json
cat delfour/data/bus/timeline.log
```

Re-run safely any time; the timeline is reset at start.

---

## Customizing the demo

* **Change the scanned product**: edit `programs/05_shopping_runtime.py` → `scanned_qname = "prod:BIS_001"`.
* **Tune the threshold/policy**: adjust `rules/derive_insight_from_need_and_context.n3`.
* **Expand the dialog**: replace the simple copy in `02_capability_dialog.py` with a handshake or capability negotiation.
* **Add expiry enforcement**: in `05_shopping_runtime.py`, refuse to use an insight if `ins:expiresAt` is in the past.
* **Add programs** (e.g., consent, receipt, payment) and thread them via the bus.

---

## Troubleshooting

* **`eye: command not found`**
  Install EYE or set `EYE` env var:
  `export EYE=/absolute/path/to/eye`
  (The scripts respect `os.environ["EYE"]`.)
* **Checks fail: “minimization_no_sensitive_terms”**
  Ensure steps 03/04/05 use `--pass-only-new`. If you see “Diabetes” in `insight.ttl`, a step echoed inputs.
* **No suggestion appears**
  Verify `rules/shopping_suggest_alternative.n3` declares `@prefix xsd:` and catalog sugar values are strings parsable as decimals.
* **Regex error in 05_shopping_runtime.py**
  Make sure the `name_for()` helper uses the fixed pattern with `re.escape(qname)` and `re.DOTALL`.

---

## Conceptual mapping (privacy story)

* Sensitive POD fact (**private**): `:me health:householdCondition health:Diabetes .`
* Derived **need** (**shareable**): `:me need:needsLowSugar true .`
* Neutral **insight envelope** (**retailer-side**): metric/threshold/scope/expiry—no medical terms.
* **Behavior**: only during session & only when needed (scan event), suggest lower-sugar alternatives.

This separation lets retailers improve UX **without** handling medical data.

---

## License / attribution

This demo is designed to slot alongside the EYE/P3 examples. Adapt as needed for your own experiments and add a license file appropriate to your project.

