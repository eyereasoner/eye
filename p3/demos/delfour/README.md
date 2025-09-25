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
.
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

During a run, a working “bus” folder `data/bus/` is created for inter-program files.

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
# data/solid/profile.ttl
@prefix : <https://example.org/person#> .
@prefix health: <https://example.org/health#> .
:me  health:householdCondition  health:Diabetes .
```

2. Run the pipeline:

```bash
python insight_economy_pipeline.py
```

You’ll see:

* a **TIMELINE** of the session
* **ANSWER — INSIGHT** (publishable, neutral)
* **ANSWER — RUNTIME PREVIEW** (what the device shows)
* **REASON WHY** (private summary)
* **CHECKS** (verifications; should all PASS)

### Example output

```
=== TIMELINE ===
2025-09-25T19:26:08.665507+00:00  PICKUP            scanner="self-scanner" retailer="Delfour"
2025-09-25T19:26:08.697592+00:00  AGENT-DIALOG      capabilities received from Delfour
2025-09-25T19:26:08.829310+00:00  DESENSITIZE       need=needsLowSugar written
2025-09-25T19:26:08.916533+00:00  INSIGHT           neutral low-sugar envelope emitted
2025-09-25T19:26:09.031472+00:00  SCAN              product="Classic Tea Biscuits" → suggest="Low-Sugar Tea Biscuits"
2025-09-25T19:26:09.031574+00:00  RUNTIME           banner written
2025-09-25T19:26:09.051002+00:00  DROP              scanner returned; session closed

=== ANSWER — INSIGHT (to retailer) ===
@prefix ins: <https://example.org/insight#>.
@prefix xsd: <http://www.w3.org/2001/XMLSchema#>.

_:sk_0 a ins:Insight.
_:sk_0 ins:metric "sugar_g_per_serving".
_:sk_0 ins:threshold "10.0"^^xsd:decimal.
_:sk_0 ins:suggestionPolicy "lower_metric_first_higher_price_ok".
_:sk_0 ins:scopeDevice "self-scanner".
_:sk_0 ins:scopeEvent "pick_up_scanner".
_:sk_0 ins:retailer "Delfour".
_:sk_0 ins:expiresAt "2025-09-25T21:26:08.664591+00:00".


=== ANSWER — RUNTIME PREVIEW (device banner) ===
{
  "headline": "Track sugar per serving while you scan",
  "product_name": "Classic Tea Biscuits",
  "note": "High sugar",
  "suggested_alternative": "Low-Sugar Tea Biscuits"
}

=== REASON WHY (private) ===
Household requires low-sugar guidance (diabetes in POD). You are starting a self-scanner session at Delfour right now. Envelope is minimized and limited to this session.

=== CHECKS ===
PASS - insight_file_exists: bus/insight.ttl present
PASS - minimization_no_sensitive_terms: insight.ttl is neutral
PASS - scope_has_device_event_expiry: derived insight includes scope
PASS - runtime_banner_exists: bus/runtime_out.json present
PASS - behavior_suggests_on_high_sugar: suggestion was present
PASS - events_have_pickup_and_drop: start and end recorded
Overall: 6/6 checks passed.
```

---

## How it works (step by step)

Each step is a small program with minimal screen output and a log entry (`data/bus/timeline.log`).

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
python insight_economy_pipeline.py
```

Peek at the bus:

```bash
# created during the run
ls -1 data/bus
cat data/bus/need.ttl
cat data/bus/insight.ttl
cat data/bus/runtime_out.json
cat data/bus/timeline.log
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

## Conceptual mapping (privacy story)

* Sensitive POD fact (**private**): `:me health:householdCondition health:Diabetes .`
* Derived **need** (**shareable**): `:me need:needsLowSugar true .`
* Neutral **insight envelope** (**retailer-side**): metric/threshold/scope/expiry—no medical terms.
* **Behavior**: only during session & only when needed (scan event), suggest lower-sugar alternatives.

This separation lets retailers improve UX **without** handling medical data.

