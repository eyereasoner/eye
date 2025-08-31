# EYE Mixed Computation: from RDF/N3 to Specialized Drivers

```text
  ME (user) ---------------------+ 
                                 |        +-------------------+
                                 |        |   Behaviors       |   (rules / N3,
                                 |        |   (rulebooks)     |    policy templates)
                                 |        +-------------------+
                                 |                 ^     ^
                                 |                 |     |
                                 v                 |     |  context (dynamic)
+----------------+    AC     +----------+          |     |
| data (RDF)     | <-------> | context  |----------+     |
| facts, defaults|  (access) +----------+                |
+----------------+                                       v
                           Agent (partial evaluator / codegen)
                                      |
                                      v
                           Driver (specialized decision fn)
                                      |
                                      v
            +--------------------------------------------------+
            | Targets / Capabilities / Context (candidates)    |  → ranked options
            +--------------------------------------------------+
                                      |
                                      v
                           actionable insight + feedback
```

Aligned with the [EYE learning guide](https://github.com/eyereasoner/eye/blob/master/learning/EYE-learning.md), the pattern treats **facts** as RDF graphs and **behaviors** as N3 rule sets, with *ME* providing preferences or intent. In the schematic, **data (RDF)** and **context (RDF)** sit on the left, **behaviors (N3)** above, and the **AC** link denotes access/entailment—optionally running EYE to materialize derived triples. An **Agent** then consumes these three inputs and emits a small, executable **Driver** (a decision/scoring function). This keeps the rulebook declarative and auditable while producing a concise procedure that turns an enriched graph into **actionable insight + feedback**.

Ershov’s mixed computation gives the operational split. We mark policy constants, ontological maps, and stable rules as **static**; we treat user prefs, live signals, and candidates as **dynamic**. The Agent **partially evaluates** the behaviors with the static knowledge to **specialize** a Driver (typically via closures). At run time the Driver needs only dynamic facts, so the loop is simple, fast, and testable. If desired, an EYE step precedes specialization: run rules over data/context to *materialize* closures, contraindications, or bonuses, then specialize against that entailed graph—cleanly separating reasoning (EYE) from execution (Driver).

Governance and traceability follow naturally. Policies live as **N3 rules** and weights in RDF, so changes are human-reviewable and versioned; re-specializing the Driver applies them without algorithm rewrites. The Driver emits **explanation traces** (why an option ranked higher, which bonuses/penalties applied), and lightweight **self-checks** guard invariants. Because the schematic is stable—*ME → data/context/behaviors → Agent → Driver → targets/capabilities → insight*—you can move across domains, swap in richer rulebooks, or tighten assurance by recording EYE proofs alongside the Driver’s traces, all without entangling policy with code.

We mirror the [EYE learning guide](https://github.com/eyereasoner/eye/blob/master/learning/EYE-learning.md)’s contract by synthesizing a **single, self-contained Python program** that (1) computes an **Answer**, (2) prints a **Reason why**, and (3) runs an internal **Check (harness)**—the “answer / reason-why / check” triad. Inputs are provided as **Data (RDF)** and **Rules (N3)**, optionally preceded by an EYE reasoning pass; the **Agent** plays the role of the “LLM synthesizer” that produces the portable script; and the **Driver** is the run-time core that turns the (possibly entailed) graph into actionable results. This directly matches the guide’s “Data + Rules + Goal → LLM synthesizer → Python \[Answer • Reason-why • Check] → Actionable insight,” with optional hand-off to the EYE reasoner for proofs and scale.

Some concrete cases:
* **[Bike-trip planner](https://github.com/eyereasoner/eye/blob/master/learning/cases/biketrip_mixed.py):** Static RDF captured default policy weights and infrastructure/traffic maps, dynamic RDF held weather/closures/routes, and the specialized driver ranked candidate routes with an explanation trace.
* **[Clinical demo (educational, not medical advice)](https://github.com/eyereasoner/eye/blob/master/learning/cases/clinicsupport_mixed.py):** Static RDF/N3 documented thresholds and policy weights while dynamic RDF provided patient vitals/comorbidities; the specialized driver prioritized generic interventions with contraindication penalties and emitted rationale.
* **[Beer advisor](https://github.com/eyereasoner/eye/blob/master/learning/cases/beeradvisor_mixed.py):** Static RDF defined style trait vectors and policy weights, dynamic RDF expressed your taste/constraints plus context (weather/meal), and the specialized driver scored and ranked beers—rejecting those that violated constraints—along with a clear trace.
* **[EV/home energy scheduler](https://github.com/eyereasoner/eye/blob/master/learning/cases/evscheduler_mixed.py):** Static RDF captured device specs, peak limits, and policy weights (with N3 rule intent mirroring constraints), dynamic RDF supplied 96×15-min price/carbon/base-load signals plus SoC/deadlines, and the specialized driver scheduled EV charging and a dishwasher to meet targets under the peak cap with a cost/CO₂-aware explanation trace.

