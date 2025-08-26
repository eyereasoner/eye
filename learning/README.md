# learning

EYE learning turns raw **Data**  (e.g., *RDF*), **Logic** (e.g., *N3*) and **Goal** into **actionable insight**.
It uses an **LLM** (e.g., *GPT-5 Thinking*) as a **meta-compiler** to synthesize a **self-contained Python program** that produces an **Answer**, explains the **Reason why**, and runs an independent **Check (harness)**.

---

## Conceptual Overview

![Conceptual Overview](images/conceptual_overview.svg)

## Features

* **Actionable insight:** LLM emits runnable Python, not just a narrative.
* **Three artifacts by design:** `Answer`, `Reason why`, `Check (harness)`.
* **Separation of concerns:** declarative **Data + Logic + Goal** → imperative program.
* **Auditable & diffable:** code, not hidden activations; easy to review and rerun.
* **Extensible:** swap datasets, rule sets, goals, or models.

---

## Quickstart

Run the [eye learning examples and test cases](https://github.com/eyereasoner/eye/tree/master/learning/cases) to get the [answer, reason why and check (harness)](https://github.com/eyereasoner/eye/tree/master/learning/cases/output)
   ```bash
   ./test
   ```

