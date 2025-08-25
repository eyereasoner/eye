# logos

Logos turns raw data and knowledge into **actionable insights**.
By explicitly connecting **Data** (e.g., *RDF*), **Logic** (e.g., *N3*), and a **Goal**, logos uses an **LLM** (e.g., *GPT-5 Thinking*) as a **meta-compiler** to synthesize a **self-contained Python program** that produces an **Answer**, explains the **Reason why**, and runs an independent **Check (harness)**.
In doing so, it surfaces the **“why”** behind behaviors, trends, or outcomes—transforming data and knowledge into **clarity and direction**.

---

## Conceptual Overview

![Conceptual Overview](images/conceptual_overview.svg)

## Features

* **Actionable insights:** LLM emits runnable Python, not just a narrative.
* **Three artifacts by design:** `Answer`, `Reason why`, `Check (harness)`.
* **Separation of concerns:** declarative **Data + Logic + Goal** → imperative program.
* **Auditable & diffable:** code, not hidden activations; easy to review and rerun.
* **Extensible:** swap datasets, rule sets, goals, or models.

---

## Quickstart

Run the [eye logos examples and test cases](https://github.com/eyereasoner/eye/tree/master/logos/cases) to get the [answer, reason why and check (harness)](https://github.com/eyereasoner/eye/tree/master/logos/cases/output)
   ```bash
   ./test
   ```

