# EYE learning

EYE learning transforms raw **Data** (e.g., *RDF*), **Logic** (e.g., *N3*), and a defined **Goal** into **actionable insights**.  
It leverages a **LLM** (e.g., *GPT-5 Thinking*) as a **meta-compiler** to automatically synthesize a **self-contained Python program**.  
This program not only produces an **Answer** but also explains the **Reason why** and performs an independent **Check (harness)** to ensure correctness.

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

