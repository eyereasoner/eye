# LLM Generated Code to Emulate Thinking (LET)

**Abstract**
This project presents a novel approach to computational reasoning by using **Large Language Models (LLMs)**—specifically **Large Reasoning Models (LRMs)** such as *ChatGPT o3*—to translate **RDF data** and **N3 logic rules** into **self-contained Python programs**. The generated code frequently executes correctly **on the first attempt**, producing expected inferential outcomes without further debugging. Additionally, this method supports the automated production of **goal-directed reasoning traces** (proof-oriented explanations), bridging symbolic reasoning paradigms with data-driven generative models. This work demonstrates that LLMs can serve not only as text generators but also as **meta-compilers for executable reasoning processes**, emulating certain aspects of structured thought.

---

## Conceptual Overview (Suggested Diagram)

```
 ┌─────────────────┐     ┌─────────────────┐      ┌────────────────────┐
 │ RDF Data (Facts)│     │ N3 Logic Rules  │      │ Goal / Query       │
 └─────────┬───────┘     └───────┬─────────┘      └───────────┬────────┘
           │                     │                            │
           └───────────┐         │                            │
                       ▼         ▼                            │
                  ┌───────────────────────────────┐           │
                  │    LLM (e.g., ChatGPT o3)     │◄──────────┘
                  │  • Translates RDF + N3        │
                  │  • Synthesizes Python code    │
                  │  • Constructs proof strategy  │
                  └───────────┬───────────────────┘
                              ▼
                 ┌───────────────────────────────┐
                 │ Self-contained Python Program │
                 │ • Executes reasoning steps    │
                 │ • Produces expected results   │
                 │ • Outputs goal-oriented proof │
                 └───────────────────────────────┘
```

---

## Installation

1. Install [Python](https://www.python.org/downloads/).

2. Install the required Python packages:

   ```bash
   pip install numpy sympy
   ```

---

## Usage

1. Run the [brain](brain) module (branches of intelligence) to generate results and produce a [dump](brain/dump):

   ```bash
   ./test
   ```

