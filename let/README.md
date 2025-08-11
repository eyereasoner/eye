# LLM Generated Code to Emulate Thinking (LET)

**Abstract**  
This research presents a novel approach to computational reasoning by using **Large Language Models (LLMs)** such as *GPT 5 Thinking*—to translate **RDF data** and **N3 logic rules** into **self-contained Python programs**. The generated code frequently executes correctly **on the first attempt**, producing expected inferential outcomes without further debugging. Additionally, this method supports the automated production of **goal-directed reasoning traces** (proof-oriented explanations), bridging symbolic reasoning paradigms with data-driven generative models. This work demonstrates that LLMs can serve not only as text generators but also as **meta-compilers for executable reasoning processes**, emulating certain aspects of structured thought.

---

## Conceptual Overview
```
┌───────────────────────┐    ┌───────────────────────┐    ┌───────────────────────┐
│    RDF Data (Facts)   │    │     N3 Logic Rules    │    │       Goal / Query    │
└───────────┬───────────┘    └───────────┬───────────┘    └───────────┬───────────┘
            │                            │                            │
            └─────────────┐              │              ┌─────────────┘
                          ▼              ▼              ▼
                 ┌──────────────────────────────────────────────┐
                 │           LLM (e.g., GPT 5 Thinking)         │
                 │   • Translates RDF + N3                      │
                 │   • Synthesizes Python code                  │
                 │   • Constructs proof strategy                │
                 └───────────────────────┬──────────────────────┘
                                         ▼
                 ┌──────────────────────────────────────────────┐
                 │        Self-contained Python Program         │
                 │   • Executes reasoning steps                 │
                 │   • Produces expected results                │
                 │   • Outputs goal-oriented proof              │
                 └──────────────────────────────────────────────┘
```
---

## Usage

1. Run [brain](brain) branches of intelligence to get [arc](brain/arc) answer, reason why and check (harness):

   ```bash
   ./test
   ```

