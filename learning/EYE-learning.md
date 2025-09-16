# EYE Learning

## Abstract

EYE learning is a practical method for transforming **Data** (like RDF graphs), **Rules** (in Notation3/N3), and a specific **Goal** into actionable, verifiable insight. The approach uses a Large Language Model (LLM) as a *meta-compiler* to generate a **single, self-contained Python program**. This program is designed to be fully autonomous, delivering three key outputs:

1.  The final **Answer**.
2.  An explanation of the **Reason Why**.
3.  An independent **Check (harness)** that validates the result, guarding against errors and hallucinations.

A [public repository](https://github.com/eyereasoner/eye/tree/master/learning) provides a comprehensive suite of examples and a one-command runner to reproduce this "answer, reason, check" triad.

-----

## What EYE Learning Is

At its heart, EYE learning is a pattern for **goal-directed program synthesis**. The process is simple:

1.  You declare a precise **Goal** (e.g., a question to answer or a conclusion to reach).
2.  You provide the necessary **Data** (RDF) and **Rules** (N3).
3.  You instruct the LLM to generate **one Python file** that automatically ingests the inputs and produces the complete "answer, reason, check" output.

The key deliverable is a **self-contained and self-verifying program**. It's not just a code snippet; it's a trustworthy and auditable artifact you can run in a CI/CD pipeline and share with confidence.

-----

## What Makes It Different

EYE learning stands out by combining the flexibility of generative AI with the rigor of symbolic systems.

  * **Self-Contained, Self-Checking Outputs:** The LLM's output is a single, runnable Python program with its own built-in test harness. Every run produces both a result and a verification, which builds trust and reliability.

  * **Bridge Between Symbolic and Generative AI:** It uses the LLM for what it does best—synthesizing code and structure—while relying on formal N3 rules for logic and explainability. This hybrid approach lets teams innovate quickly without sacrificing rigor.

  * **Explainability by Design:** The generated program is explicitly required to explain its reasoning. This aligns with EYE's core principle of providing transparent derivations instead of black-box answers.

  * **Goal-First Engineering:** By starting with a clear goal, the LLM learns a concrete, repeatable procedure to achieve it. The resulting program is a durable asset perfect for automation, compliance, and reproducible research.

-----

## Architecture at a Glance

The conceptual pipeline is straightforward: **Data + Rules + Goal** are fed into an **LLM synthesizer**, which produces a **self-contained Python program**. This program delivers **actionable insight** by computing the answer, explaining the reasoning, and running a verification check.

```
┌───────────────────┐                           ┌──────────────────────┐
│  Data (e.g. RDF)  │                           │  self-contained      │
|         +         |   ┌───────────────────┐   |  Python program      |   ┌──────────────────────┐
│  Rules (e.g. N3)  │──>│  LLM synthesizer  │──>│  1. Answer           │──>│  actionable insight  │
|         +         |   └───────────────────┘   │  2. Reason why       │   └──────────────────────┘
│        Goal       │                           │  3. Check (harness)  │
└───────────────────┘                           └──────────────────────┘
```

For more demanding tasks involving complex logic or large datasets, the Python program can optionally hand off the core reasoning step to the high-performance **EYE reasoner** to generate formal proofs at scale. This architecture is built on two core principles: (1) runtime **verification is mandatory**, and (2) the primary output is a **portable program** that is easy to manage, version, and execute anywhere.

-----

## Advanced Pattern: Mixed Computation

For performance-critical applications, EYE learning supports an advanced pattern that separates stable logic from dynamic data. This "mixed-computation" approach, inspired by *Ershov, A. P. (1982). Mixed Computation: Potential Applications and Problems for Study. Theoretical Computer Science, 18, 41–67*, treats **stable policies** as static code and **live inputs** (like user signals) as dynamic data.

The LLM-guided synthesis step acts as a "specializer," converting the N3 rulebook into a compact, highly-efficient **Driver** function.

  * **At Runtime:** This specialized Driver consumes only the dynamic facts, applies the pre-compiled logic, and emits the standard **Answer**, **Reason Why**, and **Check**.
  * **Governance:** Policies remain in human-readable N3 rules and version-controlled RDF files. Updating a policy simply requires re-running the synthesis step to generate a new Driver—no algorithmic rewrite is needed.
  * **Benefits:** This approach preserves the core "answer, reason, check" contract while dramatically improving **speed, determinism, and auditability**. Logic stays declarative and clear; execution becomes small and fast.

-----

## Getting Started: A Typical Workflow

1.  **Define the Goal:** Clearly state the decision or conclusion you need to reach.
2.  **Assemble Inputs:** Gather the relevant RDF data and N3 rule files.
3.  **Synthesize the Program:** Prompt the LLM to generate the single Python program that produces the answer, reason, and check.
4.  **Execute and Validate:** Run the program to confirm the outputs and see the harness pass. The repository's `./test` command automates this for all cases.
5.  **Iterate and Harden:** As your data and rules evolve, simply regenerate the program to create an updated, validated artifact.
6.  **(Optional) Integrate with EYE:** For complex reasoning at scale, modify the program to call the EYE engine for its core logic while retaining the LLM-generated harness for verification.
