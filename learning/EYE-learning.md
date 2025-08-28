# EYE learning

## Abstract

EYE learning is a pragmatic method for turning **Data** (e.g., RDF graphs), **Rules** (e.g., Notation3/N3), and a clearly scoped **Goal** into **actionable, verifiable insight**. The approach treats a large language model (LLM) as a *meta-compiler* that synthesizes a **single, self-contained Python program**. That program (1) computes an **Answer**, (2) explains the **Reason why**, and (3) executes an independent **Check (harness)** to validate the result and guard against errors or hallucinations. A public repository includes a large suite of worked cases and a one-command runner to reproduce the “answer / reason-why / check” triad end-to-end.

## Background and context

EYE (“Euler Yet another proof Engine”) is a high-performance N3 rule reasoner that supports forward and backward chaining along Euler paths. In the Semantic Web stack, EYE is used to derive new facts from data and rules, with explainable proofs and query-focused results. EYE learning complements this by using an LLM to *synthesize* the glue code and workflows that reach a specific goal—often producing runnable reasoning artifacts that engineers previously wrote by hand. In short: EYE remains the powerful reasoner; EYE learning accelerates getting to a working, testable path to your goal.

## What EYE learning is

At its core, EYE learning is a **goal-directed program synthesis pattern**:

1. You declare a **Goal** (a precise question or target entailment).
2. You provide **Data** (typically RDF) and **Rules** (typically N3).
3. You instruct the LLM to generate **one Python file** that:

   * ingests the data and rules,
   * computes the **Answer**,
   * prints a human-readable **Reason why** trace, and
   * runs a built-in **Check (harness)** to verify the result.

The emphasis is that the LLM’s output is **self-contained and self-checking**—an auditable artifact you can run in CI and share across teams.

## Why we call it “EYE learning”

We use “learning” in a precise, engineering-oriented sense: the system *learns how to reach the goal* by codifying a repeatable sequence of steps that produce, explain, and **verify** an answer. Instead of a one-off response, you receive an executable, portable procedure that captures what the LLM “learned” as a concrete, testable program. This turns a goal into **actionable insight**—not merely a narrative explanation—because the synthesized code can be rerun, audited, and evolved with your data and rules.

## What makes it different

**Self-contained, self-checking outputs.** The LLM’s deliverable is not a sketch or a notebook fragment; it’s a single Python script that brings its own harness. Each run yields a result plus an independent verification step, which materially improves trust in LLM-assisted reasoning.

**Bridge between symbolic and generative.** EYE learning uses the LLM to synthesize the orchestration logic while continuing to rely on N3 rules and—when appropriate—the EYE reasoner for semantics-aware performance and explainability. This hybrid lets teams move quickly without sacrificing formal grounding.

**Explainability by construction.** The generated program prints the **Answer** and the **Reason why**, aligning with EYE’s long-standing focus on proofs and derivations rather than black-box outputs.

**Goal-first engineering.** Because the Goal is fixed up front, the LLM “learns” a concrete procedure to reach it. The result is a durable artifact for CI/CD, compliance, and reproducible research.

## Evidence in practice

The repository contains **a large suite of cases** illustrating the pattern across varied scenarios, together with an `./test` runner that executes them end-to-end and emits the **answer / reason-why / check** outputs for each case. In our experience, these cases typically worked **first-time-right**, which suggests the approach is robust under realistic constraints.

> **Reproducibility:** Because the generated Python is self-contained, anyone can rerun a case from a clean environment and expect the same outputs (answer, explanation, and verification), which is essential for auditability and scientific method.

## Typical workflow

1. **Frame the goal.** Define the entailment or decision you want (e.g., “Given this RDF and these N3 rules, compute X and justify it.”).
2. **Assemble inputs.** Provide the relevant RDF graphs and N3 rule files.
3. **Synthesize.** Prompt the LLM to generate a **single Python script** that: loads inputs; computes the **Answer**; prints the **Reason why**; runs an internal **Check**.
4. **Execute and validate.** Run the script locally or in CI; record outputs and harness results. Use the repository’s `./test` command to execute the case suite consistently.
5. **Iterate and harden.** As rules and data evolve, update the inputs and regenerate the script, preserving the same acceptance criteria (answer, reason, check).
6. **(Optional) Integrate with EYE.** For larger or performance-sensitive scenarios, hand the reasoning step to the EYE engine to leverage forward/backward chaining and proof features, while retaining the LLM-synthesized harness for verification.

## Architecture at a glance

```
🟦 Data (RDF) ──┐
🟦 Rules (N3) ──┼─▶ 🟧 LLM synthesizer (goal-directed) ─▶ 🟩 Self-contained Python [Answer • Reason-why • Check] ─▶ 🟪 Actionable insight
🟦 Goal ────────┘                                                                                                   └─(optional)▶ ◻ EYE reasoner (proofs, scale)
```

The conceptual diagram shows a succinct pipeline: **Data + Rules + Goal** → **LLM synthesis** → **Self-contained Python (answer, reason-why, check)** → **Actionable insight**, with optional hand-off to EYE where formal proofs or scale demand it. This architecture makes two deliberate bets: (i) runtime **verification** is non-negotiable, and (ii) the **unit of work** is a portable script that travels well across tooling, teams, and environments.

## Relationship to EYE reasoning

EYE is a production-grade reasoner for N3, used to draw conclusions over RDF using forward and backward chaining along Euler paths. EYE learning is not a replacement: it’s a **front door** that accelerates getting to a validated solution while preserving a path to formal reasoning and proofs when needed. In practice, the LLM often emits **running reasoning code**—the sort of code we would otherwise craft by hand—so engineers can focus on domain rules and quality of evidence rather than boilerplate.

## Governance, trust, and assurance

* **Verification as a habit.** The built-in harness is designed to fail loudly when the computed answer doesn’t meet expectations, encouraging a test-first discipline for knowledge workflows.
* **Auditability.** The artifacts—inputs, generated script, and outputs—are versionable. You can re-run any case to verify claims or reproduce results, which supports audits, reviews, and regulatory needs.
* **Explainability.** The “reason-why” narrative provides human-readable context alongside machine-checked results, aligning with EYE’s emphasis on transparent derivations.

## Limitations and scope

EYE learning favors **well-formed goals** and **well-structured inputs**. Ambiguous objectives or under-specified rules may yield scripts that pass their checks but optimize for the wrong target. For complex domains, it’s good practice to (a) keep the goal narrowly defined, (b) include representative test data, and (c) add domain-specific checks to the harness. When formal proofs, performance, or very large datasets matter, integrate or migrate the reasoning core to EYE while retaining the same self-checking contract at the edges.

## Getting started

1. Read the short **README** to understand the pattern and the “answer / reason-why / check” outputs
2. inspect the **cases** folder to see many concrete examples and their expected outputs
3. run `./test` to reproduce results locally
4. adapt a case to your data and rules to create your first self-contained script. From there, you can wire the script into CI and, where appropriate, call into **EYE** for semantics-aware performance and proofs.

