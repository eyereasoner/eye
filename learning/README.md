# EYE learning

EYE learning transforms raw **Data** (e.g., *RDF*), **Rules** (e.g., *N3*), and a well-defined **Goal** into **actionable insights**.  
It leverages a **LLM** (e.g., *GPT-5 Thinking*) as a **meta-compiler** to automatically synthesize a **self-contained Python program**.  
This program not only produces an **Answer** but also explains the **Reason why** and performs an independent **Check (harness)** to ensure correctness.

---

## Conceptual overview

```
🟦 Data (RDF) ─┐
🟦 Rules (N3)  ├─▶ 🟧 LLM synthesizer ─▶ 🟩 Python code [Answer • Reason-why • Check] ─▶ 🟪 Actionable insight
🟦 Goal ───────┘                                                                                           |    
                                                             (optional) 🟨 EYE reasoner (proofs, scale) ◀──┘
```

---

## Running the EYE learning cases

Run the [eye learning cases](https://github.com/eyereasoner/eye/tree/master/learning/cases) to get the [answer, reason why and check (harness)](https://github.com/eyereasoner/eye/tree/master/learning/cases/output)
   ```bash
   ./test
   ```

