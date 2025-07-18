# ⚛️ Heisenberg‑1D Example – Uncertainty Relations in N3 Logic

This mini–knowledge‑base shows how the Heisenberg uncertainty principle can be **mirrored** in pure [eyelet](https://github.com/eyereasoner/eye/tree/master/eyelet#readme) / N3 rules and queried with the [**EYE** reasoner](https://github.com/eyereasoner/eye).

We encode one‑electron data (ground harmonic‑oscillator ⊗ spin‑½ state) and let EYE classify the three uncertainty pairs:

| Pair | Operators                             | Bound | Result        |
| ---- | ------------------------------------- | ----- | ------------- |
| XP   | position **X** & momentum **P**       | ½     | **saturates** |
| SxSz | orthogonal spin components **Sx, Sz** | ¼     | **saturates** |
| XSz  | commuting observables **X, Sz**       | 0     | **satisfied** |

---

## 📂 Files

| File                         | Purpose                                                         |
| ---------------------------- | --------------------------------------------------------------- |
| `heisenberg.ttl`             | Facts + rules + answer‑rule – run this file only                |
| `heisenberg-answer.ttl`      | the answers that eye is giving                                  |
| `heisenberg-proof.ttl`       | the proof that eye is giving                                    |

---

## ▶️ Running the example

```bash
eye --quiet [--nope] heisenberg.ttl
```

EYE prints the answer graph:

```turtle
{ :psi  a :QuantumState.
        ...
        :xpStatus   "saturates";
        :sxszStatus "saturates";
        :xszStatus  "satisfied". }
```

If you edit one of the numeric facts (say lower `:deltaP`), re‑run EYE and watch the status flip to **violated** – the rules are purely algebraic.

---

## 📝 How it works

1. **Facts** record the standard deviations (ΔX, ΔP, ΔSx, ΔSz) and the Robertson bounds ½|⟨\[A,B]⟩| calculated in Python.
2. **Forward rules** compute each product ΔA·ΔB and store it as `:prod…`.
3. A small mapping `(:prodXP :boundXP) :statusPredicate :xpStatus.` lets generic rules discover which status‑property to assert.
4. **Classification rules** compare product vs. bound with an ε tolerance (1 × 10⁻¹²):

   * `violated`   → prod < bound
   * `saturates` → |prod − bound| < ε
   * `satisfied` → prod > bound
5. The **answer rule** returns every `:psi ?status ?label` triple.

Because X and P are in the ground state of the HO, ΔXΔP = ½ exactly, so the XP pair *saturates* the canonical limit.  Spin behaves analogously, while X and Sz commute so the lower bound is zero, merely *satisfied*.

---

## ✨ Try your own state

Replace the four `:delta…` numbers with values from another wave‑function (or a lab measurement) and EYE will instantly tell you whether each uncertainty relation holds, saturates, or is violated.

