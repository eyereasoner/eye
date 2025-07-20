# ⚛️ Heisenberg Uncertainty – Eyelet Classification Demo

We encode one quantum state `:psi` (1D HO ground × spinor) with its
standard deviations:

* ΔX = ΔP = 0.7071067811865476
* ΔSx = ΔSz = 0.5

Robertson lower bounds:

| Pair  | Bound |
|-------|-------|
| X,P   | 0.5   |
| Sx,Sz | 0.25  |
| X,Sz  | 0.0   |

The KB *derives products*, *maps each (product,bound) pair to a status
predicate*, then *classifies* every pair as **saturates**, **satisfied**, or
**violated** (none violated here), and finally answers with the status triples.

---

## 🧾 Core facts (fragment)

```turtle
:psi a :QuantumState;
     :deltaX "0.7071067811865476"^^xsd:double;
     :deltaP "0.7071067811865476"^^xsd:double;
     :deltaSx "0.5"^^xsd:double;
     :deltaSz "0.5"^^xsd:double;
     :boundXP "0.5"^^xsd:double;
     :boundSxSz "0.25"^^xsd:double;
     :boundXSz "0.0"^^xsd:double.
````

---

## 🧮 Product derivation (one pattern)

```turtle
# ΔX·ΔP → :prodXP
[ log:graph (
    [ log:triple (:psi :deltaX var:dX) ]
    [ log:triple (:psi :deltaP var:dP) ]
    [ log:triple ((var:dX var:dP) math:product var:prod) ]
) ] log:implies [ log:graph (
    [ log:triple (:psi :prodXP var:prod) ]
) ].
```

(Analogous rules produce `:prodSxSz` and `:prodXSz`.)

---

## 🔗 Mapping products to status predicates

```turtle
(:prodXP   :boundXP)   :statusPredicate :xpStatus.
(:prodSxSz :boundSxSz) :statusPredicate :sxszStatus.
(:prodXSz  :boundXSz)  :statusPredicate :xszStatus.
```

---

## 🧪 Classification (excerpt)

```turtle
# “saturates” (|prod - bound| < 1e-12)
[ log:graph (
    [ log:triple ((var:prodPred var:boundPred) :statusPredicate var:statPred) ]
    [ log:triple (:psi var:prodPred var:prod) ]
    [ log:triple (:psi var:boundPred var:bound) ]
    [ log:triple ((var:prod var:bound) math:difference var:diff) ]
    [ log:triple (var:diff math:absoluteValue var:gap) ]
    [ log:triple (var:gap math:lessThan 1e-12) ]
) ] log:implies [ log:graph (
    [ log:triple (:psi var:statPred "saturates") ]
) ].
```

Two more similar rules tag `violated` or `satisfied` depending on the sign and
size of the difference.

---

## ❓ Answer rule

```turtle
[ log:graph ( [ log:triple (:psi var:statPred var:label) ] ) ]
  log:impliesAnswer
[ log:graph ( [ log:triple (:psi var:statPred var:label) ] ) ].
```

---

## ▶️ Run

```bash
eye --quiet --nope heisenberg.ttl
```

**Expected answer:**

```turtle
:psi :xpStatus "saturates";
       :sxszStatus "saturates";
       :xszStatus "satisfied".
```

Drop `--nope` for the proof.

---

## 🔍 Interpretation

* **X,P** ground state of HO saturates ΔX·ΔP = ½.
* **Sx,Sz** spinor chosen to saturate ΔSx·ΔSz = ¼.
* **X,Sz** commute ⇒ bound 0; product is positive ⇒ only *satisfied* (not 0).

---

## 🔧 Tweak ideas

* Change a variance (e.g. shrink `:deltaP`) to trigger a **violated** status.
* Add more states (`:psi2`, …) — the generic rules classify them automatically.

