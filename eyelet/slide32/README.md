# 🏙️ Ghent Inference – Universal Quantification via Refutation in N3 Logic

This example uses **Notation3 (N3)** and **negative surface logic** to encode a **universal implication** (i.e., "all cities are human communities") via **refutation**. The reasoning is based on deriving a contradiction from the negation of the intended conclusion.

---

## 📘 Setup

### Prefixes

```n3
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
@prefix log: <http://www.w3.org/2000/10/swap/log#>.
@prefix : <http://example.org/#>.
````

---

## 🧾 Facts and Refutations

### 1. Ghent is a city

```n3
:Ghent a :City.
```

---

### 2. It is **impossible** that a city is not a human community

```n3
(_:S) log:onNegativeSurface [ log:graph (
  [ log:triple (_:S rdf:type :City)]
  [ log:triple (() log:onNegativeSurface [ log:graph (
      [ log:triple (_:S rdf:type :HumanCommunity)]
  )])]
)].
```

This means:

> It is **not possible** that some `S` is a city **and** not provably a human community.

Logical form (refutation of the negation):

> **¬∃S. City(S) ∧ ¬HumanCommunity(S)**
> ⟺ **∀S. City(S) → HumanCommunity(S)**

So:

> All cities are human communities.

---

## 🔍 Logical Inference

Given:

* Ghent is a city
* All cities are human communities

We conclude:

> **:Ghent rdf\:type \:HumanCommunity**

(by **modus ponens**)

---

## ❓ Query (to confirm entailment)

```n3
() log:onNegativeSurface [ log:graph (
  [ log:triple (:Ghent rdf:type :HumanCommunity)]
  [ log:triple (() log:onNegativeAnswerSurface [ log:graph (
      [ log:triple (:Ghent rdf:type :HumanCommunity)]
  )])]
)].
```

This queries:

> Is Ghent a human community **and** is it **not provable** that Ghent is a human community?

This creates a **contradiction** if the inference engine successfully proves that `:Ghent a :HumanCommunity`.

So, if the inference engine **rejects** this query, that confirms:

> Ghent *is* provably a human community.

---

## ✅ Conclusion

* A universal rule is encoded via **refutation**:

  > It’s **impossible** for a city to not be a human community.
* Ghent is declared a city.
* Therefore, Ghent must be a human community.
* A query attempts to produce a contradiction by assuming the statement is both true and not provable.
* The query fails, confirming that the system **correctly infers** the conclusion.

This is a textbook use of **negative surface logic** for **constructive reasoning via contradiction** in N3.
