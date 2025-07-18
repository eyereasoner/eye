# 🧠 Socrates Inference – Subclass Reasoning in N3 Logic

This example demonstrates how **class inheritance** (via `rdfs:subClassOf`) works in **Notation3 (N3)** logic using explicit reasoning rules. The goal is to infer that **Socrates is mortal**, given that humans are a subclass of mortals.

---

## 📘 Setup

### Prefixes

```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
@prefix log: <http://www.w3.org/2000/10/swap/log#>.
@prefix var: <http://www.w3.org/2000/10/swap/var#>.
@prefix : <http://example.org/#>.
````

---

## 🧾 Facts

### 1. Socrates is a human

```turtle
:Socrates a :Human.
```

### 2. Human is a subclass of Mortal

```turtle
:Human rdfs:subClassOf :Mortal.
```

---

## 📐 Subclass Rule (RDFS Inference Rule)

```turtle
[ log:graph (
  [ log:triple (var:A rdfs:subClassOf var:B)]
  [ log:triple (var:S rdf:type var:A)]
)] log:implies [ log:graph (
  [ log:triple (var:S rdf:type var:B)]
)].
```

This expresses a general rule:

> If `A ⊆ B` (i.e., A is a subclass of B)
> and `S ∈ A` (S is a member of A),
> then `S ∈ B` (S is also a member of B)

In this case:

* `:Human ⊆ :Mortal`
* `:Socrates ∈ :Human`
  ⟹ `:Socrates ∈ :Mortal`

---

## ❓ Query

```turtle
[ log:graph (
  [ log:triple (var:WHO rdf:type var:WHAT)]
)] log:impliesAnswer [ log:graph (
  [ log:triple (var:WHO rdf:type var:WHAT)]
)].
```

This is a generic query that asks:

> What type statements can be inferred?

It will match any `(WHO rdf:type WHAT)` triple that can be derived.

So, in this case, it should yield:

```turtle
:Socrates rdf:type :Mortal.
```

---

## ✅ Conclusion

* Socrates is declared a **Human**.
* All Humans are a **subclass of Mortal**.
* A rule is given to propagate class membership via subclassing.
* The system successfully infers:

  > **Socrates is Mortal**.
* The query then extracts this inferred fact from the reasoning engine.

This is a classic example of **RDFS-style inheritance** implemented through **N3 logic and inference rules**.

