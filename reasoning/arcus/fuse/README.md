# ⚠️ Inference Fuse – Detecting Logical Inconsistencies in N3

This example demonstrates how **Notation3 (N3)** logic can be used to detect **contradictory facts** in a dataset, using an inference rule that **explodes** (returns `false`) when a specific inconsistency is encountered.

---

## 📘 Prefixes

```ttl
@prefix log: <http://www.w3.org/2000/10/swap/log#>.
@prefix var: <http://www.w3.org/2000/10/swap/var#>.
@prefix : <http://example.org/#>.
````

---

## 🧾 Facts

```ttl
:stone :color :black.
:stone :color :white.
```

The stone is said to be both **black** and **white**.

---

## 🔥 Inconsistency Rule ("Inference Fuse")

```ttl
[ log:graph (
  [ log:triple (var:X :color :black)]
  [ log:triple (var:X :color :white)]
)] log:implies false.
```

### Logical Interpretation

> If any object `X` is asserted to be both `:black` and `:white`,
> then a contradiction is derived (`false`).

This acts like a **fuse**: if triggered, it causes the reasoning process to fail or signal an **inconsistency** in the knowledge base.

---

## ✅ Result

In this case:

* `:stone :color :black`
* `:stone :color :white`
  ⟹ **Contradiction** is triggered.

The reasoner will derive `false`, indicating **logical inconsistency**.

---

## 🧠 Use Cases

This pattern is useful for:

* **Sanity checks** in data integration
* **Conflict detection** in ontologies
* Implementing **closed-world assumptions** in selected contexts

---

## ⚠️ Conclusion

This minimal example shows how N3 logic can be used for:

* Declaring and enforcing **mutual exclusivity**
* Implementing **logical validation**
* Flagging **inconsistent datasets** through contradiction

> 🧠 This is a key use of logic programming: **detecting what cannot be true**.

