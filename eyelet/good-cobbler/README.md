# 👞 Good Cobbler – Functional Logic in N3

This example demonstrates **functional logic** in **Notation3 (N3)**, where a relation is represented as a structured term (e.g., someone *is* good at something). It’s based on a classical logical example used to introduce predicate logic with function-like terms.

> 📚 Reference:
> - Formal logic background: [Stanford Logic Chapter 11 – Functional Logic](http://intrologic.stanford.edu/chapters/chapter_11.html)
> - Source case: [HAL Archive: Good Cobbler Example](https://shs.hal.science/halshs-04148373/document)

---

## 📘 Prefixes

```n3
@prefix log: <http://www.w3.org/2000/10/swap/log#>.
@prefix var: <http://www.w3.org/2000/10/swap/var#>.
@prefix : <http://example.org/#>.
````

---

## 🧾 Fact

```n3
:joe :is (:good :Cobbler).
```

Joe **is good at being a Cobbler**.
This uses a **functional term** `(:good :Cobbler)` as the object of the `:is` relation.

---

## ❓ Query

```n3
[ log:graph (
  [ log:triple (var:X :is (:good var:Y))]
)] log:impliesAnswer [ log:graph (
  [ log:triple (var:X :is (:good var:Y))]
)].
```

This asks:

> Is there someone `X` who **is good at** something `Y`?

### Expected Answer:

```n3
:joe :is (:good :Cobbler).
```

---

## 🧠 Logical Interpretation

* The predicate `:is` is used with a **structured object** `(:good var:Y)`—a functional-style term.
* This encodes a relationship like `is(X, good(Y))` in predicate logic.
* The use of **nested structures** as terms allows fine-grained semantic distinctions (e.g., distinguishing between being good *as a person* vs. being good *at something*).

---

## ✅ Conclusion

This example illustrates:

* Use of **functional logic** in RDF/N3 via structured terms
* A simple **existential query** over structured relationships
* Expressivity of **Notation3** in modeling higher-order-like constructs

> 🧠 This is a useful illustration of structured terms in logic-based knowledge representation — especially in contexts like professional skill graphs, competency modeling, or linguistic knowledge bases.

