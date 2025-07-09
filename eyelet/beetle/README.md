# 🪲 Beetle Example – Disjunction Elimination in N3 Logic

This example demonstrates **disjunction elimination** using **Notation3 (N3)** and **negative surface logic**, inspired by the [Wikipedia article on Disjunction Elimination](https://en.wikipedia.org/wiki/Disjunction_elimination).

---

## 📘 Setup

### Prefixes

```n3
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
@prefix log: <http://www.w3.org/2000/10/swap/log#>.
@prefix : <http://example.org/#>.
````

---

## 🧾 Facts and Rules

### 1. Beetle is a car

```n3
:beetle a :Car.
```

### 2. All cars are green or blue

Expressed as a **refutation**:

```n3
(_:A) log:onNegativeSurface [ log:graph (
  [ log:triple (_:A rdf:type :Car)]
  [ log:triple (() log:onNegativeSurface [ log:graph (
      [ log:triple (_:A :is :green)]
  )])]
  [ log:triple (() log:onNegativeSurface [ log:graph (
      [ log:triple (_:A :is :blue)]
  )])]
)].
```

Logical form:

> ∀A. Car(A) → green(A) ∨ blue(A)

---

### 3. Green things are beautiful

```n3
(_:A) log:onNegativeSurface [ log:graph (
  [ log:triple (_:A :is :green)]
  [ log:triple (() log:onNegativeSurface [ log:graph (
      [ log:triple (_:A :is :beautiful)]
  )])]
)].
```

> ∀A. green(A) → beautiful(A)

---

### 4. Blue things are beautiful

```n3
(_:A) log:onNegativeSurface [ log:graph (
  [ log:triple (_:A :is :blue)]
  [ log:triple (() log:onNegativeSurface [ log:graph (
      [ log:triple (_:A :is :beautiful)]
  )])]
)].
```

> ∀A. blue(A) → beautiful(A)

---

## 🔍 Logical Inference

Given:

* \:beetle is a \:Car
* All cars are green or blue
* Both green and blue imply beauty

We conclude:

> **:beetle \:is \:beautiful**

This follows the classical rule:

> If A ∨ B, and A → C, and B → C, then C.

---

## ❓ Query (for contradiction)

```n3
(_:S) log:onNegativeSurface [ log:graph (
  [ log:triple (_:S :is :beautiful)]
  [ log:triple (() log:onNegativeAnswerSurface [ log:graph (
      [ log:triple (_:S :is :beautiful)]
  )])]
)].
```

This asks:

> Is there something that is beautiful but not provably beautiful?

This expression fails if beauty **is provable**, confirming the correctness of the inference.

---

## ✅ Conclusion

* **Beetle is a car**
* **All cars are green or blue**
* **Green or blue things are beautiful**
* ⟹ **Beetle is beautiful**

The N3 logic system correctly performs **disjunction elimination** and **derives the conclusion** through **negative surface reasoning**.

