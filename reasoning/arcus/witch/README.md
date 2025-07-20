# 🧙‍♀️ Witch Case – Chain Inference in N3 Logic (Monty Python Style)

This example humorously formalizes a chain of reasoning that leads to the conclusion that a girl is a **witch**, using **Notation3 (N3)** logic and rule-based inference. It mirrors the comically flawed logic from *Monty Python and the Holy Grail*, but is technically valid as a demonstration of inference chaining.

---

## 📘 Setup

### Prefixes

```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
@prefix log: <http://www.w3.org/2000/10/swap/log#>.
@prefix var: <http://www.w3.org/2000/10/swap/var#>.
@prefix : <http://example.org/#>.
````

---

## 🧾 Facts and Rules

### \[1] If someone **burns** and is a **woman**, they are a **witch**

```turtle
[ log:graph (
  [ log:triple (var:x rdf:type :BURNS)]
  [ log:triple (var:x rdf:type :WOMAN)]
)] log:implies [ log:graph (
  [ log:triple (var:x rdf:type :WITCH)]
)].
```

> ∀x. BURNS(x) ∧ WOMAN(x) → WITCH(x)

---

### \[2] GIRL is a woman

```turtle
:GIRL rdf:type :WOMAN.
```

---

### \[3] If something is made of wood, it burns

```turtle
[ log:graph (
  [ log:triple (var:x rdf:type :ISMADEOFWOOD)]
)] log:implies [ log:graph (
  [ log:triple (var:x rdf:type :BURNS)]
)].
```

> ∀x. ISMADEOFWOOD(x) → BURNS(x)

---

### \[4] If something floats, it is made of wood

```turtle
[ log:graph (
  [ log:triple (var:x rdf:type :FLOATS)]
)] log:implies [ log:graph (
  [ log:triple (var:x rdf:type :ISMADEOFWOOD)]
)].
```

> ∀x. FLOATS(x) → ISMADEOFWOOD(x)

---

### \[5] DUCK floats

```turtle
:DUCK rdf:type :FLOATS.
```

---

### \[6] If `x` floats and is the **same weight** as `y`, then `y` floats too

```turtle
[ log:graph (
  [ log:triple (var:x rdf:type :FLOATS)]
  [ log:triple (var:x :SAMEWEIGHT var:y)]
)] log:implies [ log:graph (
  [ log:triple (var:y rdf:type :FLOATS)]
)].
```

---

### \[7] DUCK and GIRL have the same weight

```turtle
:DUCK :SAMEWEIGHT :GIRL.
```

---

## 🔍 Inference Chain

Through chaining the rules, we derive:

1. DUCK floats ⇒ DUCK is made of wood
2. DUCK same weight as GIRL ⇒ GIRL floats
3. GIRL floats ⇒ GIRL is made of wood
4. GIRL is made of wood ⇒ GIRL burns
5. GIRL burns ∧ GIRL is a woman ⇒ GIRL is a witch

✅ Final inference:

```turtle
:GIRL rdf:type :WITCH.
```

---

## ❓ Query

```turtle
[ log:graph (
  [ log:triple (var:S rdf:type :WITCH)]
)] log:impliesAnswer [ log:graph (
  [ log:triple (var:S rdf:type :WITCH)]
)].
```

This asks:

> Can we infer that someone is a witch?

The reasoner will return:

```turtle
:GIRL rdf:type :WITCH.
```

---

## ✅ Conclusion

* Using a chain of logical inferences, it is deduced that **GIRL is a WITCH**.
* Each step is valid under the given rules, even though the overall reasoning is obviously satirical.
* This case serves as an entertaining and instructive demonstration of **multi-step inference**, **rule chaining**, and **type-based reasoning** in N3 logic.

> ⚠️ *Note: This is a parody example, not a valid ethical or scientific inference!*

