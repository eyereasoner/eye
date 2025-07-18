# 🔢 Complex Numbers – Functional Reasoning in N3 Logic

This example defines and computes **complex number functions** using **Notation3 (N3)** and a math-compatible vocabulary for symbolic reasoning. The expressions implement advanced functions like **complex exponentiation**, **inverse trigonometric functions**, and **polar coordinate transformations** using step-by-step derivation.

Inspired by: [Wikipedia – Complex Numbers](https://en.wikipedia.org/wiki/Complex_number)

---

## 📘 Setup

### Prefixes

```turtle
@prefix xsd: <http://www.w3.org/2001/XMLSchema#>.
@prefix math: <http://www.w3.org/2000/10/swap/math#>.
@prefix complex: <http://eyereasoner.github.io/complex#>.
@prefix log: <http://www.w3.org/2000/10/swap/log#>.
@prefix var: <http://www.w3.org/2000/10/swap/var#>.
@prefix : <http://example.org/#>.
````

---

## 📐 Core Definitions

### 🧮 Complex Exponentiation

Implements:

> **(a + bi) ^ (c + di)**

Based on polar form and Euler's formula:

* Converts base to polar coordinates `(r, t)`
* Uses logarithmic identities and trigonometric components to compute the result
* Outputs a new complex number `(E, F)`

---

### 📈 Polar Coordinates

Computes:

> polar(x, y) = (r, θ)

Where:

* `r = sqrt(x² + y²)`
* `θ = arccos(|x| / r)`, adjusted by quadrant rules using `complex:dial`

---

### 🧠 Dial Rules

Computes angle adjustments (`θ`) based on quadrants:

* Handles 4 quadrant cases using signs of x and y
* Ensures correct angle `Tp` for trigonometric functions

---

### 📊 asin and acos for Complex Numbers

Both functions:

* Use algebraic identities
* Combine square roots, trigonometric inverses, exponentials
* Output a result as a complex number `(C, D)`

Each is defined as a separate rule block (`complex:asin`, `complex:acos`)

---

## ❓ Query

Evaluates the following:

```turtle
[ log:graph (
  [ log:triple (((-1 0) (0.5 0)) complex:exponentiation var:C1)]
  [ log:triple (((2.718... 0) (0 π)) complex:exponentiation var:C2)]
  [ log:triple (((0 1) (0 1)) complex:exponentiation var:C3)]
  [ log:triple (((2.718... 0) (-π/2 0)) complex:exponentiation var:C4)]
  [ log:triple ((2 0) complex:asin var:C5)]
  [ log:triple ((2 0) complex:acos var:C6)]
)] log:impliesAnswer [ ... ].
```

This triggers evaluation of:

| Expression             | Meaning                           |
| ---------------------- | --------------------------------- |
| `(-1 + 0i)^(0.5 + 0i)` | √(-1)                             |
| `e^(iπ)`               | Euler's identity                  |
| `i^i`                  | Pure imaginary to power of itself |
| `e^(-iπ/2)`            | Another Euler-based result        |
| `asin(2 + 0i)`         | Complex inverse sine              |
| `acos(2 + 0i)`         | Complex inverse cosine            |

The reasoner will compute the result complex numbers (`var:C1` to `var:C6`).

---

## ✅ Conclusion

This file encodes a powerful set of **mathematical definitions for complex numbers** using:

* **Structured logic**
* **Symbolic math operations**
* **Rule-based inference**

By chaining `math:` and `complex:` triples, the reasoner can evaluate and simplify complex arithmetic, including transcendental and trigonometric functions — a rare and advanced use case for N3 logic.

---

> 🧠 *This is an advanced demonstration of symbolic computation using rule-based logic frameworks.*

