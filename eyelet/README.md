# 🪡 eyelet - stitching logic together

A lightweight First-Order Logic (FOL) proof engine in [Notation3 (N3)](https://www.w3.org/TeamSubmission/n3/), using RDF-based inference rules. Originally authored by Jos De Roo.

---

## ✨ Features

- Inference via **resolution**, **factoring**, and **contraposition**
- Uses RDF/N3 for expressing logic rules and proofs
- Supports disjunction, negation, modus tollens, and more
- List-based pattern matching for rule manipulation
- Extensible with semantic web concepts like `log:`, `list:`, and `graph:`

---

## 🧠 Logical Framework

**eyelet** is built using custom reasoning rules represented in N3, categorized as:

- **Resolution Rules** (e.g., proof by cases, modus tollens)
- **Rewriting Rules** (e.g., factoring, contrapositive)
- **Terminal Rules** (e.g., contradiction inference fuse)

These rules enable the chaining of proofs and refutations through logical constructs in RDF.

---

## 📄 Syntax Overview

### Disjunction (Proof by Cases)

```n3
# All cars are either green or blue
{
    ?A a :Car.
} => ($ {
    ?A :is :green.
} {
    ?A :is :blue.
} $).
````

This encodes:

> ∀A. Car(A) → (green(A) ∨ blue(A))

### Negation (Contradiction)

```n3
# Nothing can be both a car and a horse
{
    ?A a :Car.
    ?A a :Horse.
} => ($ $).
```

This encodes:

> ∀A. (Car(A) ∧ Horse(A)) → ⊥

---

## 🔍 How It Works

eyelet rules are written in RDF/N3 using special predicates:

* `=>` : Logical implication
* `($ { ... } { ... } $)` : Disjunction
* `($ $)` : Contradiction / falsehood
* `list:select`, `list:removeDuplicates` : Pattern matching and normalization
* `graph:union` : Graph-based logical composition
* `log:call` : Procedural inference hook

The inference engine applies patterns like:

* **Modus Tollens**: From A → B and ¬B, infer ¬A
* **Factoring**: Eliminate redundancy in conclusions
* **Contrapositive**: Reformulate implications for backward chaining

---

## ✅ Example Use Cases

### 1. Contrapositive Reasoning

```n3
# If it's not green, then it's not a car (from earlier rule)
{
    ?A :is :red.
} => {
    ?A a :Car => ($ $).
}.
```

### 2. Modus Tollens

```n3
# If A implies B, but B leads to contradiction, then A is false
{
    ?A => ?B.
    ?B => ($ $).
    ?B list:isList false.
} => {
    ?A => ($ $).
}.
```

### 3. Case Elimination

```n3
# Given A implies (C or D), and C leads to false, then A implies D
{
    ?A => ?B.
    ?B list:select (?C ?D).
    ?C => ($ $).
} => {
    ?A => ?D.
}.
```

---

## 🧪 Testing & Extending

You can define your own rules using the same RDF-based logic syntax. Create proof graphs using `graph:list`, `list:select`, and `log:` built-ins to structure inference chains.

---

## 📚 References

* Jos De Roo, [Euler and N3 Logic](http://eulersharp.sourceforge.net/)
* [Notation3 Logic](https://www.w3.org/TeamSubmission/n3/)
* [W3C SWAP vocabulary](https://www.w3.org/2000/10/swap/)

---

## 🧩 License

MIT — feel free to build on and explore the logic.

