# 🪲 Beetle–Nice Example – Disjunction Elimination in N3 Logic (eyelet)

This example shows **disjunction elimination** carried out by the [**EYE** reasoner](https://github.com/eyereasoner/eye) using the **eyelet** surface for N3 logic.  It is an extended variant of the classic “Beetle” puzzle: the only way to avoid contradiction is to conclude that **the Beetle is *nice***.

---

## 📘 Setup

### Prefixes

```ttl
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
@prefix log: <http://www.w3.org/2000/10/swap/log#>.
@prefix :    <urn:example:>.
```

---

## 🧾 Facts and Rules

### 1. Beetle is a car

```ttl
:beetle a :Car.
```

### 2. All cars are green **or** blue *(refutation rule)*

```ttl
(_:A) log:onNegativeSurface [ log:graph (
  [ log:triple (_:A rdf:type :Car) ]
  [ log:triple (() log:onNegativeSurface [ log:graph (
      [ log:triple (_:A :is :green) ]
  ) ]) ]
  [ log:triple (() log:onNegativeSurface [ log:graph (
      [ log:triple (_:A :is :blue) ]
  ) ]) ]
)].
```

> ∀A . Car(A) → green(A) ∨ blue(A)

---

### 3. Green things are **nice** or **pretty**

```ttl
(_:A) log:onNegativeSurface [ log:graph (
  [ log:triple (_:A :is :green) ]
  [ log:triple (() log:onNegativeSurface [ log:graph (
      [ log:triple (_:A :is :nice) ]
  ) ]) ]
  [ log:triple (() log:onNegativeSurface [ log:graph (
      [ log:triple (_:A :is :pretty) ]
  ) ]) ]
)].
```

> ∀A . green(A) → nice(A) ∨ pretty(A)

---

### 4. Pretty things are **beautiful**

```ttl
(_:A) log:onNegativeSurface [ log:graph (
  [ log:triple (_:A :is :pretty) ]
  [ log:triple (() log:onNegativeSurface [ log:graph (
      [ log:triple (_:A :is :beautiful) ]
  ) ]) ]
)].
```

> ∀A . pretty(A) → beautiful(A)

---

### 5. Cars are **not** beautiful *(explicit contradiction)*

```ttl
(_:A) log:onNegativeSurface [ log:graph (
  [ log:triple (_:A rdf:type :Car) ]
  [ log:triple (_:A :is :beautiful) ]
)].
```

> ¬∃A . Car(A) ∧ beautiful(A)

---

### 6. Blue things are **beautiful**

```ttl
(_:A) log:onNegativeSurface [ log:graph (
  [ log:triple (_:A :is :blue) ]
  [ log:triple (() log:onNegativeSurface [ log:graph (
      [ log:triple (_:A :is :beautiful) ]
  ) ]) ]
)].
```

> ∀A . blue(A) → beautiful(A)

---

## 🔍 Logical Inference

Given

* `:beetle rdf:type :Car.`
* Every car is **green ∨ blue**
* **Green ⇒ (nice ∨ pretty)**
* **Pretty ⇒ beautiful**, **Blue ⇒ beautiful**
* **Car ∧ beautiful** is impossible

we branch on the disjunctions:

| Branch                       | Consequence                      | Consistent?      |
| ---------------------------- | -------------------------------- | ---------------- |
| beetle is **blue**           | ⇒ beautiful → contradicts rule 5 | ❌ contradiction  |
| beetle is **green ∧ pretty** | ⇒ beautiful → contradicts rule 5 | ❌ contradiction  |
| beetle is **green ∧ nice**   | no clash                         | ✅ **consistent** |

Because every disjunct except the *green ∧ nice* case collapses into contradiction, disjunction elimination forces the conclusion:

> **:beetle \:is \:nice.**

---

## ❓ Query (posed as a refutation)

```ttl
() log:onNegativeSurface [ log:graph (
  [ log:triple (:beetle :is :nice) ]
  [ log:triple (() log:onNegativeAnswerSurface [ log:graph (
      [ log:triple (:beetle :is :nice) ]
  ) ]) ]
)].
```

This “negative‑answer surface” fails iff `:beetle :is :nice.` **is** provable, delivering the desired result.

---

## ✅ Conclusion

* **Beetle is a car**.
* **All cars are green or blue**.
* **Green ⇒ (nice ∨ pretty)** and **Pretty ⇒ beautiful**.
* **Blue ⇒ beautiful**.
* **Car ∧ beautiful** is forbidden.
* Therefore ⟹ **Beetle is nice**.

The eyelet representation allows EYE to perform **disjunction elimination** cleanly within negative‑surface reasoning.

---

## ▶️ Running the Example

```bash
# Run knowledge‑base + query in one go
eye beetle-nice.n3 --quiet
```

Expected output:

```n3
{ :beetle :is :nice. }
```

For KB/query separation:

```bash
eye --quiet --nope beetle6.ttl
```
