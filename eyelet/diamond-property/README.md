# 🔷 Diamond Property Example (DPE) in EYE

This example verifies that the **diamond property** of a binary relation `:r` is preserved under its **reflexive closure** `:re`.
It is adapted from a formal proof environment and encoded in N3 logic for EYE.

The DPE checks whether if `a → b` and `a → c`, then there exists a common descendant `x` such that `b → x` and `c → x`. We test whether this holds under the closure `:re`.

---

## 🧱 Setup

```turtle
:a :re :b.
:a :re :c.
```

These are the starting facts: node `:a` has two `:re` successors `:b` and `:c`.

---

## ♻️ Reflexive Closure and Equality Axioms

Rules to relate `:re`, `:r`, and equality `:e`:

```turtle
# reflexivity of e
[ log:graph ([ log:triple (X :re Y) ]) ] log:implies [ log:graph ([ log:triple (X :e X) ]) ].
[ log:graph ([ log:triple (X :re Y) ]) ] log:implies [ log:graph ([ log:triple (Y :e Y) ]) ].

# symmetry of e
[ log:graph ([ log:triple (X :e Y) ]) ] log:implies [ log:graph ([ log:triple (Y :e X) ]) ].
```

---

## ❌ Negation & Disproof Support

Used to derive contradictions for indirect proof:

```turtle
[ log:graph ([ log:triple (X :not_re Z), (Y :re Z) ]) ] log:implies [ log:graph ([ log:triple (X :not_e Y) ]) ].
[ log:graph ([ log:triple (X :e Y), (X :not_re Z) ]) ] log:implies [ log:graph ([ log:triple (Y :not_e Z) ]) ].
```

Also includes rules for `:not_r` and `:not_e` symmetry.

---

## 🔄 re–r Interaction Rules

```turtle
[ log:graph ([ log:triple (X :e Y) ]) ] log:implies [ log:graph ([ log:triple (X :re Y) ]) ].
[ log:graph ([ log:triple (X :r Y) ]) ] log:implies [ log:graph ([ log:triple (X :re Y) ]) ].
[ log:graph ([ log:triple (X :re Y), (X :not_e Y) ]) ] log:implies [ log:graph ([ log:triple (X :r Y) ]) ].
[ log:graph ([ log:triple (X :re Y), (X :not_r Y) ]) ] log:implies [ log:graph ([ log:triple (X :e Y) ]) ].
```

---

## 🔷 Diamond Property (DP)

```turtle
[ log:graph (
    [ log:triple (X :r Y) ]
    [ log:triple (X :r Z) ]
)] log:implies [ log:graph (
    [ log:triple (Y :r U) ]
    [ log:triple (Z :r U) ]
)].
```

This defines the core DP over `:r`.

---

## ❓ Query with Assumed Negation

We assume `:b` and `:c` don’t share an `:re` successor and attempt to derive a contradiction.

```turtle
[ log:graph ([ log:triple (:b :re X) ]) ] log:implies [ log:graph ([ log:triple (:c :not_re X) ]) ].
[ log:graph ([ log:triple (:c :re X) ]) ] log:implies [ log:graph ([ log:triple (:b :not_re X) ]) ].
```

---

## ✅ Final Query

```turtle
[ log:graph (
    [ log:triple (:b :re X) ]
    [ log:triple (:c :re X) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple (:b :re X) ]
    [ log:triple (:c :re X) ]
)].
```

---

## ▶️ Running the Program

```bash
eye --quiet --nope diamond-property.ttl
```

Omit `--nope` to trace how the contradiction is discharged.

---

## 🧠 Summary

This example uses N3 logic to simulate indirect proof and verify preservation of the diamond property under reflexive closure. It demonstrates symmetry, negation, and equality within logical inference.

