# 📐 Pythagoras – Eyelet Rule Demo

A tiny eyelet (N3) encoding of the abstract Pythagorean pattern:

> **Square(A,A²) ∧ Square(B,B²) ∧ Square(C,C²) ∧ Add(A²,B²,C²) ⇒ Pythagoras(A,B,C)**

We give one symbolic triangle `(a,b,c)`, assert its three squares and their sum, and ask EYE to confirm the relation.

---

## 🧠 Core rule (eyelet fragment)

```turtle
# Squares + addition ⇒ Pythagoras
[ log:graph (
    [ log:triple (var:A :Square var:A2) ]
    [ log:triple (var:B :Square var:B2) ]
    [ log:triple (var:C :Square var:C2) ]
    [ log:triple ((var:A2 var:B2) :Add var:C2) ]
) ] log:implies [ log:graph (
    [ log:triple ((var:A var:B var:C) :Pythagoras true) ]
) ].
````

---

## 🧾 Facts (symbolic triangle)

```turtle
:a :Square :a2 .
:b :Square :b2 .
:c :Square :c2 .
(:a2 :b2) :Add :c2 .
```

---

## ❓ Query

```turtle
[ log:graph ( [ log:triple ((:a :b :c) :Pythagoras true) ] ) ]
  log:impliesAnswer
[ log:graph ( [ log:triple ((:a :b :c) :Pythagoras true) ] ) ].
```

---

## ▶️ Run

```bash
eye --quiet --nope pythagoras.ttl
```

Expected answer:

```turtle
(:a :b :c) :Pythagoras true.
```

Drop `--nope` for the proof.

---

## 🔍 What happens

1. The rule matches the four premises (`Square` facts + one `Add`).
2. It asserts `:Pythagoras true` for the triple.
3. The answer rule returns that triple.

---

## 🧪 Optional numeric variant

Replace the symbolic squares:

```turtle
:a :Square 9 .
:b :Square 16 .
:c :Square 25 .
(9 16) :Add 25 .
```

You still get `(:a :b :c) :Pythagoras true`.

---

## 💡 Possible extensions

* Replace custom `:Add` with built‑in arithmetic: `((var:A2 var:B2) math:sum var:C2)`.
* Add a rule that *computes* one square from the others instead of supplying it.
* Introduce multiple triangles; the single rule handles them all.

