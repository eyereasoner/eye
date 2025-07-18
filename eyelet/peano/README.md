# ➕ Peano Arithmetic in EYE

This example encodes **Peano arithmetic**—addition, multiplication, and factorial—using successor notation in Turtle/N3.
Natural numbers are represented as nested `:s` (successor) functors, e.g. `0`, `(:s 0)` = 1, `(:s (:s 0))` = 2, etc.

---

## 🧮 Addition Rules

```turtle
# A + 0 = A
[ log:graph ( [ log:triple ((var:A 0) :add var:A) ] ) ] log:isImpliedBy true.

# A + S(B) = S(C)  ←  A + B = C
[ log:graph ( [ log:triple ((var:A (:s var:B)) :add (:s var:C)) ] ) ]
    log:isImpliedBy [ log:graph ( [ log:triple ((var:A var:B) :add var:C) ] ) ].
```

## ✖️ Multiplication Rules

```turtle
# A × 0 = 0
[ log:graph ( [ log:triple ((var:A 0) :multiply 0) ] ) ] log:isImpliedBy true.

# A × S(B) = C  ←  A × B = D ∧ A + D = C
[ log:graph ( [ log:triple ((var:A (:s var:B)) :multiply var:C) ] ) ]
    log:isImpliedBy [ log:graph (
        [ log:triple ((var:A var:B) :multiply var:D) ]
        [ log:triple ((var:A var:D) :add var:C) ]
    ) ].
```

## ⚡ Factorial Rules

```turtle
# Factorial(X) ↦ Y via helper :fac carrying accumulator
[ log:graph ( [ log:triple (var:A :factorial var:B) ] ) ]
    log:isImpliedBy [ log:graph ( [ log:triple ((var:A (:s 0)) :fac var:B) ] ) ].

# Base case: fac(0,A) = A
[ log:graph ( [ log:triple ((0 var:A) :fac var:A) ] ) ] log:isImpliedBy true.

# Recursive: fac(S(A),B) = C  ←  multiply(B,S(A)) = D ∧ fac(A,D) = C
[ log:graph ( [ log:triple (((:s var:A) var:B) :fac var:C) ] ) ]
    log:isImpliedBy [ log:graph (
        [ log:triple ((var:B (:s var:A)) :multiply var:D) ]
        [ log:triple ((var:A var:D) :fac var:C) ]
    ) ].
```

---

## ❓ Query

The query computes:

1. `1 × 3`
2. adds `2`
3. takes the factorial of the result.

```turtle
[ log:graph (
    [ log:triple (((:s 0) (:s (:s (:s 0)))) :multiply var:A) ] ; 1 × 3
    [ log:triple ((var:A (:s (:s 0))) :add var:B) ]             ; + 2
    [ log:triple (var:B :factorial var:C) ]                     ; factorial
) ] log:impliesAnswer [ log:graph ( [ log:triple (var:B :factorial var:C) ] ) ].
```

Expected `var:C` corresponds to **120** in successor form.

---

## ▶️ Running the Program

```bash
eye --quiet --nope peano.ttl
```

Omit `--nope` to view the proof tree:

```bash
eye --quiet peano.ttl
```

---

## 🧠 Summary

This example illustrates how EYE can perform arithmetic purely through logical rules and successor terms, demonstrating recursion, accumulators, and helper predicates.

