# ✨ Euler's Identity in EYE

This example demonstrates how to derive **Euler’s identity** using symbolic complex arithmetic in Notation3/Turtle with the EYE reasoner.

It encodes complex number addition, polar transformation, and Euler’s formula:

> $e^{i\pi} + 1 = 0$

using N3 rules and math operations over real and complex pairs.

## 🧮 Complex Arithmetic

We define complex addition:

```turtle
[ log:graph (
    [ log:triple (((var:A var:B) (var:C var:D)) complex:sum (var:E var:F))]
)] log:isImpliedBy [ log:graph (
    [ log:triple ((var:A var:C) math:sum var:E)]
    [ log:triple ((var:B var:D) math:sum var:F)]
)].
```

And complex exponentiation, using polar coordinates and Euler's formula:

```turtle
[ log:graph (
    [ log:triple (((var:A var:B) (var:C var:D)) complex:exponentiation (var:E var:F))]
)] log:isImpliedBy [ log:graph (
    [ log:triple ((var:A var:B) complex:polar (var:R var:T))]
    ... (truncated for brevity)
)].
```

## 🔁 Polar Conversion

To support complex exponentiation, we convert rectangular coordinates to polar:

```turtle
[ log:graph (
    [ log:triple ((var:X var:Y) complex:polar (var:R var:Tp))]
)] log:isImpliedBy [ log:graph (
    ... (squares, sqrt, acos, etc.)
)].
```

Dial adjustments handle quadrant-specific angles.

## ❓ Query

We ask EYE to compute:

```turtle
:e ≈ (2.718281828459045 0)
:π ≈ (0 3.141592653589793)
```

```turtle
[ log:graph (
    [ log:triple (((2.718281828459045 0) (0 3.141592653589793)) complex:exponentiation (var:A var:B))]
    [ log:triple (((var:A var:B) (1 0)) complex:sum (var:C var:D))]
)] log:impliesAnswer [ log:graph (
    [ log:triple (((2.718281828459045 0) (0 3.141592653589793)) complex:exponentiation (var:A var:B))]
    [ log:triple (((var:A var:B) (1 0)) complex:sum (var:C var:D))]
)].
```

Expected result: `(var:C var:D)` ≈ (0 0)

## ▶️ Running the Program

Use EYE like this:

```bash
eye --quiet --nope eulers-identity.ttl
```

To display a full proof tree, omit `--nope`:

```bash
eye --quiet eulers-identity.ttl
```

## 🧠 Summary

This example showcases symbolic complex arithmetic, including polar transformations and exponentiation in EYE.
It demonstrates that logic reasoning can reconstruct core mathematical identities like Euler’s famous equation from first principles.

