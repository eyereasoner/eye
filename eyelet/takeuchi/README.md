# 🌀 Takeuchi Function in EYE

This example encodes the **Takeuchi function** (or `tak`) in Turtle/N3 using backward rules for the EYE reasoner.

The `tak` function is a recursive benchmark frequently used to stress logic engines. It is defined as:

```text
if x ≤ y then z
else tak(
  tak(x−1, y, z),
  tak(y−1, z, x),
  tak(z−1, x, y)
)
```

---

## 🔁 Tak Rules

### Base Case

```turtle
[ log:graph (
    [ log:triple ((var:X var:Y var:Z) :tak var:Z) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple ([ log:graph (
        [ log:triple (var:X math:notGreaterThan var:Y) ]
    )] log:call true) ]
    [ log:triple (true log:callWithCut true) ]
)].
```

### Recursive Case

```turtle
[ log:graph (
    [ log:triple ((var:X var:Y var:Z) :tak var:A) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple ((var:X 1) math:difference var:X1) ]
    [ log:triple ((var:X1 var:Y var:Z) :tak var:A1) ]
    [ log:triple ((var:Y 1) math:difference var:Y1) ]
    [ log:triple ((var:Y1 var:Z var:X) :tak var:A2) ]
    [ log:triple ((var:Z 1) math:difference var:Z1) ]
    [ log:triple ((var:Z1 var:X var:Y) :tak var:A3) ]
    [ log:triple ((var:A1 var:A2 var:A3) :tak var:A) ]
)].
```

---

## ❓ Query

Evaluate the Takeuchi function for (18, 13, 8):

```turtle
[ log:graph (
    [ log:triple ((18 13 8) :tak var:A) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple ((18 13 8) :tak var:A) ]
)].
```

---

## ▶️ Running the Program

Run using:

```bash
eye --quiet --nope takeuchi.ttl
```

To see the full recursive proof:

```bash
eye --quiet takeuchi.ttl
```

---

## 🧠 Summary

This example highlights deep recursion and control logic (including `log:callWithCut`) in EYE. It’s a benchmark for evaluating symbolic reasoning depth and stack performance.

