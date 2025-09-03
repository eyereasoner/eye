# ⚙️ Control Systems with arcus

This example demonstrates how to model and reason over control systems using **N3 Logic** and **arcus**. It includes **sensor inputs**, **state observations**, **disturbances**, and **forward and backward control rules** that result in actuator outputs.

---

## 📚 Prefixes

```turtle
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix log: <http://www.w3.org/2000/10/swap/log#> .
@prefix var: <http://www.w3.org/2000/10/swap/var#> .
@prefix : <https://eyereasoner.github.io/eye/reasoning/cs#> .
```

---

## 🔢 Inputs

```turtle
:input1 :measurement1 (6 11) .
:input2 :measurement2 true .
:input3 :measurement3 56967 .
```

---

## 🌪️ Disturbances

```turtle
:disturbance1 :measurement3 35766 .
:disturbance2 :measurement1 (45 39) .
```

---

## 📊 System State

```turtle
:state1 :observation1 80 .
:state2 :observation2 false .
:state3 :observation3 22 .
```

---

## 🎯 Output Measurements

```turtle
:output2 :measurement4 24 .
:output2 :target2 29 .
```

---

## 🔁 Forward Rules

### Feedforward Control (with disturbance compensation)

```turtle
[ log:graph (
    [ log:triple (:input1 :measurement10 var:M1) ]
    [ log:triple (:input2 :measurement2 true) ]
    [ log:triple (:disturbance1 :measurement3 var:D1) ]
    [ log:triple ((var:M1 19.6) math:product var:C1) ]
    [ log:triple ((10 var:C2) math:exponentiation var:D1) ]
    [ log:triple ((var:C1 var:C2) math:difference var:C) ]
)] log:implies [ log:graph (
    [ log:triple (:actuator1 :control1 var:C) ]
)] .
```

### PND Feedback Control (Proportional–Nonlinear–Differential)

```turtle
[ log:graph (
    [ log:triple (:input3 :measurement3 var:M3) ]
    [ log:triple (:state3 :observation3 var:P3) ]
    [ log:triple (:output2 :measurement4 var:M4) ]
    [ log:triple (:output2 :target2 var:T2) ]
    [ log:triple ((var:T2 var:M4) math:difference var:E) ]
    [ log:triple ((var:P3 var:M4) math:difference var:D) ]
    [ log:triple ((5.8 var:E) math:product var:C1) ]
    [ log:triple ((7.3 var:E) math:quotient var:N) ]
    [ log:triple ((var:N var:D) math:product var:C2) ]
    [ log:triple ((var:C1 var:C2) math:sum var:C) ]
)] log:implies [ log:graph (
    [ log:triple (:actuator2 :control1 var:C) ]
)] .
```

---

## 🔄 Backward Rules

### Infer `:measurement10` when values differ

```turtle
[ log:graph (
    [ log:triple (var:I :measurement10 var:M) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:I :measurement1 (var:M1 var:M2)) ]
    [ log:triple (var:M1 math:lessThan var:M2) ]
    [ log:triple ((var:M2 var:M1) math:difference var:M3) ]
    [ log:triple ((var:M3 0.5) math:exponentiation var:M) ]
)] .
```

### Otherwise fallback to direct value

```turtle
[ log:graph (
    [ log:triple (var:I :measurement10 var:M1) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:I :measurement1 (var:M1 var:M2)) ]
    [ log:triple (var:M1 math:notLessThan var:M2) ]
)] .
```

---

## ❓ Query

```turtle
[ log:graph (
    [ log:triple (var:O :control1 var:C) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple (var:O :control1 var:C) ]
)] .
```

This query returns any actuator output `:control1` computed from the inputs and state.

---

## ✅ Output Example

```turtle
:actuator1 :control1 ... .
:actuator2 :control1 ... .
```

The final control outputs are derived through either feedforward or feedback rules.

---

> **NOTE:** This setup allows combining sensor data, environmental disturbances, and state observations into logic-based control decisions—bridging **symbolic reasoning** with **control theory**.

> **TIP:** You can easily extend this by adding noise filters, actuator constraints, or tuning PID parameters as part of the rule logic.

