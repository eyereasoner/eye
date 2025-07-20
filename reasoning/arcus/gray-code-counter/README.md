# 🟩 Gray Code Counter in EYE

This example implements a **Gray‑code up‑counter** (3‑bit) using backward rules written in Turtle/N3.
Gray codes change only **one bit at a time**, a property widely used in error‑minimising counters and rotary encoders.

The logic network is built from primitive Boolean operators (`:and`, `:or`, `:inv`), D‑flip‑flops (`:dff`), and two combinational sub‑nets (`:neta`, `:netb`).  A higher‑level rule `:gcc` (Gray‑Code Counter) wires them together, and a test harness `:testgcc` walks a stimulus sequence through the counter.

## 📐 Rule Logic

Below is a condensed view of the key rules (see *graycode.ttl* for full listings).

### 🧩 Primitives

```turtle
(0 0) :and 0.
(0 1) :and 0.
(1 0) :and 0.
(1 1) :and 1.

(0 0) :or 0.
(0 1) :or 1.
(1 0) :or 1.
(1 1) :or 1.

(0) :inv 1.
(1) :inv 0.
```

### ⏩ D‑Flip‑Flop Behaviour

```turtle
[ log:graph ( [ log:triple ((var:D 0 var:Q) :dff var:Q)] ) ]
    log:isImpliedBy true.

[ log:graph ( [ log:triple ((var:D 1 var:Q) :dff var:D)] ) ]
    log:isImpliedBy true.
```

### 🪄 Combinational Nets `:neta` and `:netb`

```turtle
# neta = (A AND B) OR (¬A AND ¬B)
[ log:graph ( [ log:triple ((var:A var:B) :neta var:Q)] ) ]
    log:isImpliedBy [ log:graph (
        [ log:triple ((var:A var:B) :and var:T1)]
        [ log:triple ((var:A) :inv var:NA)]
        [ log:triple ((var:B) :inv var:NB)]
        [ log:triple ((var:NA var:NB) :and var:T2)]
        [ log:triple ((var:T1 var:T2) :or var:Q)] ) ].

# netb returns two outputs (Q1,Q2) from (A,B,C)
[ log:graph ( [ log:triple ((var:A var:B var:C) :netb (var:Q1 var:Q2))] ) ]
    log:isImpliedBy [ log:graph (
        ... ) ].
```

### 🔁 Gray‑Code Counter `:gcc`

```turtle
[ log:graph ( [ log:triple ((var:C (var:Qa var:Qb var:Qc)) :gcc (var:Za var:Zb var:Zc))] ) ]
    log:isImpliedBy [ log:graph (
        [ log:triple ((var:Qa var:Qb var:Qc) :netb (var:D1 var:D2))]
        [ log:triple ((var:Qa var:Qb) :neta var:D3)]
        [ log:triple ((var:D1 var:C var:Qa) :dff var:Za)]
        [ log:triple ((var:D2 var:C var:Qb) :dff var:Zb)]
        [ log:triple ((var:D3 var:C var:Qc) :dff var:Zc)] ) ].
```

### 🧪 Test Harness `:testgcc`

```turtle
[ log:graph ( [ log:triple ((() var:S) :testgcc ())] ) ] log:isImpliedBy true.

[ log:graph ( [ log:triple ((var:Cc var:S) :testgcc var:Nc)] ) ]
    log:isImpliedBy [ log:graph (
        [ log:triple (var:Cc list:firstRest (var:C var:Cs))]
        [ log:triple (var:Nc list:firstRest (var:N var:Ns))]
        [ log:triple ((var:C var:S) :gcc var:N)]
        [ log:triple ((var:Cs var:N) :testgcc var:Ns)] ) ].
```

## ❓ Query

The sample query starts the counter with stimulus control bits `(1 1 … 1)` (9 ones) and an initial state `(0 0 0)`.  It requests the resulting state list `var:Q`:

```turtle
[ log:graph ( [ log:triple (((1 1 1 1 1 1 1 1 1) (0 0 0)) :testgcc var:Q)] ) ]
    log:impliesAnswer [ log:graph ( [ log:triple (((1 1 1 1 1 1 1 1 1) (0 0 0)) :testgcc var:Q)] ) ].
```

## ▶️ Running the Program

```bash
eye --quiet --nope gray-code-counter.ttl
```

Drop `--nope` if you want to inspect the proof tree:

```bash
eye --quiet graycode.ttl
```

Depending on list length, the run can be computationally intensive—start with shorter control‑bit lists to explore behaviour.

## 🧠 Summary

This example showcases non‑trivial **Boolean circuit modelling** and **stateful reasoning** in EYE, built entirely from declarative rules.  The Gray‑code counter demonstrates how recursion, arithmetic, and flip‑flop state can work together to simulate hardware logic within an N3/Turtle reasoning environment.

