# 🐦 Tweety & Polly in eyelet

This eyelet reasoning program models the classic *Tweety* example: **most birds fly, unless they are abnormal**.
It adds a twist—**penguins are abnormal birds**, plus optional *probability* annotations that capture how confident we are a normal bird can fly.

The model illustrates:

* Default / exception reasoning with *abnormality* flags
* Class inheritance (`:Penguin` ⇒ `:Bird`)
* Answer production with `log:impliesAnswer`

## 🐤 Facts

We begin with two individuals:

```turtle
:tweety  rdf:type :Bird ;
         :abnormal false .

:polly   rdf:type :Penguin .  # abnormality & flight decided by rules
```

## 📜 Rules

### 1. Penguins are Birds

```turtle
[ log:graph ( [ log:triple (var:X rdf:type :Penguin) ] ) ]
    log:implies
[ log:graph ( [ log:triple (var:X rdf:type :Bird) ] ) ].
```

### 2. Penguins are Abnormal

```turtle
[ log:graph ( [ log:triple (var:X rdf:type :Penguin) ] ) ]
    log:implies
[ log:graph ( [ log:triple (var:X :abnormal true) ] ) ].
```

### 3. Normal Birds Fly (with probability 0.9)

```turtle
[ log:graph (
    [ log:triple (var:X rdf:type :Bird) ]
    [ log:triple (var:X :abnormal false) ]
) ] log:implies [ log:graph (
    [ log:triple (var:X :flies true) ]
    [ log:triple (var:X :fliesProb "0.9"^^xsd:double) ]
) ].
```

### 4. Abnormal Birds Do NOT Fly (with probability 0.0)

```turtle
[ log:graph (
    [ log:triple (var:X rdf:type :Bird) ]
    [ log:triple (var:X :abnormal true) ]
) ] log:implies [ log:graph (
    [ log:triple (var:X :flies false) ]
    [ log:triple (var:X :fliesProb "0.0"^^xsd:double) ]
) ].
```

## ❓ Answer Rules

To retrieve answers for flight, probability, and abnormality:

```turtle
[ log:graph ( [ log:triple (var:A :flies var:B) ] ) ]
    log:impliesAnswer
[ log:graph ( [ log:triple (var:A :flies var:B) ] ) ].

[ log:graph ( [ log:triple (var:A :fliesProb var:P) ] ) ]
    log:impliesAnswer
[ log:graph ( [ log:triple (var:A :fliesProb var:P) ] ) ].

[ log:graph ( [ log:triple (var:A :abnormal var:B) ] ) ]
    log:impliesAnswer
[ log:graph ( [ log:triple (var:A :abnormal var:B) ] ) ].
```

## ▶️ Running the program

Use the EYE reasoner to run this:

```bash
eye --quiet [--nope] tweety.ttl
```

You should get results like:

```turtle
:tweety :abnormal false.
:tweety :flies true.
:tweety :fliesProb "0.9"^^xsd:double.

:polly :abnormal true.
:polly :flies false.
:polly :fliesProb "0.0"^^xsd:double.
```

## 🧠 Summary

This example demonstrates how **nonmonotonic reasoning** and **probabilistic annotations** can work together using N3 logic in EYE. By layering class hierarchies and exception-based rules, we get nuanced, human-like inference over simple data.

