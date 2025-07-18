# ![Bird Icon](https://img.icons8.com/emoji/48/bird-emoji.png) Tweety & Polly in eyelet

This eyelet reasoning program models the classic *Tweety* example: **most birds fly, unless they are abnormal**.
It adds a twist—**penguins are abnormal birds**, plus optional *probability* annotations that capture how confident we are a normal bird can fly.

The model illustrates:

* Default / exception reasoning with *abnormality* flags
* Class inheritance (`:Penguin` ⇒ `:Bird`)
* Answer production with `log:impliesAnswer`

## ![Bird Icon](https://img.icons8.com/emoji/24/bird-emoji.png) Facts

We begin with two individuals:

```n3
:tweety  rdf:type :Bird ;
         :abnormal false .

:polly   rdf:type :Penguin .  # abnormality & flight decided by rules
```

## ![Scroll Icon](https://img.icons8.com/emoji/24/scroll-emoji.png) Rules

### 1. Penguins are Birds

```n3
[ log:graph ( [ log:triple (var:X rdf:type :Penguin) ] ) ]
    log:implies
[ log:graph ( [ log:triple (var:X rdf:type :Bird) ] ) ].
```

### 2. Penguins are Abnormal

```n3
[ log:graph ( [ log:triple (var:X rdf:type :Penguin) ] ) ]
    log:implies
[ log:graph ( [ log:triple (var:X :abnormal true) ] ) ].
```

### 3. Normal Birds Fly (with probability 0.9)

```n3
[ log:graph (
    [ log:triple (var:X rdf:type :Bird) ]
    [ log:triple (var:X :abnormal false) ]
) ] log:implies [ log:graph (
    [ log:triple (var:X :flies true) ]
    [ log:triple (var:X :fliesProb "0.9"^^xsd:double) ]
) ].
```

### 4. Abnormal Birds Do NOT Fly (with probability 0.0)

```n3
[ log:graph (
    [ log:triple (var:X rdf:type :Bird) ]
    [ log:triple (var:X :abnormal true) ]
) ] log:implies [ log:graph (
    [ log:triple (var:X :flies false) ]
    [ log:triple (var:X :fliesProb "0.0"^^xsd:double) ]
) ].
```

## ![Question Icon](https://img.icons8.com/emoji/24/question-mark-emoji.png) Answer Rules

To retrieve answers for flight, probability, and abnormality:

```n3
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

## ![Play Icon](https://img.icons8.com/emoji/24/play-button-emoji.png) Running the program

Use the EYE reasoner to run this:

```bash
eye tweety.n3 --nope --query tweety.n3
```

You should get results like:

```n3
:tweety :abnormal false.
:tweety :flies true.
:tweety :fliesProb "0.9"^^xsd:double.

:polly :abnormal true.
:polly :flies false.
:polly :fliesProb "0.0"^^xsd:double.
```

## ![Brain Icon](https://img.icons8.com/emoji/24/brain-emoji.png) Summary

This example demonstrates how **nonmonotonic reasoning** and **probabilistic annotations** can work together using N3 logic in EYE. By layering class hierarchies and exception-based rules, we get nuanced, human-like inference over simple data.

