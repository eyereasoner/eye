# 🧮 Meta-Interpreter in eyelet

This eyelet reasoning model implements a **meta-interpreter**, a reasoning construct that **interprets its own logic rules**. It’s inspired by [Markus Triska’s meta-interpreter](https://www.metalevel.at/acomip/) and showcases how to define logic programming semantics within N3 Logic.

The model also includes an encoding of **natural numbers**, demonstrating recursive computation within a logical framework.

---

## 📚 Prefixes

```turtle
@prefix list: <http://www.w3.org/2000/10/swap/list#> .
@prefix log:  <http://www.w3.org/2000/10/swap/log#> .
@prefix var:  <http://www.w3.org/2000/10/swap/var#> .
@prefix :     <http://example.org/#> .
```

---

## 🧠 Meta-Interpreter Rules

### Base Case: The empty program succeeds

```turtle
[ log:graph (
    [ log:triple (() :mi ()) ]
)] log:isImpliedBy true.
```

This defines success for an empty goal list.

---

### Recursive Case: Evaluate a goal and then continue

```turtle
[ log:graph (
    [ log:triple (() :mi var:A) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:A list:firstRest (var:G var:Gs)) ]
    [ log:triple (() :headBody (var:G var:Goals var:Gs)) ]
    [ log:triple (() :mi var:Goals) ]
)].
```

This rule evaluates the head of a goal list (`var:G`) by matching it to a rule body, then recursively evaluates the body.

---

## 🧩 Rule Interpretation (Head–Body Pairs)

These rules extract and evaluate `headBody` rules, simulating a Prolog-like interpreter in RDF:

```turtle
[ log:graph (
    [ log:triple (() :headBody ([ log:graph (
        [ log:triple (() :mi var:A) ]
    )] var:B var:Rs)) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:A list:firstRest (var:G var:Gs)) ]
    [ log:triple (var:B list:firstRest ([ log:graph (
        [ log:triple (() :headBody (var:G var:Goals var:Gs)) ]
    )] var:C)) ]
    [ log:triple (var:C list:firstRest ([ log:graph (
        [ log:triple (() :mi var:Goals) ]
    )] var:Rs)) ]
)].
```

These rules recursively evaluate compound structures of goals, just like a mini logic engine.

---

## 🔢 Natural Number Logic

This portion encodes **Peano-style natural numbers** using successor notation:

### Base Case: Zero is a natural number

```turtle
[ log:graph (
    [ log:triple (() :headBody ([ log:graph (
        [ log:triple (() :natnum (0)) ]
    )] var:Rs var:Rs)) ]
)] log:isImpliedBy true.
```

### Recursive Case: `s(X)` is a natnum if `X` is a natnum

```turtle
[ log:graph (
    [ log:triple (() :headBody ([ log:graph (
        [ log:triple (() :natnum (:s var:X)) ]
    )] var:A var:Rs)) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:A list:firstRest ([ log:graph (
        [ log:triple (() :natnum var:X) ]
    )] var:Rs)) ]
)].
```

---

## ❓ Query: Evaluate a Deeply Nested Goal

```turtle
[ log:graph (
    [ log:triple (() :mi ([ log:graph (
        [ log:triple (() :mi ([ log:graph (
            [ log:triple (() :mi ([ log:graph (
                [ log:triple (() :natnum (:s (:s (:s (:s (:s (0))))))) ]
            )])) ]
        )])) ]
    )])) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple (() :natnum (:s (:s (:s (:s (:s (0))))))) ]
)].
```

This query asks whether `:s(:s(:s(:s(:s(0)))))` is a valid natural number using the meta-interpreter. The deeply nested `:mi` calls simulate layered interpretation.

---

### ✅ Expected Output

```turtle
():natnum (:s (:s (:s (:s (:s (0))))))
```

Which confirms that 5 (in successor notation) is a valid natural number.

---

> **TIP:** A meta-interpreter allows eyelet to reason about its own reasoning rules — enabling logic reflection and dynamic rule chaining.

> **NOTE:** This model recursively evaluates goals using RDF list structure and simulated logic program clauses.

> **Reference:** Inspired by [Markus Triska's article](https://www.metalevel.at/acomip/) on meta-interpreters in Prolog, adapted here into RDF/N3 logic with eyelet.

