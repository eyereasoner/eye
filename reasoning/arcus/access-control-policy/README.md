# 🔐 Access Control Policy in EYE

This example models an **access control policy** using Notation3/Turtle with the EYE reasoner. It evaluates whether a `:Test` instance satisfies all the conditions (all-of, any-of, none-of) defined in a `:Policy`.

Each policy field is checked using quantified logic constructs like `log:forAllIn` and `log:collectAllIn` over RDF triples.

---

## 🧪 Example Test & Policy

```turtle
:test1 :policy :PolicyX ;
    :has :A, :B, :C .

:PolicyX a :Policy ;
    :allOf :A, :B ;
    :anyOf :C ;
    :noneOf :D .
```

---

## ✅ Rule: AllOf Test

All required fields must be present:

```turtle
[ log:graph (
    [ log:triple (var:Pol :pass :allOfTest) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:Test :policy var:Pol) ]
    [ log:triple (var:Pol rdf:type :Policy) ]
    [ log:triple (([ log:graph ([ var:Pol :allOf var:Field ]) ]
                   [ log:graph ([ var:Test :has var:Field ]) ])
                   log:forAllIn var:X) ]
)].
```

---

## ✅ Rule: AnyOf Test

At least one `anyOf` field must be matched:

```turtle
[ log:graph (
    [ log:triple (var:Pol :pass :anyOfTest) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:Test :policy var:Pol) ]
    [ log:triple (var:Pol rdf:type :Policy) ]
    [ log:triple ((var:Field [ log:graph ([ var:Pol :anyOf var:Field ] [ var:Test :has var:Field ]) ] var:List)
                  log:collectAllIn var:X) ]
    [ log:triple (var:List list:length var:L) ]
    [ log:triple ((var:L) log:notEqualTo (0)) ]
)].
```

---

## ✅ Rule: NoneOf Test

No `noneOf` fields may be present:

```turtle
[ log:graph (
    [ log:triple (var:Pol :pass :noneOfTest) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:Test :policy var:Pol) ]
    [ log:triple (var:Pol rdf:type :Policy) ]
    [ log:triple ((var:Field [ log:graph ([ var:Pol :noneOf var:Field ] [ var:Test :has var:Field ]) ] var:List)
                  log:collectAllIn var:X) ]
    [ log:triple (var:List list:length var:L) ]
    [ log:triple ((var:L) log:equalTo (0)) ]
)].
```

---

## ❓ Query

Ask whether any policy passes all three tests:

```turtle
[ log:graph (
    [ log:triple (var:Pol rdf:type :Policy) ]
    [ log:triple (var:Pol :pass :allOfTest) ]
    [ log:triple (var:Pol :pass :anyOfTest) ]
    [ log:triple (var:Pol :pass :noneOfTest) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple (:test :for var:Pol) ]
    [ log:triple (:test :is true) ]
)].
```

---

## ▶️ Running the Program

Run the policy logic with:

```bash
eye --quiet --nope access-control-policy.ttl
```

To inspect the proof:

```bash
eye --quiet access-control-policy.ttl
```

---

## 🧠 Summary

This access control logic models real-world conditions such as *required*, *optional*, and *prohibited* fields using RDF and quantified logic. EYE’s support for `log:forAllIn` and `log:collectAllIn` allows expressive, declarative validation.

