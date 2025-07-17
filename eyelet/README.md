# eyelet

## EYE for Logic expressed in Turtle

### ✅ Forward Rules

eyelet allows reasoning with forward rules written in RDF Turtle.

**Example: Subclass inference**

```turtle
# Subclass rule
[ log:graph (
    [ log:triple (var:A rdfs:subClassOf var:B) ]
    [ log:triple (var:S rdf:type var:A) ]
)] log:implies [ log:graph (
    [ log:triple (var:S rdf:type var:B) ]
)].
```

---

### ✅ Backward Rules

eyelet supports reasoning with backward rules, enabling goal-directed inference.

**Example: Checking if a person's age is above a threshold**

```turtle
# Is the age of a person above some duration?
[ log:graph (
    [ log:triple (var:S :ageAbove var:A) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:S :birthDay var:B) ]
    [ log:triple ("" time:localTime var:D) ]
    [ log:triple ((var:D var:B) math:difference var:F) ]
    [ log:triple (var:F math:greaterThan var:A) ]
)].
```

---

### ✅ RDF Surfaces

eyelet handles RDF Surfaces, enabling reasoning over negated or hypothetical graphs.

**Example: Cars are either green or blue**

```turtle
# All cars are green or blue
(_:A) log:onNegativeSurface [ log:graph (
    [ log:triple (_:A rdf:type :Car) ]
    [ log:triple (() log:onNegativeSurface [ log:graph (
        [ log:triple (_:A :is :green) ]
    ) ]) ]
    [ log:triple (() log:onNegativeSurface [ log:graph (
        [ log:triple (_:A :is :blue) ]
    ) ]) ]
)].
```

---

### ✅ Querying

eyelet supports structured queries in RDF Turtle.

**Example: Who is a what?**

```turtle
# Who is a what?
[ log:graph (
    [ log:triple (var:WHO rdf:type var:WHAT) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple (var:WHO rdf:type var:WHAT) ]
)].
```

---

> \[!NOTE]
> A forward rule with `log:implies false` acts as an **inference fuse**.

> \[!NOTE]
> The `var:` prefix refers to `<http://www.w3.org/2000/10/swap/var#>`.
> Variables are universally quantified, except those used only in the conclusion of a forward rule, which are **existentially** interpreted.

