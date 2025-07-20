# ✈️ Airroute Reasoning with eyelet

This eyelet logic program demonstrates **graph traversal and route discovery** using RDF Turtle and N3 Logic. It's inspired by [AWS Neptune's blog on building knowledge graphs with RDF and OpenCypher](https://aws.amazon.com/blogs/database/build-and-deploy-knowledge-graphs-faster-with-rdf-and-opencypher/).

The goal is to compute valid flight routes between airports with a limited number of stopovers.

---

## 📚 Prefixes

```turtle
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix nepo: <http://neptune.aws.com/ontology/airroutes/> .
@prefix list: <http://www.w3.org/2000/10/swap/list#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix log:  <http://www.w3.org/2000/10/swap/log#> .
@prefix var:  <http://www.w3.org/2000/10/swap/var#> .
@prefix pd:   <http://example.org/pd#> .
```

---

## 🧠 Base Case: Direct Route

```turtle
[ log:graph (
    [ log:triple ((var:from var:to var:visited var:length var:max) pd:route (var:from var:to)) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:length math:notGreaterThan var:max) ]
    [ log:triple (var:from nepo:hasOutboundRouteTo var:to) ]
    [ log:triple (var:visited list:notMember var:to) ]
)].
```

This rule allows a direct connection from `from` to `to` if:

* The current route length does not exceed `max`
* `to` has not already been visited

---

## 🔁 Recursive Case: Route via Another Airport

```turtle
[ log:graph (
    [ log:triple ((var:from var:to var:visited var:length var:max) pd:route var:route) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:length math:notGreaterThan var:max) ]
    [ log:triple (var:from nepo:hasOutboundRouteTo var:via) ]
    [ log:triple (var:visited list:notMember var:via) ]
    [ log:triple (var:newVisited list:firstRest (var:from var:visited)) ]
    [ log:triple ((var:length 1) math:sum var:newLength) ]
    [ log:triple ((var:via var:to var:newVisited var:newLength var:max) pd:route var:newRoute) ]
    [ log:triple (var:route list:firstRest (var:from var:newRoute)) ]
)].
```

This rule builds a recursive route:

* Advances from `from` to `via`, then to `to`
* Tracks visited airports to prevent cycles
* Increments the length of the path at each step

---

## 🔍 Query: Find Routes with At Most 2 Stopovers

```turtle
[ log:graph (
    [ log:triple (var:source rdfs:label "Manchester-Boston Regional Airport") ]
    [ log:triple (var:destination rdfs:label "Helsinki Vantaa Airport") ]

    # Find routes from source to destination with ≤ 2 stopovers
    [ log:triple ((var:source var:destination () 0 2) pd:route var:airports) ]

    # Lookup city labels for airports in the route
    [ log:triple ((var:city [ log:graph (
        [ log:triple (var:airports list:member var:airport) ]
        [ log:triple (var:airport rdfs:label var:city) ]
    ) ] var:route) log:collectAllIn var:scope) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple (pd:discovered pd:airroute var:route) ]
)].
```

This query:

* Looks up airport IRIs for two cities by their labels
* Finds all paths from source to destination with up to 2 stopovers
* Collects the readable labels of airports along the route
* Outputs the discovered route

---

> **TIP:** This program uses `list:notMember` to avoid revisiting airports and `math:notGreaterThan` to constrain route length.

> **NOTE:** Routes are built recursively, and airport labels are resolved using a `log:collectAllIn` pattern.

> **Reference:** Based on [this AWS Neptune blog post](https://aws.amazon.com/blogs/database/build-and-deploy-knowledge-graphs-faster-with-rdf-and-opencypher/) and implemented in eyelet using RDF and N3 Logic.

