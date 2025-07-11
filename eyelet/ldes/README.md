# 🔄 Linked Data Event Streams (LDES) with Policies in Eyelet

This example demonstrates how to model and reason over a **transactional LDES member** using **Eyelet** and **N3 logic**. It captures an event with metadata such as the payload, provenance, policy, and cryptographic signature.

Based on [LDES](https://github.com/SEMICeu/LinkedDataEventStreams) and ActivityStreams.

---

## 📚 Prefixes

```turtle
@prefix tree:   <https://w3id.org/tree#> .
@prefix ldes:   <https://w3id.org/ldes#> .
@prefix dcat:   <http://www.w3.org/ns/dcat#> .
@prefix prov:   <http://www.w3.org/ns/prov#> .
@prefix rdf:    <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs:   <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd:    <http://www.w3.org/2001/XMLSchema#> .
@prefix patch:  <http://example.org/patch#> .
@prefix as:     <http://example.org/as#> .
@prefix list:   <http://www.w3.org/2000/10/swap/list#> .
@prefix log:    <http://www.w3.org/2000/10/swap/log#> .
@prefix var:    <http://www.w3.org/2000/10/swap/var#> .
@prefix :       <http://example.org/#> .
```

---

## 🌐 Event Stream Definition

```turtle
:LDES a ldes:EventStream ;
    rdfs:comment "An LDES with per member: an ActivityStreams update, the payload and the signature of the payload" ;
    tree:view <> ;
    tree:member <A> .
```

---

## 🧩 Member: A Transactional LDES Entry

```turtle
<A> a patch:Event ;
    patch:processingMethod patch:Upsert ;
    patch:upsertKey <https://example.org/Dataset1> ;
    patch:transaction :Transaction1 ;
    patch:upsertPayload _:bn_1 ;
    patch:sequence 1 ;
    patch:time "2024-09-09T13:27:33.681Z" ;
    patch:provenance _:bn_2 ;
    patch:signature _:bn_3 ;
    patch:policy _:bn_4 .
```

Each field links to embedded content via RDF blank nodes.

---

## 📦 Payload Graph

```turtle
_:bn_1 log:graph (
    [ log:triple (<https://example.org/Dataset1> rdf:type dcat:Dataset) ]
).
```

---

## 🧾 Provenance Graph

```turtle
_:bn_2 log:graph (
    [ log:triple (<https://example.org/Dataset1#Event1> rdf:type as:Create) ]
    [ log:triple (<https://example.org/Dataset1#Event1> rdf:type prov:Activity) ]
    [ log:triple (<https://example.org/Dataset1#Event1> as:object <https://example.org/Dataset1>) ]
    [ log:triple (<https://example.org/Dataset1#Event1> as:published "2023-10-01T12:00:00Z"^^xsd:dateTime) ]
).
```

---

## 🔐 Signature Graph

```turtle
_:bn_3 log:graph (
    [ log:triple (_:bn_5 rdf:type :DataIntegrityProof) ]
    [ log:triple (_:bn_5 :signature "rCWNBuxBK1In93...") ]
    [ log:triple (_:bn_5 :target (_:bn_1 _:bn_2 _:bn_4)) ]
).
```

---

## ⚖️ Policy Graph

```turtle
_:bn_4 log:graph (
    [ log:triple (_:bn_6 rdf:type :Policy) ]
    [ log:triple (_:bn_6 :target (_:bn_1 _:bn_2 _:bn_3)) ]
    [ log:triple (_:bn_6 :duty _:bn_7) ]
    [ log:triple (_:bn_7 rdf:type :RemovalDuty) ]
    [ log:triple (_:bn_7 :after "P1M") ]
).
```

Defines a **policy** to remove the event data after 1 month.

---

## 🔍 Query: Extract Policy-Related Triples

```turtle
[ log:graph (
    [ log:triple (var:Node log:graph var:Content) ]
    [ log:triple (var:Content list:member var:Item1) ]
    [ log:triple (var:Item1 log:triple (var:Pol rdf:type :Policy)) ]
    [ log:triple (var:Content list:member var:Item2) ]
    [ log:triple (var:Item2 log:triple (var:Pol :target var:Target)) ]
    [ log:triple (var:Content list:member var:Item) ]
    [ log:triple (var:Item log:triple var:Triple) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple (var:Item log:triple var:Triple) ]
)].
```

This query retrieves all triples related to a `:Policy` and its `:target`.

---

### ✅ Sample Output

```turtle
_:item123 log:triple (_:bn_6 :target (_:bn_1 _:bn_2 _:bn_3)) .
_:item123 log:triple (_:bn_6 :duty _:bn_7) .
_:item123 log:triple (_:bn_7 :after "P1M") .
```

---

> **NOTE:** This LDES example demonstrates how to **encode temporal policies, provenance, and transactional behavior** in RDF and query it using Eyelet logic rules.

> **TIP:** You can extend this model with verification rules, retention enforcement, or time-based triggers using `xsd:duration` and external reasoning.

