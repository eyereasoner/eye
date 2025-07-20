# 🏷️ Deep Taxonomy Reasoning in EYE

This example stress‑tests subclass reasoning with a **10 000‑level taxonomy**. A single instance (`dt:TestVariable`) is typed as `dt:N1`; a simple transitive rule infers its membership all the way up to `dt:A2`.

The dataset size illustrates how EYE handles large RDFS‑style hierarchies.

See https://web.archive.org/web/20220119222608/http://responder.ruleml.org/WellnessRules/files/WellnessRulesN3-2009-11-10.pdf

---

## 🗂️ Fact

```turtle
dt:TestVariable a dt:N1.
```

---

## 🏛️ Taxonomy (excerpt)

```turtle
dt:N0 rdfs:subClassOf dt:N1.
dt:N1 rdfs:subClassOf dt:N2.
dt:N2 rdfs:subClassOf dt:N3.
...
dt:N9999 rdfs:subClassOf dt:N10000.
dt:N10000 rdfs:subClassOf dt:A2.
```

The full file contains three parallel subclass chains (`N`, `I`, `J`) for variety. Only the `N` chain ultimately points at `dt:A2`.

---

## ⚙️ Subclass Rule

```turtle
[ log:graph (
    [ log:triple (var:A rdfs:subClassOf var:B) ]
    [ log:triple (var:S rdf:type var:A) ]
)] log:implies [ log:graph (
    [ log:triple (var:S rdf:type var:B) ]
)].
```

This single rule mimics the RDFS subclass closure.

---

## ❓ Query

Ask *which* resources end up typed as `dt:A2`:

```turtle
[ log:graph ( [ log:triple (var:X rdf:type dt:A2) ] ) ]
    log:impliesAnswer
[ log:graph ( [ log:triple (var:X rdf:type dt:A2) ] ) ].
```

With the provided data, the expected answer is `dt:TestVariable`.

---

## ▶️ Running the Program

```bash
eye --quiet --nope deep-taxonomy.ttl
```

Drop `--nope` to inspect the proof (it will show each inferred type):

```bash
eye --quiet deep-taxonomy.ttl
```

Because the chain is 10 000 levels deep, the proof can be lengthy.

---

## 🧠 Summary

This example demonstrates that EYE can compute transitive subclass closures across **very deep class hierarchies** efficiently with a minimal rule set.

Try adjusting chain length or adding parallel branches to explore performance scaling.

