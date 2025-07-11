# 📅 Age Checker – Temporal Reasoning in N3 Logic

This example implements a logic-based **age checking system** in **Notation3 (N3)**. It calculates whether a person is older than a given duration (e.g., 80 years) based on their birthday and the current date, using temporal and arithmetic operations.

---

## 📘 Prefixes

```ttl
@prefix xsd: <http://www.w3.org/2001/XMLSchema#>.
@prefix time: <http://www.w3.org/2000/10/swap/time#>.
@prefix math: <http://www.w3.org/2000/10/swap/math#>.
@prefix log: <http://www.w3.org/2000/10/swap/log#>.
@prefix var: <http://www.w3.org/2000/10/swap/var#>.
@prefix : <https://example.org/#>.
````

---

## 🧾 Person Data

### 🎂 Birthday

```ttl
:patH :birthDay "1944-08-21"^^xsd:date.
```

Pat H was born on August 21, 1944.

---

## 🧠 Reasoning Rule

The rule checks whether a person is **older than a given duration**.

```ttl
[ log:graph (
  [ log:triple (var:S :ageAbove var:A)]
)] log:isImpliedBy [ log:graph (
  [ log:triple (var:S :birthDay var:B)]
  [ log:triple ("" time:localTime var:D)]
  [ log:triple ((var:D var:B) math:difference var:F)]
  [ log:triple (var:F math:greaterThan var:A)]
)].
```

### Logical Meaning:

> If a person `S` has a `birthDay` `B`,
> and the current date is `D`,
> then compute the difference `F = D - B`.
> If `F` is greater than some duration `A`, then `S :ageAbove A`.

This performs:

* **Date subtraction**
* **Duration comparison**
* **Logical inference based on age**

---

## ❓ Query

```ttl
[ log:graph (
  [ log:triple (var:S :ageAbove "P80Y"^^xsd:duration)]
)] log:impliesAnswer [ log:graph (
  [ log:triple (var:S :ageAbove "P80Y"^^xsd:duration)]
)].
```

This asks:

> Is there a person whose age is **above 80 years**?

The system will answer:

```ttl
:patH :ageAbove "P80Y"^^xsd:duration.
```

Assuming the current date is beyond **2024-08-21**, Pat H is over 80.

---

## ✅ Conclusion

This example shows how to:

* Use **time-based reasoning** with `time:localTime`
* Apply **arithmetic operations on dates**
* Define **temporal rules** for age-based logic
* Query **computed age categories**

> 🧠 This is a practical demonstration of applying logic to real-world data using dates and durations — a foundational use case for semantic web reasoning in domains like healthcare, legal age checks, and eligibility validation.

