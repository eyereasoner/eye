# 💊 Medication Prescription in EYE

This example shows how to model **clinical prescription logic** using negative surface reasoning in Notation3/Turtle with the EYE reasoner.

We simulate real-world logic for prescribing drugs based on conditions, symptoms, and contraindications—while ensuring no conflicting facts exist.

---

## 🧍 Patient Records

We define health facts and explicitly negate others using `log:onNegativeSurface` to indicate what is known *not* to be true.

### 👩‍⚕️ Ann

```turtle
:Ann :has :Fever.

() log:onNegativeSurface [ log:graph (
    [ log:triple (:Ann :has :AllergyForAspirin)] )].

() log:onNegativeSurface [ log:graph (
    [ log:triple (:Ann :has :ActivePepticUlcerDisease)] )].
```

### 👨‍⚕️ Joe

```turtle
:Joe :has :AcuteMyocardialInfarction.
:Joe :has :AllergyForAspirin.

() log:onNegativeSurface [ log:graph (
    [ log:triple (:Joe :has :ActivePepticUlcerDisease)] )].
() log:onNegativeSurface [ log:graph (
    [ log:triple (:Joe :has :SevereAsthma)] )].
() log:onNegativeSurface [ log:graph (
    [ log:triple (:Joe :has :ChronicObstructivePulmonaryDisease)] )].
```

---

## 📋 Prescription Logic

Prescriptions are made based on positive facts, *and* the absence of contraindications. All rules are on the negative surface.

### High-Dose Aspirin for Fever

```turtle
(_:WHO) log:onNegativeSurface [ log:graph (
    [ log:triple (_:WHO :has :Fever) ]
    [ log:triple (() log:onNegativeSurface [ log:graph ([ _:WHO :has :AllergyForAspirin ]) ]) ]
    [ log:triple (() log:onNegativeSurface [ log:graph ([ _:WHO :has :ActivePepticUlcerDisease ]) ]) ]
    [ log:triple (() log:onNegativeSurface [ log:graph ([ _:WHO :isPrescribed :aspirinHighDose ]) ]) ]
)].
```

### Low-Dose Aspirin for AMI

```turtle
(_:WHO) log:onNegativeSurface [ log:graph (
    [ log:triple (_:WHO :has :AcuteMyocardialInfarction) ]
    [ log:triple (() log:onNegativeSurface [ log:graph ([ _:WHO :has :AllergyForAspirin ]) ]) ]
    [ log:triple (() log:onNegativeSurface [ log:graph ([ _:WHO :has :ActivePepticUlcerDisease ]) ]) ]
    [ log:triple (() log:onNegativeSurface [ log:graph ([ _:WHO :isPrescribed :aspirinLowDose ]) ]) ]
)].
```

### Beta Blocker for AMI without Pulmonary Risk

```turtle
(_:WHO) log:onNegativeSurface [ log:graph (
    [ log:triple (_:WHO :has :AcuteMyocardialInfarction) ]
    [ log:triple (() log:onNegativeSurface [ log:graph ([ _:WHO :has :SevereAsthma ]) ]) ]
    [ log:triple (() log:onNegativeSurface [ log:graph ([ _:WHO :has :ChronicObstructivePulmonaryDisease ]) ]) ]
    [ log:triple (() log:onNegativeSurface [ log:graph ([ _:WHO :isPrescribed :betaBlocker ]) ]) ]
)].
```

---

## ❓ Query

Ask which patients are prescribed what medications, provided the answer can’t be contradicted:

```turtle
(_:WHO _:WHAT) log:onNegativeSurface [ log:graph (
    [ log:triple (_:WHO :isPrescribed _:WHAT) ]
    [ log:triple (() log:onNegativeAnswerSurface [ log:graph ([ _:WHO :isPrescribed _:WHAT ]) ]) ]
)].
```

---

## ▶️ Running the Program

```bash
eye --quiet --nope prescription.ttl
```

To include proof steps:

```bash
eye --quiet prescription.ttl
```

---

## 🧠 Summary

This example illustrates how **default logic**, **negation-as-failure**, and **medical guidelines** can be combined in EYE using `log:onNegativeSurface`. It enables clinical safety by enforcing contraindication checks declaratively.

