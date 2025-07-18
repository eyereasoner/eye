# 🐞 Beetle Example 12: Disjunction Elimination in EYE

This example models **disjunction elimination** in Notation3/Turtle using negative-surface logic.  
Starting from a known fact (“a beetle is a Car”) and chained disjunctions, it infers that the beetle must be **beautiful**.

---

## ✅ Fact

```turtle
:beetle a :Car.
````

---

## 🎨 Color Disjunction: Cars are green or blue

```turtle
(_:A) log:onNegativeSurface [ log:graph (
  [ log:triple (_:A rdf:type :Car)]
  [ log:triple (() log:onNegativeSurface [ log:graph ( [ log:triple (_:A :is :green) ] )])]
  [ log:triple (() log:onNegativeSurface [ log:graph ( [ log:triple (_:A :is :blue) ] )])]
)].
```

---

## 🌟 Green Implies Nice or Pretty

```turtle
(_:A) log:onNegativeSurface [ log:graph (
  [ log:triple (_:A :is :green)]
  ... similar disjunction for `:nice` and `:pretty` ...
)].
```

Nested disjunction elimination continues for `:pretty` → `:pretty1|2`, and `:nice` → `:nice1|2`, plus all refinements.

---

## ✨ Final Disjunction to `:beautiful`

```turtle
(_:A) log:onNegativeSurface [ log:graph (
  ... excludes *all* pretty1/2, pretty2/2, nice1/2, nice2/2, and blue ...
  [ log:triple (() log:onNegativeSurface [ log:graph ( [ log:triple (_:A :is :beautiful) ] )])]
)].
```

---

## ❓ Query

```turtle
(_:S) log:onNegativeSurface [ log:graph (
  [ log:triple (_:S :is :beautiful)]
  [ log:triple (() log:onNegativeAnswerSurface [ log:graph ( [ log:triple (_:S :is :beautiful) ] )])]
)].
```

---

## ▶️ Running the Program

```bash
eye --quiet --nope beetle12.ttl
```

To view detailed proof steps, drop `--nope`:

```bash
eye --quiet beetle12.ttl
```

---

## 🧠 Summary

This example achieves **case-based disjunction elimination** through layered negative-surface tests, ultimately inferring a concrete property (`:beautiful`) from deep nested choice structures.

