# ✝️ Easter Date – Eyelet Computation Demo

Compute the **Gregorian Easter date** (Western Easter) purely with eyelet / N3
arithmetic built‑ins — no external code.  
The rule implements the standard *Anonymous Gregorian algorithm* (a variant of
Meeus/Jones/Butcher) using only integer quotient, remainder, sums, differences,
products, and negations.

The *query* asks for Easter for a **range of years** (2024 … 2050).

---

## 🧠 Core idea

A single rule derives

```

Year \:easter (Day Month)

````

by successively computing the intermediary variables (*a, b, c, d, e, …* in
traditional descriptions) through chained arithmetic triples.

---

## 🧾 Core rule (excerpt)

(Variables `var:x0 … var:x34` correspond to successive intermediate values.)

```turtle
# Derive (Day Month) for any Year
[ log:graph (
    [ log:triple (var:x0 :easter (var:x34 var:x32)) ]
) ] log:isImpliedBy [ log:graph (
    [ log:triple ((var:x0 19)  math:remainder        var:x1) ]   # a = Y mod 19
    [ log:triple ((var:x0 100) math:integerQuotient  var:x2) ]   # b = Y / 100
    [ log:triple ((var:x0 100) math:remainder        var:x3) ]   # c = Y mod 100
    …                                                           # (sequence continues)
    [ log:triple ((var:x31 31) math:integerQuotient  var:x32) ]  # Month
    [ log:triple ((var:x31 31) math:remainder        var:x33) ]  # (Day - 1)
    [ log:triple ((var:x33 1)  math:sum              var:x34) ]  # Day
) ].
````

The final pair `(var:x34 var:x32)` is thus `(Day Month)`.

---

## 🔍 Query pattern

We enumerate a block of consecutive years and ask for each Easter date.

```turtle
[ log:graph (
    [ log:triple (27 log:repeat var:x0) ]             # x0 = 0 … 26
    [ log:triple ((2024 var:x0) math:sum var:x1) ]    # Year = 2024 + x0
    [ log:triple (var:x1 :easter (var:xDay var:xMonth)) ]
) ] log:impliesAnswer [ log:graph (
    [ log:triple (var:x1 :easter (var:xDay var:xMonth)) ]
) ].
```

So the answer set lists Easter for **2024 through 2050** inclusive.

---

## ▶️ Run

```bash
eye --quiet --nope easter.ttl
```

Drop `--nope` to inspect every arithmetic step EYE performs for each year.

---

## 🗓️ Interpreting results

Each answer triple has the form:

```turtle
YYYY :easter (DD MM).
```

For example (illustrative formatting):

```turtle
2024 :easter (31 3).
2025 :easter (20 4).
…
```

Meaning Easter 2024 falls on **31 March**, Easter 2025 on **20 April**, etc.

---

## 🧪 Adjust the year range

Change the *repeat* count and base year:

```turtle
[ log:triple (10 log:repeat var:i) ]          # 0 … 9
[ log:triple ((2030 var:i) math:sum var:Year) ]
```

Now you’ll get 2030 … 2039.

---

## 🧩 Mapping to the usual symbols

| Classic symbol | Variable here | Description                   |
| -------------- | ------------- | ----------------------------- |
| a              | x1            | Year mod 19                   |
| b              | x2            | Century (Year / 100)          |
| c              | x3            | Year mod 100                  |
| d,e,…          | x4 … x30      | Intermediate arithmetic steps |
| Month          | x32           | 3 = March, 4 = April          |
| Day            | x34           | Final Easter day (1-based)    |

(Intermediate negations & sums implement “mod” style adjustments.)

---

## 💡 Tips

* To restrict to a *single* year, drop the `log:repeat` and just assert
  `:Y :easter (var:Day var:Month)` in a query rule.
* You can easily extend the rule to also compute related movable feasts
  (e.g. `:Y :goodFriday (DD MM)` by subtracting 2 days, using more arithmetic
  steps).

