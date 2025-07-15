# eyelet

## another eye looking at eye test cases

| file / folder       | role                    | notes                                                                                                 |
| ------------------- | ----------------------- | ----------------------------------------------------------------------------------------------------- |
| `eyelet.py`         | 🧠 *engine*             | Generic SOS resolution prover.  Reads KBs from text files whose syntax is described below.            |
| `input/*.txt`       | 📄 *knowledge‑bases*    | One KB per file.  Each ends with `GOAL:` specifying the literal to prove.  *Add as many as you like!* |
| `output/*.txt`      | 📄 *proof-explanations* | a breadth‑first derivation.  The last line `⊥` means empty clause derived so goal is entailed.        |

---

## 1  Prerequisites

* Python ≥ 3.8 (no external packages required)

```bash
python --version
```

---

## 2  Running a proof

```bash
python eyelet.py <your‑kb>.txt
```

### Example

```bash
$ python eyelet.py beetle12.txt

Knowledge base loaded from 'beetle12.txt'.  Goal: Beautiful(beetle)

01. ¬Blue(beetle) | Beautiful(beetle)            (from ¬Beautiful(beetle) , ¬Blue(x) | Beautiful(x))
02. Beautiful(beetle)                            (from 01 , Car(beetle) | Green(beetle) | Blue(beetle))
...
09. ⊥                                            (from Beautiful(beetle) , ¬Beautiful(beetle))

Empty clause derived — goal is entailed. 🎉
```

**What you’re seeing**: a breadth‑first derivation.  The last line `⊥`
(empty clause) means the negated goal led to contradiction ⇒ the original goal is proved.

---

## 3  KB file format

* **One clause per line.**  Literals separated by `|` (logical OR).
* Negated literals start with `¬` or `~`.
* **Variables** = identifiers whose first character is lowercase.  (All
  variables are implicitly ∀‑quantified.)
* **Comments**: any line that begins with `#` is ignored.
* **Blank lines** are ignored.
* The **last non‑comment line** must be

  ```
  GOAL: <single‑literal>
  ```

  The prover automatically negates this literal internally.

### Example

```text
# Toy knowledge base
¬Human(x) | Mortal(x)
Human(Socrates)

GOAL: Mortal(Socrates)
```

---

## 4  Adding new knowledge‑bases

1. Create a new `my_problem.txt` following the syntax above.
2. Run

   ```bash
   python eyelet.py my_problem.txt
   ```
3. Profit 🙂

Tips:

* Keep clauses short.
* For branching rules use a **single** clause with multiple positive literals,
  e.g. `¬Bird(x) | Flies(x) | Swims(x)`.
* If the derivation seems to loop forever your KB may not be SOS‑compatible.

---

## 5  How it works (very briefly)

* **Set‑of‑support (SOS)**: only clauses that depend on the negated goal are
  used to generate new resolvents, keeping the search finite for many
  practical examples.
* **Binary resolution**: pick complementary literals, unify, merge the rest.
* **Unification**: Martelli‑Montanari with occurs‑check.

---

## 6 Run all examples and test cases

```
./test
```

---

## 7  Limitations & future ideas

* No term indexing; large KBs will be slow.
* No support for equality or function symbols with special semantics.
* No redundancy elimination beyond tautology checking.

