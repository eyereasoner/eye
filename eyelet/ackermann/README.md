# 🧮 Ackermann Function – Hyper-Recursive Computation in N3 Logic

This example models the **Ackermann function** using **Notation3 (N3)** and logic-based arithmetic. The Ackermann function is known for its **non-primitive recursive** nature and **extremely rapid growth**, making it a benchmark in **computability theory** and **complexity analysis**.

> 🔗 See also:  
> - [Ackermann function on Wikipedia](https://en.wikipedia.org/wiki/Ackermann_function)  
> - [EYE test case](https://github.com/eyereasoner/eye/blob/master/reasoning/ackermann/ackermann-answer.n3)

---

## 📘 Prefixes

```ttl
@prefix math: <http://www.w3.org/2000/10/swap/math#>.
@prefix log: <http://www.w3.org/2000/10/swap/log#>.
@prefix var: <http://www.w3.org/2000/10/swap/var#>.
@prefix : <http://example.org/#>.
````

---

## 🧾 Logic Rules (Recursive Definition)

### Base Case: `ackermann(0, y) = y + 1`

```ttl
((0 Y Z) :ackermann A) ⇐ A = Y + 1
```

---

### Next Cases (for small X):

| X  | Function Type   | Rule                   |
| -- | --------------- | ---------------------- |
| 1  | Addition        | `ack(1, y, z) = y + z` |
| 2  | Multiplication  | `ack(2, y, z) = y * z` |
| 3  | Exponentiation  | Hyperoperation: `y^z`  |
| 4+ | Tetration, etc. | Higher hyperoperations |

---

### General Case (Recursive Step for X > 0, Y > 0):

```ttl
ackermann(X, Y, Z) = ackermann(X-1, ackermann(X, Y-1, Z), Z)
```

---

### Edge Case:

```ttl
ackermann(X, 0, Z) = 1  # For X > 0
```

---

## 🧠 Logical Summary

The system defines the **Ackermann function** using logic rules with explicit arithmetic, allowing it to compute:

* **Successor**: A(0, y) → y + 1
* **Addition**: A(1, y, z) → y + z
* **Multiplication**: A(2, y, z) → y \* z
* **Exponentiation**: A(3, y, z) → y^z
* **And beyond...**

The higher values are defined **recursively** through controlled descent in the function’s parameters, modeled using `math:sum`, `math:product`, `math:difference`, and so on.

---

## ❓ Query: Sample Evaluations

This query asks the reasoner to compute:

| Ackermann(x, y) | Result Symbol            |
| --------------- | ------------------------ |
| A(0, 0)         | `A0`                     |
| A(0, 6)         | `A1`                     |
| A(1, 2)         | `A2`                     |
| A(1, 7)         | `A3`                     |
| A(2, 2)         | `A4`                     |
| A(2, 9)         | `A5`                     |
| A(3, 4)         | `A6`                     |
| A(3, 14)        | `A7`                     |
| A(4, 0)         | `A8` (2 digits)          |
| A(4, 1)         | `A9` (5 digits)          |
| A(4, 2)         | `A10` (\~20,000 digits!) |

These demonstrate the **explosive growth** of the Ackermann function.

---

## ✅ Conclusion

This N3 encoding:

* Represents **recursive** and **hyper-recursive** behavior using logic rules
* Computes symbolic Ackermann values through **arithmetic reasoning**
* Illustrates the **limits of primitive recursion**
* Showcases a powerful use of N3 for **computational expressiveness** beyond typical first-order logic

> ⚠️ Computing even `A(4,2)` results in a number with nearly **20,000 digits** – making it a serious stress test for any symbolic or numerical system!

