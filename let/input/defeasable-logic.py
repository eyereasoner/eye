"""
defeasible-logic.py – A tiny defeasible‑logic vignette in ProbLog
==================================================================

Classical defeasible­‑logic story: **Birds normally fly** but **penguins
are birds that (normally) do not fly**.  The rule for penguins *defeats*
(bars) the general bird‑flying default.

Encoding technique
------------------
* Strict facts: `bird(tweety).`, `penguin(polly).`
* Subclass: `bird(X) :- penguin(X).`
* **Default rule** with probability 0.9:

```prolog
0.9::flies(X) :- bird(X), \+ abnormal(X).
```

* **Defeater**: being a penguin makes the bird *abnormal* w.r.t. flying.

```prolog
abnormal(X) :- penguin(X).
```

Thus Tweety flies with 90 % probability, Polly does not fly (probability
0) unless another rule re‑establishes flight.

Run
---
```bash
pip install problog
python defeasible-logic.py
```
Expected:
```
flies(tweety): 0.9
flies(polly): 0.0
abnormal(polly): 1.0
```

You can experiment by adding `0.2::flies(X) :- penguin(X).` to model
rare flying penguins and observe non‑monotonic interplay.
"""
from problog.program import PrologString
from problog import get_evaluatable

problog_code = """
% --- base facts -----------------------------------------------------------
bird(tweety).
penguin(polly).

% --- hierarchy ------------------------------------------------------------
bird(X) :- penguin(X).

% --- defeasible default: birds normally fly ------------------------------
0.9::flies(X) :- bird(X), \+ abnormal(X).

% --- defeating rule: penguins are abnormal wrt flying --------------------
abnormal(X) :- penguin(X).

% (optionally we could add rare flying penguins)
% 0.01::flies(X) :- penguin(X).

% --- queries -------------------------------------------------------------
query(flies(tweety)).
query(flies(polly)).
query(abnormal(polly)).
"""

model = PrologString(problog_code)
results = get_evaluatable().create_from(model).evaluate()
for atom, prob in results.items():
    print(f"{atom}: {prob}")
