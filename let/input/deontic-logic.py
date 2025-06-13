"""
deontic.py – A tiny deontic‑logic scenario in ProbLog
======================================================

Scenario
--------
A driver approaches an intersection with a traffic light.

* **Norms** (obligations)
    * When the light is **red** you *ought* to **stop**.
    * When the light is **green** you *ought* to **go**.
* **Behaviour** (probabilistic choices)
    * The light is red or green with equal likelihood (50 % each).
    * Faced with a **red** light the driver chooses:
        * 0.8 probability to **stop** (comply)
        * 0.2 probability to **go**  (violate)
    * Faced with a **green** light the driver chooses:
        * 0.9 probability to **go**   (comply)
        * 0.1 probability to **stop** (over‑cautious but not a violation)

* **Violation rule**
    * A violation occurs when an action that is *obligated* is **not** taken.

We ask ProbLog for the probabilities of:

* the driver stopping, going,
* the light’s colour,
* a **violation** of the traffic rule.

Run
----
```bash
pip install problog
python deontic.py
```
Expected output:
```
violation: 0.15
light(red): 0.5
light(green): 0.5
action(stop): 0.45
action(go): 0.55
```

This captures a simple *ought‑to‑do* deontic logic in a probabilistic
setting.
"""
from problog.program import PrologString
from problog import get_evaluatable

problog_code = """
% ---------- light colour -------------------------------------------------
0.5::light(red); 0.5::light(green).

% ---------- obligations --------------------------------------------------
oblig(stop) :- light(red).
oblig(go)   :- light(green).

% ---------- driver behaviour --------------------------------------------
0.8::action(stop); 0.2::action(go) :- light(red).
0.1::action(stop); 0.9::action(go) :- light(green).

% ---------- violation detection -----------------------------------------
violation :- oblig(A), \+ action(A).

% ---------- queries ------------------------------------------------------
query(violation).
query(light(red)).
query(light(green)).
query(action(stop)).
query(action(go)).
"""

model = PrologString(problog_code)
results = get_evaluatable().create_from(model).evaluate()

for atom, prob in results.items():
    print(f"{atom}: {prob}")
