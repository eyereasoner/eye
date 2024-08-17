# Delta

:abacus: Algorithms are constructed using logical rules and queries, with backward rules (including built-ins) providing control.

- queries use an upwards double arrow `=^`
- backward rules use a leftwards double arrow `<=`
- forward rules use a rightwards double arrow `=>`

:cheese: variables

- only `_:x` blank nodes in triples and quads
- only `?x` quickvars in rules and queries
- quantified `var:x` variables in proofs

:information_source: The name delta is inspired by these 2 deltas:
```
--------------------    -----------------------
Euler lost both eyes    Hawking Hertog triptych
--------------------    -----------------------

        eye                  observership
         u                    /        \
        =^                   /          \
   l <= eye => r        origin ------- evolution
```
