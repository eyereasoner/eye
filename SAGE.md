# Special-purpose Algorithms on Generic-purpose Engines - SAGE

:abacus: Algorithms are constructed using logical rules and queries, with backward rules (including built-ins) providing control.

- queries use an upwards double arrow `=^`
- backward rules use a leftwards double arrow `<=`
- forward rules use a rightwards double arrow `=>`

:warning: variables

- only `_:x` blank nodes in triples and quads
- only `?x` quickvars in rules and queries
- quantified `var:x` variables in proofs

:information_source: The arrows form a triptych like in
```
        eye                  observership
         u                    /        \
        =^                   /          \
   l <= eye => r        origin ------- evolution

Euler lost both eyes    Hawking-Hertog triptych
```
