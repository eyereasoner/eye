# Special-purpose Algorithms on Generic-purpose Engines - SAGE

:abacus: __Algorithms are constructed using logical rules and queries, with backward rules (including built-ins) providing control.__

- queries use an upwards double arrow `=^`
- backward rules use a leftwards double arrow `<=`
- forward rules use a rightwards double arrow `=>`

:warning: __variables__

- only `_:x` blank nodes in triples and quads
- only `?x` quickvars in rules and queries
- quantified `var:x` variables in proofs

:globe_with_meridians: __examples and test cases__

- bayesian networks:
    [ccd](https://github.com/eyereasoner/eye/tree/master/reasoning/ccd),
    [nbbn](https://github.com/eyereasoner/eye/tree/master/reasoning/nbbn),
    [swet](https://github.com/eyereasoner/eye/tree/master/reasoning/swet)
- control systems:
    [acp](https://github.com/eyereasoner/eye/tree/master/reasoning/acp),
    [cs](https://github.com/eyereasoner/eye/tree/master/reasoning/cs),
    [steps](https://github.com/eyereasoner/eye/tree/master/reasoning/steps)
- description logic: 
    [bmt](https://github.com/eyereasoner/eye/tree/master/reasoning/bmt),
    [dt](https://github.com/eyereasoner/eye/tree/master/reasoning/dt),
    [edt](https://github.com/eyereasoner/eye/tree/master/reasoning/edt),
    [entail](https://github.com/eyereasoner/eye/tree/master/reasoning/entail),
    [gedcom](https://github.com/eyereasoner/eye/tree/master/reasoning/gedcom),
    [h2o](https://github.com/eyereasoner/eye/tree/master/reasoning/h2o),
    [RDF plus OWL](https://github.com/eyereasoner/eye/tree/master/reasoning/rpo)
- ershovian compilation:
    [preduction](https://github.com/eyereasoner/eye/tree/master/reasoning/preduction)
- extensible imaging:
    [lldm](https://github.com/eyereasoner/eye/tree/master/reasoning/lldm)
- graph computation:
    [graph](https://github.com/eyereasoner/eye/tree/master/reasoning/graph),
    [path-discovery](https://github.com/eyereasoner/eye/tree/master/reasoning/path-discovery)
- logic programming:
    [4color](https://github.com/eyereasoner/eye/tree/master/reasoning/4color),
    [beetle](https://github.com/eyereasoner/eye/tree/master/reasoning/beetle),
    [dp](https://github.com/eyereasoner/eye/tree/master/reasoning/dp),
    [diamond-property](https://github.com/eyereasoner/eye/tree/master/reasoning/diamond-property),
    [gcc](https://github.com/eyereasoner/eye/tree/master/reasoning/gcc),
    [hanoi](https://github.com/eyereasoner/eye/tree/master/reasoning/hanoi),
    [lee](https://github.com/eyereasoner/eye/tree/master/reasoning/lee),
    [n-queens](https://github.com/eyereasoner/eye/tree/master/reasoning/n-queens),
    [socrates](https://github.com/eyereasoner/eye/tree/master/reasoning/socrates),
    [witch](https://github.com/eyereasoner/eye/tree/master/reasoning/witch),
    [zebra](https://github.com/eyereasoner/eye/tree/master/reasoning/zebra)
- markovian networks:
    [mmln](https://github.com/eyereasoner/eye/tree/master/reasoning/mmln)
- mathematical reasoning:
    [ackermann](https://github.com/eyereasoner/eye/tree/master/reasoning/ackermann),
    [eulers-identity](https://github.com/eyereasoner/eye/tree/master/reasoning/eulers-identity),
    [fibonacci](https://github.com/eyereasoner/eye/tree/master/reasoning/fibonacci),
    [padovan](https://github.com/eyereasoner/eye/tree/master/reasoning/padovan),
    [peano](https://github.com/eyereasoner/eye/tree/master/reasoning/peano),
    [peasant-multiplication](https://github.com/eyereasoner/eye/tree/master/reasoning/peasant-multiplication),
    [peasant-power](https://github.com/eyereasoner/eye/tree/master/reasoning/peasant-power),
    [pi](https://github.com/eyereasoner/eye/tree/master/reasoning/pi),
    [polygon](https://github.com/eyereasoner/eye/tree/master/reasoning/polygon),
    [tak](https://github.com/eyereasoner/eye/tree/master/reasoning/tak)
- neural networks:
    [fcm](https://github.com/eyereasoner/eye/tree/master/reasoning/fcm),
    [fgcm](https://github.com/eyereasoner/eye/tree/master/reasoning/fgcm)
- quantum computation:
    [dqc](https://github.com/eyereasoner/eye/tree/master/reasoning/dqc)
- universal machines:
    [turing](https://github.com/eyereasoner/eye/tree/master/reasoning/turing),
    [usm](https://github.com/eyereasoner/eye/tree/master/reasoning/usm)
- workflow composers:
    [gps](https://github.com/eyereasoner/eye/tree/master/reasoning/gps),
    [map](https://github.com/eyereasoner/eye/tree/master/reasoning/map),
    [resto](https://github.com/eyereasoner/eye/tree/master/reasoning/resto),
    [restpath](https://github.com/eyereasoner/eye/tree/master/reasoning/restpath),
    [twf](https://github.com/eyereasoner/eye/tree/master/reasoning/twf)

:red_triangle: __The arrows form a triptych like in__

```
        eye                 observership
         u                   /        \
        =^                  /          \
   l <= eye => r        origin ------ evolution

Euler lost both eyes    Hawking-Hertog triptych
```

:bulb: __SAGE originates from__ [Drawing Conclusions from Linked Data on the Web: The EYE Reasoner by Ruben Verborgh and Jos De Roo](https://josd.github.io/Papers/EYE.pdf)

> In the future, reasoners could have a large impact on software. A few decades ago, software took over many jobs from hardware: calculations that used to be hardwired became soft-wired as lines of code. Although some highly optimized calculations still happen in hardware, most are now performed with special-purpose software on generic-purpose hardware. Similarly, reasoners will start rewiring software for specific situations. EYE already does this when performing rule-based Web service composition. In the end, this opens the door to automatically customized software processes. What software once did for hardware, reasoning might one day do for software.
