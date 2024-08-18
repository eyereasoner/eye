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

:information_source: SAGE originates from [Drawing Conclusions from Linked Data on the Web: The EYE Reasoner by Ruben Verborgh and Jos De Roo](https://josd.github.io/Papers/EYE.pdf)
> In the future, reasoners could have a large impact on software. A few decades ago, software took over many jobs from hardware: calculations that used to be hardwired became soft-wired as lines of code. Although some highly optimized calculations still happen in hardware, most are now performed with special-purpose software on generic-purpose hardware. Similarly, reasoners will start rewiring software for specific situations. EYE already does this when performing rule-based Web service composition. In the end, this opens the door to automatically customized software processes. What software once did for hardware, reasoning might one day do for software.
