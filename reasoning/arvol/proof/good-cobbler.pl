ascribed(some0,good(cobbler)).

step(rule(true, ascribed(_, good(cobbler))), true, ascribed(some0, good(cobbler))).
step(rule(ascribed(A, good(B)), answer(ascribed(A, good(B)))), ascribed(some0, good(cobbler)), answer(ascribed(some0, good(cobbler)))).
