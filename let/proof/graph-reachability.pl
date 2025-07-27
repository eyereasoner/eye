isReachable(a,f).
\+isReachable(b,e).
isReachable(c,g).

step(rule(isReachable(a, f), answer(isReachable(a, f))), isReachable(a, f), answer(isReachable(a, f))).
step(rule(\+isReachable(b, e), answer(\+isReachable(b, e))), \+isReachable(b, e), answer(\+isReachable(b, e))).
step(rule(isReachable(c, g), answer(isReachable(c, g))), isReachable(c, g), answer(isReachable(c, g))).
