findRoot([1,1.0,1.0e-15],1.4142135623730951) .
findRoot([2,2.0,1.0e-15],2.7182818284590455) .
findRoot([3,3.0,1.0e-15],3.141592653589793) .

step(rule(findRoot([1, 1.0, 1.0e-15], A), answer(findRoot([1, 1.0, 1.0e-15], A))), findRoot([1, 1.0, 1.0e-15], 1.4142135623730951), answer(findRoot([1, 1.0, 1.0e-15], 1.4142135623730951))).
step(rule(findRoot([2, 2.0, 1.0e-15], A), answer(findRoot([2, 2.0, 1.0e-15], A))), findRoot([2, 2.0, 1.0e-15], 2.7182818284590455), answer(findRoot([2, 2.0, 1.0e-15], 2.7182818284590455))).
step(rule(findRoot([3, 3.0, 1.0e-15], A), answer(findRoot([3, 3.0, 1.0e-15], A))), findRoot([3, 3.0, 1.0e-15], 3.141592653589793), answer(findRoot([3, 3.0, 1.0e-15], 3.141592653589793))).
