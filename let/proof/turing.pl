compute([1,0,1,0,0,1],[1,0,1,0,1,0,#]).
compute([1,0,1,1,1,1],[1,1,0,0,0,0,#]).
compute([1,1,1,1,1,1],[1,0,0,0,0,0,0,#]).
compute([],[1,#]).

step(rule(compute([1, 0, 1, 0, 0, 1], A), answer(compute([1, 0, 1, 0, 0, 1], A))), compute([1, 0, 1, 0, 0, 1], [1, 0, 1, 0, 1, 0, #]), answer(compute([1, 0, 1, 0, 0, 1], [1, 0, 1, 0, 1, 0, #]))).
step(rule(compute([1, 0, 1, 1, 1, 1], A), answer(compute([1, 0, 1, 1, 1, 1], A))), compute([1, 0, 1, 1, 1, 1], [1, 1, 0, 0, 0, 0, #]), answer(compute([1, 0, 1, 1, 1, 1], [1, 1, 0, 0, 0, 0, #]))).
step(rule(compute([1, 1, 1, 1, 1, 1], A), answer(compute([1, 1, 1, 1, 1, 1], A))), compute([1, 1, 1, 1, 1, 1], [1, 0, 0, 0, 0, 0, 0, #]), answer(compute([1, 1, 1, 1, 1, 1], [1, 0, 0, 0, 0, 0, 0, #]))).
step(rule(compute([], A), answer(compute([], A))), compute([], [1, #]), answer(compute([], [1, #]))).
