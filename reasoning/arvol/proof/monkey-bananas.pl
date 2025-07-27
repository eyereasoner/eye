plan(1,5,4,[go(loc3),push(loc1),climb_on,grab]).
plan(1,5,5,[go(loc1),go(loc3),push(loc1),climb_on,grab]).
plan(1,5,5,[go(loc3),push(loc1),climb_on,grab,climb_off]).
plan(1,5,5,[go(loc3),push(loc2),push(loc1),climb_on,grab]).

step(rule(plan(1, 5, A, B), answer(plan(1, 5, A, B))), plan(1, 5, 4, [go(loc3), push(loc1), climb_on, grab]), answer(plan(1, 5, 4, [go(loc3), push(loc1), climb_on, grab]))).
step(rule(plan(1, 5, A, B), answer(plan(1, 5, A, B))), plan(1, 5, 5, [go(loc1), go(loc3), push(loc1), climb_on, grab]), answer(plan(1, 5, 5, [go(loc1), go(loc3), push(loc1), climb_on, grab]))).
step(rule(plan(1, 5, A, B), answer(plan(1, 5, A, B))), plan(1, 5, 5, [go(loc3), push(loc1), climb_on, grab, climb_off]), answer(plan(1, 5, 5, [go(loc3), push(loc1), climb_on, grab, climb_off]))).
step(rule(plan(1, 5, A, B), answer(plan(1, 5, A, B))), plan(1, 5, 5, [go(loc3), push(loc2), push(loc1), climb_on, grab]), answer(plan(1, 5, 5, [go(loc3), push(loc2), push(loc1), climb_on, grab]))).
