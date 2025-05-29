:- op(1200, xfx, :+).

answer(believes('Fabian', (notNecessarilyA(A, gold):+glitter(A)))).
answer(notNecessarilyA(northStar, gold)).

step((A:+says('Einstein', A)), says('Einstein', (notNecessarilyA(B, gold):+glitter(B))), (notNecessarilyA(B, gold):+glitter(B))).
step((true:+believes('Fabian', _)), believes('Fabian', (notNecessarilyA(A, gold):+glitter(A))), true).
step((notNecessarilyA(A, gold):+glitter(A)), glitter(northStar), notNecessarilyA(northStar, gold)).
step((true:+notNecessarilyA(_, _)), notNecessarilyA(northStar, gold), true).
