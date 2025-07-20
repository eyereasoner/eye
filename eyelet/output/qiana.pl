:- op(1200, xfx, :+).

answer(believes('Fabian',(notNecessarilyA(A,gold):+glitter(A)))).
answer(notNecessarilyA(northStar,gold)).

step((A:+says('Einstein',A)),says('Einstein',(notNecessarilyA(B,gold):+glitter(B))),(notNecessarilyA(B,gold):+glitter(B))).
step((true:+believes('Fabian',A)),believes('Fabian',(notNecessarilyA(B,gold):+glitter(B))),true).
step((notNecessarilyA(A,gold):+glitter(A)),glitter(northStar),notNecessarilyA(northStar,gold)).
step((true:+notNecessarilyA(A,B)),notNecessarilyA(northStar,gold),true).
