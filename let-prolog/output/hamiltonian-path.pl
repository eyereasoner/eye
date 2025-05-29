:- op(1200, xfx, :+).

answer(hamiltonianPath([2, 1, 7, 5, 8, 6, 4, 3])).
answer(hamiltonianPath([2, 7, 5, 8, 6, 4, 3, 1])).

step((true:+hamiltonianPath(_)), hamiltonianPath([2, 1, 7, 5, 8, 6, 4, 3]), true).
step((true:+hamiltonianPath(_)), hamiltonianPath([2, 7, 5, 8, 6, 4, 3, 1]), true).
