:- op(1200, xfx, :+).

answer(move(14, [left, centre, right])).

step((true:+move(14, [left, centre, right])), move(14, [left, centre, right]), true).
