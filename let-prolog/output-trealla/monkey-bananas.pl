:- op(1200, xfx, :+).

answer(plan([go(loc3),push(loc1),climb_on,grab])).
answer(plan([go(loc1),go(loc3),push(loc1),climb_on,grab])).
answer(plan([go(loc3),push(loc1),climb_on,grab,climb_off])).
answer(plan([go(loc3),push(loc2),push(loc1),climb_on,grab])).

step((true:+plan([A,B,C,D])),plan([go(loc3),push(loc1),climb_on,grab]),true).
step((true:+plan([A,B,C,D,E])),plan([go(loc1),go(loc3),push(loc1),climb_on,grab]),true).
step((true:+plan([A,B,C,D,E])),plan([go(loc3),push(loc1),climb_on,grab,climb_off]),true).
step((true:+plan([A,B,C,D,E])),plan([go(loc3),push(loc2),push(loc1),climb_on,grab]),true).
