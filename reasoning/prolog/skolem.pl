% Using Skolem function

person(peter).
person(bob).

likes(P,skolem(P)) :-
    person(P).

% query implies goal
likes(_X,_Y) -: goal.
