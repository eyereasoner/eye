% Using Skolem function

person(peter).
person(bob).

likes(P,skolem(P)) :-
    person(P).

% query
query(likes(_X,_Y)).
