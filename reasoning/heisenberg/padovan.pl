% See https://en.wikipedia.org/wiki/Padovan_sequence

padovan(A,B) :-
    padovan(A,0,1,1,B).

padovan(0,A,_,_,A).
padovan(1,_,A,_,A).
padovan(2,_,_,A,A).
padovan(A,B,C,D,E) :-
    A > 2,
    F is A-1,
    G is B+C,
    padovan(F,C,D,G,E).

plastic_ratio(A,B) :-
    padovan(A,C),
    D is A+1,
    padovan(D,E),
    B is E/C.

% query implies goal
padovan(1,_ANSWER) => goal.
padovan(2,_ANSWER) => goal.
padovan(3,_ANSWER) => goal.
padovan(4,_ANSWER) => goal.
padovan(5,_ANSWER) => goal.
padovan(6,_ANSWER) => goal.
padovan(7,_ANSWER) => goal.
padovan(8,_ANSWER) => goal.
padovan(9,_ANSWER) => goal.
padovan(10,_ANSWER) => goal.
padovan(11,_ANSWER) => goal.
padovan(12,_ANSWER) => goal.
padovan(13,_ANSWER) => goal.
padovan(14,_ANSWER) => goal.
padovan(15,_ANSWER) => goal.
padovan(16,_ANSWER) => goal.
padovan(17,_ANSWER) => goal.
padovan(18,_ANSWER) => goal.
padovan(19,_ANSWER) => goal.
padovan(20,_ANSWER) => goal.
padovan(91,_ANSWER) => goal.
padovan(283,_ANSWER) => goal.
padovan(3674,_ANSWER) => goal.
plastic_ratio(1,_ANSWER) => goal.
plastic_ratio(10,_ANSWER) => goal.
plastic_ratio(100,_ANSWER) => goal.
plastic_ratio(1000,_ANSWER) => goal.
