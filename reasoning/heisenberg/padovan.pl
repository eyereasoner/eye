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

% query
query(padovan(1,_ANSWER)).
query(padovan(2,_ANSWER)).
query(padovan(3,_ANSWER)).
query(padovan(4,_ANSWER)).
query(padovan(5,_ANSWER)).
query(padovan(6,_ANSWER)).
query(padovan(7,_ANSWER)).
query(padovan(8,_ANSWER)).
query(padovan(9,_ANSWER)).
query(padovan(10,_ANSWER)).
query(padovan(11,_ANSWER)).
query(padovan(12,_ANSWER)).
query(padovan(13,_ANSWER)).
query(padovan(14,_ANSWER)).
query(padovan(15,_ANSWER)).
query(padovan(16,_ANSWER)).
query(padovan(17,_ANSWER)).
query(padovan(18,_ANSWER)).
query(padovan(19,_ANSWER)).
query(padovan(20,_ANSWER)).
query(padovan(91,_ANSWER)).
query(padovan(283,_ANSWER)).
query(padovan(3674,_ANSWER)).
query(plastic_ratio(1,_ANSWER)).
query(plastic_ratio(10,_ANSWER)).
query(plastic_ratio(100,_ANSWER)).
query(plastic_ratio(1000,_ANSWER)).
