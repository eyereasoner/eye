% Calculating the area of a polygon

web_nsp(etc_,'http://josd.github.io/eye/two/cases#').

etc_area([_],0).
etc_area([[A,B],[C,D]|E],F) :-
    etc_area([[C,D]|E],G),
    F is (A*D-B*C)/2+G.

% test cases
case(etc_area([[3,2],[6,2],[7,6],[4,6],[5,5],[5,3],[3,2]],_ANSWER)).

test :-
    case(A),
    A,
    write(A),
    write('.\n'),
    fail.
test :-
    halt.
