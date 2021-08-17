% Calculate pi using Nilakantha series
% See http://www.wikihow.com/Calculate-Pi

web_nsp(etc_,'http://josd.github.io/eye/two/cases#').

etc_pi(A,B) :-
    etc_pi(1,A,0,C,1),
    B is 3+4*C.

etc_pi(A,A,B,B,_) :-
    !.
etc_pi(A,B,C,D,E) :-
    F is A+1,
    L is C+E/(2*A*(2*A+1)*(2*A+2)),
    M is -E,
    etc_pi(F,B,L,D,M).

% test cases
case(etc_pi(100000,_ANSWER)).

test :-
    case(A),
    A,
    write(A),
    write('.\n'),
    fail.
test :-
    halt.
