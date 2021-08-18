% See https://en.wikipedia.org/wiki/Complex_number

nsp(etc_,'http://josd.github.io/eye/two/cases#').

etc_exp([A,B],[C,D],[E,F]) :-
    etc_polar([A,B],[R,T]),
    E is R^C*exp(-D*T)*cos(D*log(R)+C*T),
    F is R^C*exp(-D*T)*sin(D*log(R)+C*T).

etc_polar([X,Y],[R,Tp]) :-
    R is sqrt(X**2+Y**2),
    T is acos(abs(X)/R),
    etc_angular(X,Y,T,Tp).

etc_angular(X,Y,T,Tp) :-
    X >= 0,
    Y >= 0,
    Tp = T.
etc_angular(X,Y,T,Tp) :-
    X < 0,
    Y >= 0,
    Tp is pi-T.
etc_angular(X,Y,T,Tp) :-
    X < 0,
    Y < 0,
    Tp is T+pi.
etc_angular(X,Y,T,Tp) :-
    X >= 0,
    Y < 0,
    Tp is 2*pi-T.

etc_minus([X1,X2],[Y1,Y2]) :-
    Y1 is -X1,
    Y2 is -X2.

etc_minus([X1,X2],[Y1,Y2],[Z1,Z2]) :-
    Z1 is X1-Y1,
    Z2 is X2-Y2.

etc_add([X1,X2],[Y1,Y2],[Z1,Z2]) :-
    Z1 is X1+Y1,
    Z2 is X2+Y2.

etc_times([X1,X2],[Y1,Y2],[Z1,Z2]) :-
    Z1 is X1*Y1-X2*Y2,
    Z2 is X1*Y2+X2*Y1.

etc_inverse([X1,X2],[Y1,Y2]) :-
    Y1 is X1/(X1**2+X2**2),
    Y2 is -X2/(X1**2+X2**2).

etc_divide(X,Y,Z) :-
    inverse(Y,Yp),
    etc_times(X,Yp,Z).

% test cases
case(etc_exp([-1,0],[0.5,0],_ANSWER)).
case(etc_exp([e,0],[0,pi],_ANSWER)).

test :-
    case(A),
    A,
    write(A),
    write('.\n'),
    fail.
test :-
    halt.
