% See https://en.wikipedia.org/wiki/Complex_number

exponentiation([A,B],[C,D],[E,F]) :-
    polar([A,B],[R,T]),
    E is R^C*exp(-D*T)*cos(D*log(R)+C*T),
    F is R^C*exp(-D*T)*sin(D*log(R)+C*T).

polar([X,Y],[R,Tp]) :-
    R is sqrt(X**2+Y**2),
    T is acos(abs(X)/R),
    angular(X,Y,T,Tp).

angular(X,Y,T,Tp) :-
    X >= 0,
    Y >= 0,
    Tp = T.
angular(X,Y,T,Tp) :-
    X < 0,
    Y >= 0,
    Tp is pi-T.
angular(X,Y,T,Tp) :-
    X < 0,
    Y < 0,
    Tp is T+pi.
angular(X,Y,T,Tp) :-
    X >= 0,
    Y < 0,
    Tp is 2*pi-T.

minus([X1,X2],[Y1,Y2]) :-
    Y1 is -X1,
    Y2 is -X2.

minus([X1,X2],[Y1,Y2],[Z1,Z2]) :-
    Z1 is X1-Y1,
    Z2 is X2-Y2.

add([X1,X2],[Y1,Y2],[Z1,Z2]) :-
    Z1 is X1+Y1,
    Z2 is X2+Y2.

times([X1,X2],[Y1,Y2],[Z1,Z2]) :-
    Z1 is X1*Y1-X2*Y2,
    Z2 is X1*Y2+X2*Y1.

inverse([X1,X2],[Y1,Y2]) :-
    Y1 is X1/(X1**2+X2**2),
    Y2 is -X2/(X1**2+X2**2).

divide(X,Y,Z) :-
    inverse(Y,Yp),
    times(X,Yp,Z).

% test cases
case(exponentiation([-1,0],[0.5,0],_)).
case(exponentiation([e,0],[0,pi],_)).

test :-
    case(A),
    A,
    write(A),
    write('.'),
    nl,
    fail.
test :-
    halt.
