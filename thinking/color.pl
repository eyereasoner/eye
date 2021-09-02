% See https://en.wikipedia.org/wiki/Four_color_theorem

:- use_module(library(lists)).

webize(eye/1,'https://josd.github.io/eye/thinking/ns#').

eye(colors('./color.pl',Places)) :-
    findall(Place-_,eye(neighbours(Place,_)),Places),
    places(Places).

places([]).
places([Place-Color|Tail]) :-
    places(Tail),
    eye(neighbours(Place,Neighbours)),
    member(Color,[eye(c1),eye(c2),eye(c3),eye(c4)]),
    \+ (member(Neighbour-Color,Tail),member(Neighbour,Neighbours)),
    !.

% test data
eye(neighbours(eye(p1),[eye(p2),eye(p5),eye(p4),eye(p3)])).
eye(neighbours(eye(p2),[eye(p1),eye(p4),eye(p3)])).
eye(neighbours(eye(p3),[eye(p5),eye(p1),eye(p4),eye(p2)])).
eye(neighbours(eye(p4),[eye(p1),eye(p2),eye(p3)])).
eye(neighbours(eye(p5),[eye(p1),eye(p3)])).

% test cases
case(webize(_NS,_P)).
case(eye(colors(_SCOPE,_PLACES))).

test :-
    case(A),
    A,
    writeq(A),
    write('.\n'),
    fail.
test :-
    halt.
