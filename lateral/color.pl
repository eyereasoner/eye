% See https://en.wikipedia.org/wiki/Four_color_theorem

:- use_module(library(lists)).

colors(Places) :-
    findall(Place-_,neighbours(Place,_),Places),
    places(Places).

places([]).
places([Place-Color|Tail]) :-
    places(Tail),
    neighbours(Place,Neighbours),
    member(Color,[c1,c2,c3,c4]),
    \+ (member(Neighbour-Color,Tail),member(Neighbour,Neighbours)).

% test data
neighbours(p1,[p2,p5,p4,p3]).
neighbours(p2,[p1,p4,p3]).
neighbours(p3,[p5,p1,p4,p2]).
neighbours(p4,[p1,p2,p3]).
neighbours(p5,[p1,p3]).

% test cases
case(once(colors(_X))).

test :-
    case(A),
    A,
    writeq(A),
    write('.\n'),
    fail.
test :-
    halt.
