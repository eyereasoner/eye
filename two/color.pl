% See https://en.wikipedia.org/wiki/Four_color_theorem

:- use_module(library(lists)).

web_nsp(etc_,'http://josd.github.io/eye/two/cases#').

etc_colors(Places) :-
    findall(Place-_,etc_neighbours(Place,_),Places),
    etc_places(Places).

etc_places([]).
etc_places([Place-Color|Tail]) :-
    etc_places(Tail),
    etc_neighbours(Place,Neighbours),
    member(Color,[etc_c1,etc_c2,etc_c3,etc_c4]),
    \+ (member(Neighbour-Color,Tail),member(Neighbour,Neighbours)).

% test data
etc_neighbours(etc_p1,[etc_p2,etc_p5,etc_p4,etc_p3]).
etc_neighbours(etc_p2,[etc_p1,etc_p4,etc_p3]).
etc_neighbours(etc_p3,[etc_p5,etc_p1,etc_p4,etc_p2]).
etc_neighbours(etc_p4,[etc_p1,etc_p2,etc_p3]).
etc_neighbours(etc_p5,[etc_p1,etc_p3]).

% test cases
case(etc_colors(_X)).

test :-
    case(A),
    A,
    write(A),
    write('.\n'),
    fail.
test :-
    halt.
