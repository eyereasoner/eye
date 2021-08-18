% Lee routing for VLSI circuits
% Code from the book "The Art of Prolog" Chapter 16

:- use_module(library(lists)).

nsp(etc_,'http://josd.github.io/eye/two/cases#').

etc_route(Source,Destination,Obstacles,Path) :-
    etc_waves(Destination,[[Source],[]],Obstacles,Waves),
    etc_path(Source,Destination,Waves,P),
    reverse(P,Path).

% etc_waves(Destination,Wavessofar,Obstacles,Waves)
etc_waves(B,[Wave|Waves],_,Waves) :-
    member(B,Wave),
    !.
etc_waves(B,[Wave,LastWave|LastWaves],Obstacles,Waves) :-
    etc_next_wave(Wave,LastWave,Obstacles,NextWave),
    etc_waves(B,[NextWave,Wave,LastWave|LastWaves],Obstacles,Waves).

% etc_next_waves(Wave,LastWave,Obstacles,NextWave)
etc_next_wave(Wave,LastWave,Obstacles,NextWave) :-
    setof(X,etc_admissible(X,Wave,LastWave,Obstacles),NextWave).

etc_admissible(X,Wave,LastWave,Obstacles) :-
    etc_adjacent(X,Wave,Obstacles),
    \+ member(X,LastWave),
    \+ member(X,Wave).

etc_adjacent(X,Wave,Obstacles) :-
    member(X1,Wave),
    etc_neighbor(X1,X),
    \+ etc_obstructed(X,Obstacles).

etc_neighbor([X1,Y],[X2,Y]) :-
    etc_next_to(X1,X2).
etc_neighbor([X,Y1],[X,Y2]) :-
    etc_next_to(Y1,Y2).

etc_next_to(X,X1) :-
    X1 is X+1.
etc_next_to(X,X1) :-
    X > 0,
    X1 is X-1.

etc_obstructed(Point,Obstacles) :-
    member(Obstacle,Obstacles),
    etc_obstructs(Point,Obstacle).

etc_obstructs([X,Y],[[X,Y1],[_,Y2]]) :-
    Y1 =< Y,
    Y =< Y2.
etc_obstructs([X,Y],[[_,Y1],[X,Y2]]) :-
    Y1 =< Y,
    Y =< Y2.
etc_obstructs([X,Y],[[X1,Y],[X2,_]]) :-
    X1 =< X,
    X =< X2.
etc_obstructs([X,Y],[[X1,_],[X2,Y]]) :-
    X1 =< X,
    X =< X2.

% path(Source,Destination,Waves,Path)
etc_path(A,A,_,[A]) :-
    !.
etc_path(A,B,[Wave|Waves],[B|Path]) :-
    member(B1,Wave),
    etc_neighbor(B,B1),
    !,
    etc_path(A,B1,Waves,Path).

% test cases
case(etc_route([1,1],[9,8],[[[2,3],[4,5]],[[6,6],[8,8]]],_ANSWER)).

test :-
    case(A),
    A,
    write(A),
    write('.\n'),
    fail.
test :-
    halt.
