% Lee routing for VLSI circuits
% Code from the book "The Art of Prolog" Chapter 16

:- op(1200, xfx, :+).

'urn:example:route'([Source, Destination, Obstacles], Path) :-
    waves(Destination, [[Source], []], Obstacles, Waves),
    path(Source, Destination, Waves, Path).

% waves(Destination, Wavessofar, Obstacles, Waves)
waves(B, [Wave|Waves], _, Waves) :-
    member(B, Wave),
    !.
waves(B, [Wave, LastWave|LastWaves], Obstacles, Waves) :-
    next_wave(Wave, LastWave, Obstacles, NextWave),
    waves(B, [NextWave, Wave, LastWave|LastWaves], Obstacles, Waves).

% next_waves(Wave, LastWave, Obstacles, NextWave)
next_wave(Wave, LastWave, Obstacles, NextWave) :-
    setof(X, admissible(X, Wave, LastWave, Obstacles), NextWave).

admissible(X, Wave, LastWave, Obstacles) :-
    adjacent(X, Wave, Obstacles),
    \+ member(X, LastWave),
    \+ member(X, Wave).

adjacent(X, Wave, Obstacles) :-
    member(X1, Wave),
    neighbor(X1, X),
    \+ obstructed(X, Obstacles).

neighbor([X1, Y], [X2, Y]) :-
    next_to(X1, X2).
neighbor([X, Y1], [X, Y2]) :-
    next_to(Y1, Y2).

next_to(X, X1) :-
    X1 is X+1.
next_to(X, X1) :-
    X > 0,
    X1 is X-1.

obstructed(Point, Obstacles) :-
    member(Obstacle, Obstacles),
    obstructs(Point, Obstacle).

obstructs([X, Y], [[X, Y1], [_, Y2]]) :-
    Y1 =< Y,
    Y =< Y2.
obstructs([X, Y], [[_, Y1], [X, Y2]]) :-
    Y1 =< Y,
    Y =< Y2.
obstructs([X, Y], [[X1, Y], [X2, _]]) :-
    X1 =< X,
    X =< X2.
obstructs([X, Y], [[X1, _], [X2, Y]]) :-
    X1 =< X,
    X =< X2.

% path(Source, Destination, Waves, Path)
path(A, A, _, [A]) :-
    !.
path(A, B, [Wave|Waves], [B|Path]) :-
    member(B1, Wave),
    neighbor(B, B1),
    !,
    path(A, B1, Waves, Path).

% query
true :+ 'urn:example:route'([[1, 1], [9, 8], [[[2, 3], [4, 5]], [[6, 6], [8, 8]]]], _).
