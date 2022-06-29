% Solving Sudoku puzzles
% Original code from https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/p97.pl

%  P97 (**) Sudoku
%
%  Sudoku puzzles go like this:

%   Problem statement                Solution

%    .  .  4 | 8  .  . | .  1  7     9  3  4 | 8  2  5 | 6  1  7
%            |         |                     |         |
%    6  7  . | 9  .  . | .  .  .     6  7  2 | 9  1  4 | 8  5  3
%            |         |                     |         |
%    5  .  8 | .  3  . | .  .  4     5  1  8 | 6  3  7 | 9  2  4
%    --------+---------+--------     --------+---------+--------
%    3  .  . | 7  4  . | 1  .  .     3  2  5 | 7  4  8 | 1  6  9
%            |         |                     |         |
%    .  6  9 | .  .  . | 7  8  .     4  6  9 | 1  5  3 | 7  8  2
%            |         |                     |         |
%    .  .  1 | .  6  9 | .  .  5     7  8  1 | 2  6  9 | 4  3  5
%    --------+---------+--------     --------+---------+--------
%    1  .  . | .  8  . | 3  .  6     1  9  7 | 5  8  2 | 3  4  6
%            |         |                     |         |
%    .  .  . | .  .  6 | .  9  1     8  5  3 | 4  7  6 | 2  9  1
%            |         |                     |         |
%    2  4  . | .  .  1 | 5  .  .     2  4  6 | 3  9  1 | 5  7  8

% Every spot in the puzzle belongs to a (horizontal) row and a (vertical)
% column, as well as to one single 3x3 square (which we call "square"
% for short). At the beginning, some of the spots carry a single-digit
% number between 1 and 9. The problem is to fill the missing spots with
% digits in such a way that every number between 1 and 9 appears exactly
% once in each row, in each column, and in each square.

% We represent the Sudoku puzzle as a simple list of 81 digits. At
% the beginning, the list is partially instantiated. During the
% process, all the elememts of the list get instantiated with digits.

% We are going to treat each spot as a Prolog term spot(X,R,C,S) where
% X is the number to put into the field, R is the row, C the column, and
% S the square the field belongs to. R, C, and S are lists which represent
% the respective number sets.

% --------------------------------------------------------------------------

% sudoku(Puzzle) :- solve the given Sudoku puzzle and print the
%    problem statement as well as the solution to the standard output
%   (list-of-integers, partially instantiated)

sudoku(Puzzle) :-
    printPuzzle(Puzzle),
    nl,
    connect(Spots),
    init(Puzzle,Spots),
    solve(Spots),
    printPuzzle(Puzzle),
    fail.
sudoku(_) :-
    nl.

% ---------------------------------------------------------------

% The most difficult part of the problem solution is to prepare
% the list of spot/4 terms representing the spots in the puzzle.
% We have to make sure that every spot "knows" its row, column,
% and square. In other words, all the spots in a row access the
% same list in order to check whether a new number can be placed
% in the row. The same is true for the columns and the squares.

% connect(Spots) :- Spots = [spot(_,R1,C1,S1),spot(_,R1,C2,S1),.....].

connect(Spots) :-
    length(Spots,81),
    connectRows(Spots),
    connectCols(Spots),
    connectSquares(Spots).

% connectRows(Spots) :- connect the spots of all rows in the list Spot

connectRows([]).
connectRows(Spots) :-
    connectRow(Spots,_,9),
    skip(Spots,9,Spots1),
    connectRows(Spots1).

% connectRow(Spots,R,K) :- the first K elements of Spot
% are in the same row R

connectRow(_,_,0).
connectRow([spot(_,R,_,_)|Spots],R,K) :- K > 0,
    K1 is K-1,
    connectRow(Spots,R,K1).

% connectCols(Spots) :- connect the spots of the same column

connectCols(Spots) :-
    connectCols(Spots,9).

% connectCols(Spots,K) :- connect K more columns columns

connectCols(_,0) :-
    !.
connectCols(Spots,K) :-
    K > 0,
    connectCol(Spots,_),
    skip(Spots,1,Spots1),
    K1 is K-1,
    connectCols(Spots1,K1).

% connectCol(Spots,C) :- connect the first spot in Spots with
% the other spots in its column C

connectCol([],_).
connectCol([spot(_,_,C,_)|Spots],C) :-
    skip(Spots,8,Spots1),
    connectCol(Spots1,C).

% connectSquares(Spots) :- connect all three bands
% The nine squares are arranged in three horizontal bands,
% three squares in each band.

connectSquares(Spots) :-
    connectBand(Spots),
    skip(Spots,27,Spots1),
    connectBand(Spots1),
    skip(Spots1,27,Spots2),
    connectBand(Spots2).

% connectBand(Spots) :- connect the next band (of three squares

connectBand(Spots) :-
    connectSq(Spots,_),
    skip(Spots,3,Spots1),
    connectSq(Spots1,_),
    skip(Spots1,3,Spots2),
    connectSq(Spots2,_).

% connectSq(Spots,S) :- connect the spots of square S. In the Spots
%    list each square is composed of three spot-triplets which
%    are separated by 6 spots.

connectSq([],_).
connectSq(Spots,S) :-
  connectTriplet(Spots,S),
  skip(Spots,9,Spots1),
  connectTriplet(Spots1,S),
  skip(Spots1,9,Spots2),
  connectTriplet(Spots2,S).

% connectTriplet(Spots,S) :- connect the next three spots in the Spots
%    list with the square S

connectTriplet([spot(_,_,_,S),spot(_,_,_,S),spot(_,_,_,S)|_],S).

% skip(List,N,List1) :- skip the first N elements from a List
%    and return the rest of the list in List1. If the List is
%    too short, then succeed and return [] as rest.

skip([],_,[]) :-
    !.
skip(Xs,0,Xs) :-
    !.
skip([_|Xs],K,Zs) :-
    K > 0,
    K1 is K-1,
    skip(Xs,K1,Zs).

% -----------------------------------------------------------

% init(Puzzle,Spots) :- initialize the Spots list on the
%    basis of the problem statement (Puzzle) and link the
%    Puzzle list to the Spots list

init([],[]).
init([X|Xs],[Sp|Spots]) :-
    initSpot(X,Sp),
    init(Xs,Spots).

% If X is not instantiated in the given puzzle, create a link
% between the variable in the puzzle and the corresponding
% variable in the spot. Otherwise copy the given number from
% the puzzle into the spot and insert it into the spot's
% correct row, column, and square, if this is legal.

initSpot(X,spot(X,_,_,_)) :-
    var(X),
    !.
initSpot(X,spot(X,R,C,S)) :-
    integer(X),
    insert(X,R),
    insert(X,C),
    insert(X,S).

% ----------------------------------------------------------

% solve(Spots) :- solve the problem using backtrack

solve([]).
solve([Spot|Spots]) :-
    bind(Spot),
    solve(Spots).

% bind(Spot) :- bind the data field in Spot to an
% available non-conflicting digit.

bind(spot(X,_,_,_)) :-
    nonvar(X),
    !.
bind(spot(X,R,C,S)) :-
    var(X),
    between(1,9,X),  % try a digit
    insert(X,R),
    insert(X,C),
    insert(X,S).

% ---------------------------------------------------------

% insert(X,L) :- X can be inserted into the list without
% conflict, i.e. X is not yet in the list, when insert/2
% is called. Otherwise the predicate fails.

insert(X,L) :-
    var(L),
    !,
    L = [X|_].
insert(X,[Y|Ys]) :-
    X \= Y,
    insert(X,Ys).

% ---------------------------------------------------------

printPuzzle([]).
printPuzzle(Xs) :-
    nl,
    printBand(Xs,Xs1),
    write('--------+---------+--------'),
    nl,
    printBand(Xs1,Xs2),
    write('--------+---------+--------'),
    nl,
    printBand(Xs2,_).

printBand(Xs,Xs3) :-
    printRow(Xs,Xs1),
    nl,
    write('        |         |'),
    nl,
    printRow(Xs1,Xs2),
    nl,
    write('        |         |'),
    nl,
    printRow(Xs2,Xs3),
    nl.

printRow(Xs,Xs3) :-
    printTriplet(Xs,Xs1),
    write(' | '),
    printTriplet(Xs1,Xs2),
    write(' | '),
    printTriplet(Xs2,Xs3).

printTriplet(Xs,Xs3) :-
    printElement(Xs,Xs1),
    write('  '),
    printElement(Xs1,Xs2),
    write('  '),
    printElement(Xs2,Xs3).

printElement([X|Xs],Xs) :-
    var(X),
    !,
    write('.').
printElement([X|Xs],Xs) :-
    write(X).

printCounter(0) :-
    !,
    write('No solution'),
    nl.
printCounter(1) :-
    !,
    write('1 solution'),
    nl.
printCounter(K) :-
    write(K),
    write(' solutions'),
    nl.

% ---------------------------------------------------------

test(N) :-
    puzzle(N,P),
    sudoku(P).

puzzle(1,P) :-
    P = [_,_,4,8,_,_,_,1,7, 6,7,_,9,_,_,_,_,_, 5,_,8,_,3,_,_,_,4,
         3,_,_,7,4,_,1,_,_, _,6,9,_,_,_,7,8,_, _,_,1,_,6,9,_,_,5,
         1,_,_,_,8,_,3,_,6, _,_,_,_,_,6,_,9,1, 2,4,_,_,_,1,5,_,_].

puzzle(2,P) :-
    P = [3,_,_,_,7,1,_,_,_, _,5,_,_,_,_,1,8,_, _,4,_,8,_,_,_,_,_,
         _,_,6,2,_,_,3,_,_, _,_,1,_,5,_,8,_,_, _,_,3,_,_,8,2,_,_,
         _,_,_,_,_,3,_,4,_, _,6,4,_,_,_,_,7,_, _,_,_,9,6,_,_,_,1].

puzzle(3,P) :-
    P = [1,7,_,_,_,9,_,_,4, _,_,_,_,_,_,7,_,_, 5,_,_,3,_,_,2,_,_,
         _,8,_,_,_,_,5,3,6, _,_,_,_,8,_,_,_,_, 6,9,1,_,_,_,_,8,_,
         _,_,7,_,_,4,_,_,2, _,_,2,_,_,_,_,_,_, 3,_,_,5,_,_,_,7,1].

% an example with many solutions

puzzle(4,P) :-
    P = [1,_,_,_,_,9,_,_,4, _,_,_,_,_,_,7,_,_, 5,_,_,3,_,_,2,_,_,
         _,8,_,_,_,_,5,_,6, _,_,_,_,8,_,_,_,_, 6,9,1,_,_,_,_,8,_,
         _,_,7,_,_,4,_,_,2, _,_,2,_,_,_,_,_,_, 3,_,_,5,_,_,_,7,1].

puzzle(5,P) :-
    P = [_,6,5,_,_,_,7,2,_, 3,_,7,_,_,_,1,_,8, 2,9,_,_,1,_,_,3,4,
         _,_,_,5,_,7,_,_,_, _,_,1,_,_,_,8,_,_, _,_,_,2,_,1,_,_,_,
         8,1,_,_,2,_,_,5,7, 7,_,2,_,_,_,9,_,1, _,5,4,_,_,_,6,8,_].

puzzle(6,P) :-
    P = [5,_,2,_,_,3,_,_,_, 4,6,_,_,7,_,9,_,_, _,_,3,4,_,_,_,_,_,
         9,5,_,_,6,_,_,_,_, _,4,_,_,_,_,_,9,_, _,_,_,_,9,_,_,1,7,
         _,_,_,_,_,7,2,_,_, _,_,9,_,4,_,_,3,5, _,_,_,3,_,_,7,_,6].

% an example with an error in the problem statement (5 appears
% twice in the top left square)

puzzle(e1,P) :-
    P = [5,_,2,_,_,3,_,_,_, 4,6,5,_,7,_,9,_,_, _,_,3,4,_,_,_,_,_,
         9,5,_,_,6,_,_,_,_, _,4,_,_,_,_,_,9,_, _,_,_,_,9,_,_,1,7,
         _,_,_,_,_,7,2,_,_, _,_,9,_,4,_,_,3,5, _,_,_,3,_,_,7,_,6].

% another example with an error in the problem statement (garbage
% in the first row

puzzle(e2,P) :-
    P = [x,_,2,_,_,3,_,_,_, 4,6,_,_,7,_,9,_,_, _,_,3,4,_,_,_,_,_,
         9,5,_,_,6,_,_,_,_, _,4,_,_,_,_,_,9,_, _,_,_,_,9,_,_,1,7,
         _,_,_,_,_,7,2,_,_, _,_,9,_,4,_,_,3,5, _,_,_,3,_,_,7,_,6].

% some more examples from the Sonntagszeitung

puzzle(8,P) :-
    P = [4,8,_,_,7,_,_,_,_, _,_,9,6,8,_,3,_,7, 3,_,7,4,_,_,_,5,_,
         _,_,_,3,_,_,_,2,_, 9,5,_,7,2,1,_,6,8, _,1,_,_,_,4,_,_,_,
         _,4,_,_,_,2,7,_,1, 8,_,2,_,4,7,5,_,_, _,_,_,_,5,_,_,8,4].

puzzle(9,P) :-
    P = [_,1,_,_,_,_,_,2,4, 5,_,_,_,4,_,_,8,6, 6,_,4,1,_,_,_,_,_,
         _,_,_,8,_,6,9,_,_, 8,_,_,_,_,_,_,_,2, _,_,6,4,_,3,_,_,_,
         _,_,_,_,_,7,2,_,8, 1,6,_,_,9,_,_,_,5, 7,4,_,_,_,_,_,9,_].

puzzle(10,P) :-
    P = [_,9,7,_,_,5,_,_,4, _,_,_,_,_,9,_,_,_, _,_,5,_,4,_,2,_,7,
         _,8,6,_,_,3,_,_,_, _,_,_,_,2,_,_,_,_, _,_,_,5,_,_,3,4,_,
         5,_,3,_,7,_,6,_,_, _,_,_,6,_,_,_,_,_, 9,_,_,8,_,_,1,7,_].

% a puzzle rated "not fun" by
% http://dingo.sbs.arizona.edu/~sandiway/sudoku/examples.html

puzzle(11,P) :-
    P = [_,2,_,_,_,_,_,_,_, _,_,_,6,_,_,_,_,3, _,7,4,_,8,_,_,_,_,
         _,_,_,_,_,3,_,_,2, _,8,_,_,4,_,_,1,_, 6,_,_,5,_,_,_,_,_,
         _,_,_,_,1,_,7,8,_, 5,_,_,_,_,9,_,_,_, _,_,_,_,_,_,_,4,_].

% a "super hard puzzle" by
% http://www.menneske.no/sudoku/eng/showpuzzle.html?number=2155141

puzzle(12,P) :-
    P = [_,_,_,6,_,_,4,_,_, 7,_,_,_,_,3,6,_,_, _,_,_,_,9,1,_,8,_,
         _,_,_,_,_,_,_,_,_, _,5,_,1,8,_,_,_,3, _,_,_,3,_,6,_,4,5,
         _,4,_,2,_,_,_,6,_, 9,_,3,_,_,_,_,_,_, _,2,_,_,_,_,1,_,_].

% some puzzles from Spektrum der Wissenschaft 3/2006, p.100

% leicht
puzzle(13,P) :-
    P = [_,2,6,4,5,8,3,_,_, 1,7,_,_,_,_,_,4,_, _,8,_,_,_,_,_,_,_,
         _,_,_,_,_,_,9,8,_, _,_,_,5,9,_,1,_,4, 7,_,_,2,_,1,_,5,_,
         _,_,_,_,4,_,_,3,_, _,_,_,8,_,_,5,_,_, 6,_,_,_,_,7,_,9,1].

% mittel
puzzle(14,P) :-
    P = [9,_,_,6,3,_,_,_,4, _,1,_,2,5,8,_,_,_, _,_,_,7,_,_,_,_,8,
         6,4,_,_,2,_,5,_,_, _,_,_,_,_,_,_,_,_, 8,2,_,5,_,_,_,9,_,
         _,_,_,_,_,_,8,7,_, 3,_,_,_,_,5,_,4,_, _,_,1,_,7,6,_,_,_].

% schwer
puzzle(15,P) :-
    P = [_,_,_,_,_,_,_,_,7, _,_,_,_,_,_,6,3,4, _,_,_,9,4,_,_,2,_,
         5,_,1,7,_,_,8,6,_, _,_,9,_,_,_,_,_,3, _,_,_,_,8,_,_,_,_,
         4,3,_,5,_,_,_,_,_, _,1,_,_,6,8,_,_,_, _,_,_,_,_,3,1,_,9].

% hoellisch (!)
puzzle(16,P) :-
    P = [_,_,_,_,3,_,_,_,_, _,1,5,_,_,_,6,_,_, 6,_,_,2,_,_,3,4,_,
         _,_,_,6,_,_,_,8,_, _,3,9,_,_,_,5,_,_, 5,_,_,_,_,_,9,_,2,
         _,_,_,_,_,_,_,_,_, _,_,_,9,7,_,2,5,_, 1,_,_,_,5,_,_,7,_].

% Spektrum der Wissenschaft 3/2006 Preisraetsel (angeblich hoellisch !)

puzzle(17,P) :-
    P = [_,1,_,_,6,5,4,_,_, _,_,_,_,8,4,1,_,_, 4,_,_,_,_,_,_,7,_,
         _,5,_,1,9,_,_,_,_, _,_,3,_,_,_,7,_,_, _,_,_,_,3,7,_,5,_,
         _,8,_,_,_,_,_,_,3, _,_,2,6,5,_,_,_,_, _,_,9,8,1,_,_,2,_].

% the (almost) empty grid

puzzle(99,P) :-
    P = [1,2,3,4,5,6,7,8,9, _,_,_,_,_,_,_,_,_, _,_,_,_,_,_,_,_,_,
         _,_,_,_,_,_,_,_,_, _,_,_,_,_,_,_,_,_, _,_,_,_,_,_,_,_,_,
         _,_,_,_,_,_,_,_,_, _,_,_,_,_,_,_,_,_, _,_,_,_,_,_,_,_,_].

% sudoku
'https://josd.github.io/eye/ns#sudoku'(Puzzle,Solution) :-
    copy_term(Puzzle,Solution),
    connect(Spots),
    init(Solution,Spots),
    solve(Spots).

% query
query('https://josd.github.io/eye/ns#sudoku'([8,_,_,4,_,5,_,_,_,6,_,4,_,_,_,_,_,1,_,_,_,_,1,_,_,_,2,_,3,7,2,_,_,_,_,_,_,_,_,_,5,1,_,_,9,_,_,_,_,_,4,_,3,_,9,_,2,_,_,8,1,_,7,7,_,_,_,_,_,_,_,_,_,_,1,_,_,2,_,_,_],_ANSWER)).

run :-
    query(Q),
    Q,
    writeq(Q),
    write('.\n').
