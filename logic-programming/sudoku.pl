% Original at https://www.metalevel.at/sudoku/sudoku.pl

:- use_module(library(apply)).
:- use_module(library(clpfd)).

'urn:example:sudoku'(Rows) :-
    length(Rows, 9),
    maplist(same_length(Rows), Rows),
    append(Rows, Vs),
    Vs ins 1..9,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    Rows = [As, Bs, Cs, Ds, Es, Fs, Gs, Hs, Is],
    blocks(As, Bs, Cs),
    blocks(Ds, Es, Fs),
    blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1, N2, N3|Ns1], [N4, N5, N6|Ns2], [N7, N8, N9|Ns3]) :-
    all_distinct([N1, N2, N3, N4, N5, N6, N7, N8, N9]),
    blocks(Ns1, Ns2, Ns3).

% query
true :=
    'urn:example:sudoku'([
        [1, _, _, 8, _, 4, _, _, _],
        [_, 2, _, _, _, _, 4, 5, 6],
        [_, _, 3, 2, _, 5, _, _, _],
        [_, _, _, 4, _, _, 8, _, 5],
        [7, 8, 9, _, 5, _, _, _, _],
        [_, _, _, _, _, 6, 2, _, 3],
        [8, _, 1, _, _, _, 7, _, _],
        [_, _, _, 1, 2, 3, _, 8, _],
        [2, _, 5, _, _, _, _, _, 9]]
    ).
