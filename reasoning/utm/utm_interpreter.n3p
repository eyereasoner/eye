% interpreter for Univeral Turing Machine

pfx('utm:', '<http://josd.github.io/eye/reasoning/utm#>').
cpred('<http://josd.github.io/eye/reasoning/utm#compute>').

'<http://josd.github.io/eye/reasoning/utm#compute>'([], OutTape) :-
    start(I),
    find(I, [], #, [ ], OutTape).
'<http://josd.github.io/eye/reasoning/utm#compute>'([Head|Tail], OutTape) :-
    start(I),
    find(I, [], Head, Tail, OutTape).

find(State, Left, Cell, Right, OutTape) :-
    t(State, Cell, Write, Move, Next),
    move(Move, Left, Write, Right, A, B, C),
    continue(Next, A, B, C, OutTape).

continue(halt, Left, Cell, Right, OutTape) :-
    reverse(Left, R),
    append(R, [Cell|Right], OutTape).
continue(State, Left, Cell, Right, OutTape) :-
    find(State, Left, Cell, Right, OutTape).

move(l, [], Cell, Right, [], #, [Cell|Right]).
move(l, [Head|Tail], Cell, Right, Tail, Head, [Cell|Right]).
move(s, Left, Cell, Right, Left, Cell, Right).
move(r, Left, Cell, [], [Cell|Left], #, [] ).
move(r, Left, Cell, [Head|Tail], [Cell|Left], Head, Tail).
