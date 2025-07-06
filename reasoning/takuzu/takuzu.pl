%--------------------------
% Takuzu 6x6 Solver
%
% - Generate-and-test with constraints enforced after each move.
% - Finds all solutions (or up to a user-specified limit).
% - Uses lists to represent the grid: -1 = blank, 0 = 0, 1 = 1.
%--------------------------

% --- Replace the N-th element in a list (1-based) with X ---
replace_nth1([_|T], 1, X, [X|T]).
replace_nth1([H|T], N, X, [H|R]) :-
    N > 1,
    N1 is N-1,
    replace_nth1(T, N1, X, R).

% --- Set a value in a 2D grid at (Row,Col) (1-based) ---
set_cell([Row|Rest], 1, Col, Val, [NewRow|Rest]) :-
    replace_nth1(Row, Col, Val, NewRow).
set_cell([Row|Rest], RowIdx, Col, Val, [Row|NewRest]) :-
    RowIdx > 1,
    R1 is RowIdx-1,
    set_cell(Rest, R1, Col, Val, NewRest).

% --- Find first empty cell (-1) in the grid, return its (Row,Col) (1-based) ---
find_empty(Grid, Row, Col) :-
    nth1(Row, Grid, GridRow),
    nth1(Col, GridRow, -1),
    !.

% --- Entry point: solve_all(+Grid, -Solutions, +Limit)
% Returns a list of all solutions (up to Limit, or all if Limit=none)
solve_all(Grid, Solutions, Limit) :-
    solve_all_bt(Grid, Limit, Solutions).

% --- Helper: collect all solutions by backtracking
solve_all_bt(Grid, Limit, Solutions) :-
    findall(S, solve_bt(Grid, S), AllSols),
    (   Limit = none
    ->  Solutions = AllSols
    ;   (   integer(Limit)
        ->  length(Solutions, Limit),
            append(Solutions, _, AllSols)
        ;   Solutions = AllSols
        )
    ).

% --- Core solver: fill the grid recursively ---
solve_bt(Grid, Solution) :-
    (   \+ find_empty(Grid, _, _)
    ->  Solution = Grid                         % If no empty cell: we're done!
    ;   find_empty(Grid, Row, Col),             % Find the next empty cell
        member(V, [0,1]),                       % Try both 0 and 1
        set_cell(Grid, Row, Col, V, NewGrid),   % Place V in (Row,Col)
        is_locally_valid(NewGrid, Row, Col),    % Check constraints
        solve_bt(NewGrid, Solution)             % Continue recursively
    ).

% --- Constraints: check all Takuzu rules locally after each move ---
is_locally_valid(Grid, Row, Col) :-
    nth1(Row, Grid, RowList),
    check_line(RowList),                        % Row constraints
    transpose(Grid, TGrid),
    nth1(Col, TGrid, ColList),
    check_line(ColList),                        % Column constraints
    unique_filled_rows(Grid, Row),              % Unique finished rows
    unique_filled_rows(TGrid, Col).             % Unique finished columns

% --- Row/Column constraints:
%    - ≤3 zeros and ≤3 ones at all times
%    - Exactly 3 of each only when filled
%    - No three consecutive identical digits (except blanks)
check_line(Line) :-
    count(0, Line, Z), count(1, Line, O),
    Z =< 3, O =< 3,
    (   \+ member(-1, Line)
    ->  Z = 3,
        O = 3
    ;   true
    ),
    no_triples(Line).

% --- Check for three consecutive identical (filled) digits in a line ---
no_triples([A,B,C|Rest]) :-
    (   A \= -1,
        A = B,
        B = C
    ->  fail
    ;   no_triples([B,C|Rest])
    ).
no_triples([_]).
no_triples([_,_]).

% --- Count occurrences of X in a list ---
count(X, List, C) :-
    include(=(X), List, Matches),
    length(Matches, C).

% --- Check that filled rows/cols are pairwise unique ---
%     Only enforces uniqueness when a line is fully filled (no -1)
unique_filled_rows(Grid, Index) :-
    nth1(Index, Grid, Row),
    \+ member(-1, Row),
    !,
    findall(R, (nth1(I, Grid, R), I \= Index, \+ member(-1, R)), Others),
    \+ member(Row, Others).
unique_filled_rows(_, _). % Always true if line isn't filled

% --- Transpose a list-of-lists matrix ---
transpose([[]|_], []) :-
    !.
transpose(Matrix, [Row|Rows]) :-
    maplist(list_head_tail, Matrix, Row, Rest),
    transpose(Rest, Rows).
list_head_tail([H|T], H, T).

% --- Example 6x6 Puzzle ---
example_puzzle([
    [-1, 0, 1, -1, -1, -1],
    [-1, -1, -1, 0, -1, 1],
    [1, -1, -1, -1, 0, -1],
    [-1, 1, -1, -1, -1, 0],
    [-1, -1, 0, -1, 1, -1],
    [0, -1, -1, -1, -1, -1]
]).

% --- Solve the puzzle ---
solve(P, Solutions) :-
    example_puzzle(P),
    solve_all(P, Solutions, none).

