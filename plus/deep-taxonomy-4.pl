% Deep Taxonimy benchmark

% data
'<urn:example:N0>'('<urn:example:z>').

% prepare deep taxonomy
'<urn:example:prepare>'(X, Y) :-
    between(X, Y, Z),
    atom_number(I, Z),
    Z0 is Z-1,
    atom_number(I0, Z0),
    atomic_list_concat(['<urn:example:N', I0, '>'], A),
    atomic_list_concat(['<urn:example:N', I, '>'], B),
    atomic_list_concat(['<urn:example:I', I, '>'], C),
    atomic_list_concat(['<urn:example:J', I, '>'], D),
    dynamic(A/1),
    dynamic(B/1),
    dynamic(C/1),
    dynamic(D/1),
    Av =.. [A, V],
    Bv =.. [B, V],
    Cv =.. [C, V],
    Dv =.. [D, V],
    (   \+(Bv :+ Av)
    ->  assertz((Bv :+ Av))
    ;   true
    ),
    (   \+(Cv :+ Av)
    ->  assertz((Cv :+ Av))
    ;   true
    ),
    (   \+(Dv :+ Av)
    ->  assertz((Dv :+ Av))
    ;   true
    ),
    fail;
    true.

% queries
true :+ '<urn:example:prepare>'(1, 4).
true :+ '<urn:example:N4>'(_).
