% display user details

:- op(1200, xfx, :+).

% user's personal details
'urn:example:userDetails'(user_1, first_name, 'Patrick').
'urn:example:userDetails'(user_1, place, 'Brugge').
'urn:example:userDetails'(user_1, birth_year, 1971).

% determine whether a given combination is allowed
'urn:example:display'(User, perm(Perm), not(Not)) :-
    'urn:example:userDetails'(User, _, _),
    findall(Det, 'urn:example:userDetails'(User, _, Det), All),
    (   combination(1, All, Perm)
    ;   combination(2, All, Perm)
    ),
    diff(All, Perm, Not).

% combination
combination(0, _, []).
combination(I, As, Bs) :-
    I > 0,
    select(B, As, Cs),
    J is I-1,
    combination(J, Cs, Ds),
    sort([B|Ds], Bs).

% difference
diff([], _, []).
diff([A|Bs], Cs, Ds) :-
    (   memberchk(A, Cs)
    ->  diff(Bs, Cs, Ds)
    ;   Ds = [A|Es],
        diff(Bs, Cs, Es)
    ).

% query
true :+ 'urn:example:display'(_, _, _).
