% Proof by induction

:- op(1200, xfx, :+).

% base case: the sum of the first 0 numbers is 0.
'<https://eyereasoner.github.io/ns#sum>'(0, 0).

% recursive case: the sum of the first n numbers is the sum of the first n-1 numbers plus n.
'<https://eyereasoner.github.io/ns#sum>'(N, Sum) :-
    N > 0,
    N1 is N-1,
    '<https://eyereasoner.github.io/ns#sum>'(N1, Sum1),
    Sum is Sum1+N.

% query
true :+ '<https://eyereasoner.github.io/ns#sum>'(4096, _).
