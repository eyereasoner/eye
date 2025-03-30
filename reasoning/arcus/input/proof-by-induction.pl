% Proof by induction
% See https://en.wikipedia.org/wiki/Mathematical_induction#Sum_of_consecutive_natural_numbers

:- op(1200, xfx, :+).

% base case: the sum of the first 0 numbers is 0.
'urn:example:sum'(0, 0).

% recursive case: the sum of the first n numbers is the sum of the first n-1 numbers plus n.
'urn:example:sum'(N, Sum) :-
    N > 0,
    N1 is N-1,
    'urn:example:sum'(N1, Sum1),
    Sum is Sum1+N.

% query
true :+ 'urn:example:sum'(4096, _).
