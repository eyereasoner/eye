% See https://en.wikipedia.org/wiki/Permutation

:- op(1200, xfx, :+).

'<urn:example:permutation>'([], []).
'<urn:example:permutation>'(As, [B|Bs]) :-
    select(B, As, Cs),
    '<urn:example:permutation>'(Cs, Bs).

% query
true :+ '<urn:example:permutation>'([1, 2, 3, 4], _).
