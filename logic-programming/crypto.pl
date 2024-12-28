% Crypo tests

:- op(1200, xfx, :+).

'<urn:example:sha>'(A, B, C) :-
    atom_concat(sha, A, D),
    sha_hash(B, E, [algorithm(D)]),
    hash_atom(E, C).

% queries
true :+ '<urn:example:sha>'(1, 'blargh', _).
true :+ '<urn:example:sha>'(224, 'blargh', _).
true :+ '<urn:example:sha>'(256, 'blargh', _).
true :+ '<urn:example:sha>'(384, 'blargh', _).
true :+ '<urn:example:sha>'(512, 'blargh', _).
