% Access control policy example

:- op(1200, xfx, :+).

'urn:example:policy'('urn:example:test1', 'urn:example:PolicyX').
'urn:example:has'('urn:example:test1', 'urn:example:A').
'urn:example:has'('urn:example:test1', 'urn:example:B').
'urn:example:has'('urn:example:test1', 'urn:example:C').
'urn:example:Policy'('urn:example:PolicyX').
'urn:example:allOf'('urn:example:PolicyX', 'urn:example:A').
'urn:example:allOf'('urn:example:PolicyX', 'urn:example:B').
'urn:example:anyOf'('urn:example:PolicyX', 'urn:example:C').
'urn:example:noneOf'('urn:example:PolicyX', 'urn:example:D').

'urn:example:pass'(A, 'urn:example:allOfTest') :-
    'urn:example:policy'(B, A),
    'urn:example:Policy'(A),
    \+ (
        'urn:example:allOf'(A, C),
        \+ 'urn:example:has'(B, C)
    ).

'urn:example:pass'(A, 'urn:example:anyOfTest') :-
    'urn:example:policy'(B, A),
    'urn:example:Policy'(A),
    findall(C,
        (
            'urn:example:anyOf'(A, C),
            'urn:example:has'(B, C)
        ),
        D
    ),
    length(D, E),
    E \= 0.

'urn:example:pass'(A, 'urn:example:noneOfTest') :-
    'urn:example:policy'(B, A),
     'urn:example:Policy'(A),
    findall(C,
        (
            'urn:example:noneOf'(A, C),
            'urn:example:has'(B, C)
        ),
        D
    ),
    length(D, 0).

% query
true :+
    'urn:example:Policy'(A),
    'urn:example:pass'(A, 'urn:example:allOfTest'),
    'urn:example:pass'(A, 'urn:example:anyOfTest'),
    'urn:example:pass'(A, 'urn:example:noneOfTest').
