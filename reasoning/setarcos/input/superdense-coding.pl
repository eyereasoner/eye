% Superdense coding using discrete quantum computing
%
% See https://arxiv.org/pdf/1101.3764.pdf and https://arxiv.org/pdf/1010.2929.pdf
%
% Discrete quantum theory is obtained by instantiating the mathematical framework
% of Hilbert spaces with a finite field instead of the field of complex numbers.
% This instantiation collapses much the structure of actual quantum mechanics but
% retains several of its distinguishing characteristics including the notions of
% superposition, interference, and entanglement. Furthermore, discrete quantum
% theory excludes local hidden variable models, has a no-cloning theorem, and can
% express natural counterparts of quantum information protocols such as superdense
% coding and teleportation.
%
% Surprisingly discrete quantum computing is identical to conventional logic
% programming except for a small twist that is responsible for all the
% "quantum-ness". The twist occurs when merging sets of answers computed by
% several alternatives: the answers are combined using an exclusive version of
% logical disjunction. In other words, the two branches of a choice junction
% exhibit an interference effect: an answer is produced from the junction if it
% occurs in one or the other branch but not both.

:- op(1200, xfx, :+).

% |R) = |0, 0) + |1, 1)
r(false, false).
r(true, true).

% |S) = |0, 1) + |1, 0)
s(false, true).
s(true, false).

% |U) = |0, 0) + |1, 0) + |1, 1)
u(false, false).
u(true, false).
u(true, true).

% |V ) = |0, 0) + |0, 1) + |1, 0)
v(false, false).
v(false, true).
v(true, false).

% ID |0) = |0)
id(false, false).
% ID |1) = |1)
id(true, true).

% G |0) = |1)
g(false, true).
% G |1) = |0)
g(true, false).

% K |0) = |0)
k(false, false).
% K |1) = |0) + |1)
k(true, false).
k(true, true).

% KG
kg(X, Y) :-
    g(X, Z),
    k(Z, Y).

% GK
gk(X, Y) :-
    k(X, Z),
    g(Z, Y).

% alice
alice(0, [X, Y]) :-
    id(X, Y).
alice(1, [X, Y]) :-
    g(X, Y).
alice(2, [X, Y]) :-
    k(X, Y).
alice(3, [X, Y]) :-
    kg(X, Y).

% bob
bob([X, Y], 0) :-
    gk(X, Y).
bob([X, Y], 1) :-
    k(X, Y).
bob([X, Y], 2) :-
    g(X, Y).
bob([X, Y], 3) :-
    id(X, Y).

:- dynamic('urn:example:sdcoding'/2).

% superdense coding
'urn:example:sdc'(N, M) :-
    r(X, Y),
    alice(N, [X, B]),
    bob([B, Y], M),
    (   'urn:example:sdcoding'(N, M)
    ->  retract('urn:example:sdcoding'(N, M))
    ;   assertz('urn:example:sdcoding'(N, M))
    ).

% superdense coding appearing an odd number of times
'urn:example:sdconot'(N, M) :+
    'urn:example:sdc'(N, M).

% query
true :+ 'urn:example:sdcoding'(_, _).
