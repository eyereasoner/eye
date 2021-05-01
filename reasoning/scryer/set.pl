% See original code at https://samples.jbpub.com/9780763772062/PrologLabBook09.pdf

% facts
p(a, b).
p(a, c).
p(b, d).
p(c, e).

% test cases
% _S = {x | ∃y p(x, y)} = {a, b, c}.
case((findall(p(X, Y), p(X, Y), _F), setof(X, Y^p(X, Y), _S))).
% _S = {x | ∃y p(x, y)} = {a, b, c}.
case((findall(p(X, Y), p(X, Y), _F), setof(X, p(a, X), _S))).
% _S = {g(x, y) | ∃z (p(x, z) ∧ p(z, y))}.
case((findall(p(X, Y), p(X, Y), _F), setof(g(X, Y), Z^(p(X, Z), p(Z, Y)), _S))).

test :-
    case(A),
    A,
    write('[ :scryer-statement "'),
    write(A),
    write('"].'),
    nl,
    fail.
test :-
    halt.
