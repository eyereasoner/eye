% See original code at https://samples.jbpub.com/9780763772062/PrologLabBook09.pdf

nsp(etc_,'http://josd.github.io/eye/two/cases#').

% facts
etc_p(etc_a,etc_b).
etc_p(etc_a,etc_c).
etc_p(etc_b,etc_d).
etc_p(etc_c,etc_e).

% test cases
% _S = {x | ∃y etc_p(x,y)} = {etc_a,etc_b,etc_c}.
case((findall(etc_p(X,Y),etc_p(X,Y),_F),setof(X,Y^etc_p(X,Y),_S))).
% _S = {x | etc_p(0,x)} = {etc_b,etc_c}.
case((findall(etc_p(X,Y),etc_p(X,Y),_F),setof(X,etc_p(etc_a,X),_S))).
% _S = {etc_g(x,y) | ∃z (etc_p(x,z) ∧ etc_p(z,y))} = {etc_g(etc_a,etc_d),etc_g(etc_a,etc_e)}.
case((findall(etc_p(X,Y),etc_p(X,Y),_F),setof(etc_g(X,Y),Z^(etc_p(X,Z),etc_p(Z,Y)),_S))).

test :-
    case(A),
    A,
    write(A),
    write('.\n'),
    fail.
test :-
    halt.
