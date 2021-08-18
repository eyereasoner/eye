% See original code at https://samples.jbpub.com/9780763772062/PrologLabBook09.pdf

nsp(etc_,'http://josd.github.io/eye/two/cases#').

% facts
etc_p(0,1).
etc_p(0,2).
etc_p(1,3).
etc_p(2,4).

% test cases
% _S = {x | ∃y etc_p(x,y)} = {0,1,2}.
case((findall(etc_p(X,Y),etc_p(X,Y),_F),setof(X,Y^etc_p(X,Y),_S))).
% _S = {x | ∃y etc_p(x,y)} = {1,2}.
case((findall(etc_p(X,Y),etc_p(X,Y),_F),setof(X,etc_p(0,X),_S))).
% _S = {etc_g(x,y) | ∃z (etc_p(x,z) ∧ etc_p(z,y))} = {etc_g(0,3),etc_g(0,4)}.
case((findall(etc_p(X,Y),etc_p(X,Y),_F),setof(etc_g(X,Y),Z^(etc_p(X,Z),etc_p(Z,Y)),_S))).

test :-
    case(A),
    A,
    write(A),
    write('.\n'),
    fail.
test :-
    halt.
