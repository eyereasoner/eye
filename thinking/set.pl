% See original code at https://samples.jbpub.com/9780763772062/PrologLabBook09.pdf

webize(el/1,'https://josd.github.io/eye/thinking/ns#').

% facts
el(p(0,1)).
el(p(0,2)).
el(p(1,3)).
el(p(2,4)).

% rules
el(set('./set.pl',[A,B,C])) :-
    setof(A,B,C).

% test cases
% _S = {x | ∃y p(x,y)} = {0,1,2}.
case(el(set(_SCOPE,[X,Y^el(p(X,Y)),_SET]))).
% _S = {x | p(0,x)} = {1,2}.
case(el(set(_SCOPE,[X,el(p(0,X)),_SET]))).
% _S = {g(x,y) | ∃z (p(x,z) ∧ p(z,y))} = {g(0,3),g(0,4)}.
case(el(set(_SCOPE,[el(g(X,Y)),Z^(el(p(X,Z)),el(p(Z,Y))),_SET]))).

test :-
    case(A),
    A,
    writeq(A),
    write('.\n'),
    fail.
test :-
    halt.
