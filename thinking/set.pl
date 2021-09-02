% See original code at https://samples.jbpub.com/9780763772062/PrologLabBook09.pdf

webize(eye/1,'https://josd.github.io/eye/thinking/ns#').

% facts
eye(p(0,1)).
eye(p(0,2)).
eye(p(1,3)).
eye(p(2,4)).

% rules
eye(set('./set.pl',[A,B,C])) :-
    setof(A,B,C).

% test cases
% _S = {x | ∃y p(x,y)} = {0,1,2}.
case(eye(set(_SCOPE,[X,Y^eye(p(X,Y)),_SET]))).
% _S = {x | p(0,x)} = {1,2}.
case(eye(set(_SCOPE,[X,eye(p(0,X)),_SET]))).
% _S = {g(x,y) | ∃z (p(x,z) ∧ p(z,y))} = {g(0,3),g(0,4)}.
case(eye(set(_SCOPE,[eye(g(X,Y)),Z^(eye(p(X,Z)),eye(p(Z,Y))),_SET]))).

test :-
    case(A),
    A,
    writeq(A),
    write('.\n'),
    fail.
test :-
    halt.
