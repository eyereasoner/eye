% ----------------------------------------
% Mathematical library for complex numbers
% ----------------------------------------

% Inspired by http://alain.colmerauer.free.fr/alcol/ArchivesPublications/Equation4/Equation4.pdf

'<http://josd.github.io/eye/math#exp>'([[A,B],[C,D]],[E,F]) :-
    '<http://josd.github.io/eye/math#polar>'([A,B],[G,H]),
    E is G^C*exp(-D*H)*cos(D*log(G)+C*H),
    F is G^C*exp(-D*H)*sin(D*log(G)+C*H).

'<http://josd.github.io/eye/math#log>'([[A,B],[C,D]],[E,F]) :-
    '<http://josd.github.io/eye/math#polar>'([A,B],[G,H]),
    '<http://josd.github.io/eye/math#polar>'([C,D],[I,J]),
    K is log(G),
    L is log(I),
    '<http://josd.github.io/eye/math#divide>'([[L,J],[K,H]],[E,F]).

'<http://josd.github.io/eye/math#sin>'([A,B],[C,D]) :-
    C is sin(A)*(exp(B)+exp(-B))/2,
    D is cos(A)*(exp(B)-exp(-B))/2.

'<http://josd.github.io/eye/math#cos>'([A,B],[C,D]) :-
    C is cos(A)*(exp(B)+exp(-B))/2,
    D is -sin(A)*(exp(B)-exp(-B))/2.

'<http://josd.github.io/eye/math#tan>'(A,B) :-
    '<http://josd.github.io/eye/math#sin>'(A,C),
    '<http://josd.github.io/eye/math#cos>'(A,D),
    '<http://josd.github.io/eye/math#divide>'([C,D],B).

'<http://josd.github.io/eye/math#asin>'([A,B],[C,D]) :-
    E is (sqrt((1+A)^2+B^2)-sqrt((1-A)^2+B^2))/2,
    F is (sqrt((1+A)^2+B^2)+sqrt((1-A)^2+B^2))/2,
    C is asin(E),
    D is log(F+sqrt(F^2-1)).

'<http://josd.github.io/eye/math#acos>'([A,B],[C,D]) :-
    E is (sqrt((1+A)^2+B^2)-sqrt((1-A)^2+B^2))/2,
    F is (sqrt((1+A)^2+B^2)+sqrt((1-A)^2+B^2))/2,
    C is acos(E),
    D is -log(F+sqrt(F^2-1)).

'<http://josd.github.io/eye/math#atan>'(A,B) :-
    '<http://josd.github.io/eye/math#subtract>'([[0,1],A],C),
    '<http://josd.github.io/eye/math#add>'([[0,1],A],D),
    '<http://josd.github.io/eye/math#divide>'([C,D],E),
    '<http://josd.github.io/eye/math#log>'([[e,0],E],F),
    '<http://josd.github.io/eye/math#divide>'([F,[0,2]],B).

'<http://josd.github.io/eye/math#polar>'([A,B],[C,D]) :-
    C is sqrt(A^2+B^2),
    E is acos(abs(A)/C),
    angular(A,B,E,D).

angular(A,B,C,D) :-
    A >= 0,
    B >= 0,
    D = C.
angular(A,B,C,D) :-
    A < 0,
    B >= 0,
    D is pi-C.
angular(A,B,C,D) :-
    A < 0,
    B < 0,
    D is C+pi.
angular(A,B,C,D) :-
    A >= 0,
    B < 0,
    D is 2*pi-C.

'<http://josd.github.io/eye/math#minus>'([A,B],[C,D]) :-
    C is -A,
    D is -B.

'<http://josd.github.io/eye/math#subtract>'([[A,B],[C,D]],[E,F]) :-
    E is A-C,
    F is B-D.

'<http://josd.github.io/eye/math#add>'([[A,B],[C,D]],[E,F]) :-
    E is A+C,
    F is B+D.

'<http://josd.github.io/eye/math#multiply>'([[A,B],[C,D]],[E,F]) :-
    E is A*C-B*D,
    F is A*D+B*C.

'<http://josd.github.io/eye/math#inverse>'([A,B],[C,D]) :-
    C is A/(A^2+B^2),
    D is -B/(A^2+B^2).

'<http://josd.github.io/eye/math#divide>'([A,B],C) :-
    '<http://josd.github.io/eye/math#inverse>'(B,D),
    '<http://josd.github.io/eye/math#multiply>'([A,D],C).
