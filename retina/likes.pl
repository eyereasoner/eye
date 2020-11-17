:- initialization(main).

main :-
    findall(likes(X,Y),likes(X,Y),Z),
    Z = [likes('Peter',sk('Peter')),likes('Bob',sk('Bob'))],
    write('[] a "PASS".'),
    nl,
    halt.

person('Peter').
person('Bob').

likes(Person,sk(Person)) :- person(Person).
