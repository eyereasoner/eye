:- initialization(main).

main :-
    findall(likes(X,Y),likes(X,Y),Z),
    Z = [likes('Peter',sk('Peter')),likes('Bob',sk('Bob'))],
    write('true.\n').

person('Peter').
person('Bob').

likes(Person,sk(Person)) :- person(Person).
