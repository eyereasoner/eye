:- style_check(-discontiguous).

:- table('<https://eyereasoner.github.io/ns#has>'/2).
:- table('<https://eyereasoner.github.io/ns#has-instance>'/2).

'<https://eyereasoner.github.io/ns#has>'(X, Y) :-
    '<https://eyereasoner.github.io/ns#has-instance>'(X, Y).

'<https://eyereasoner.github.io/ns#has-instance>'(X, Y) :-
    '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(X, '<https://eyereasoner.github.io/ns#Class>'),
    '<https://eyereasoner.github.io/ns#has>'(X, Y).

'<https://eyereasoner.github.io/ns#has>'('<https://eyereasoner.github.io/ns#A>', '<https://eyereasoner.github.io/ns#b>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<https://eyereasoner.github.io/ns#A>', '<https://eyereasoner.github.io/ns#Class>').
'<https://eyereasoner.github.io/ns#has-instance>'('<https://eyereasoner.github.io/ns#A>', '<https://eyereasoner.github.io/ns#c>').
