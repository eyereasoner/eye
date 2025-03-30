:- table('<https://eyereasoner.github.io/ns#pass>'/2).  

'<https://eyereasoner.github.io/ns#likes>'('<https://eyereasoner.github.io/ns#bob>', '<https://eyereasoner.github.io/ns#apple>').

'<https://eyereasoner.github.io/ns#pass>'('<https://eyereasoner.github.io/ns#test>', 7) :-
    '<https://eyereasoner.github.io/ns#likes>'('<https://eyereasoner.github.io/ns#bob>', '<https://eyereasoner.github.io/ns#apple>').
