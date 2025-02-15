:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#obligatory>'('<https://eyereasoner.github.io/ns#complete:task>'('<https://eyereasoner.github.io/ns#agent2>', '<https://eyereasoner.github.io/ns#task1>'))).
answer('<https://eyereasoner.github.io/ns#obligatory>'('<https://eyereasoner.github.io/ns#escalate:task>'('<https://eyereasoner.github.io/ns#agent1>', '<https://eyereasoner.github.io/ns#task1>'))).
answer('<https://eyereasoner.github.io/ns#permitted>'('<https://eyereasoner.github.io/ns#execute:task>'('<https://eyereasoner.github.io/ns#agent2>', '<https://eyereasoner.github.io/ns#task1>'))).
answer('<https://eyereasoner.github.io/ns#violation>'('<https://eyereasoner.github.io/ns#task1>')).
answer('<https://eyereasoner.github.io/ns#sanction>'('<https://eyereasoner.github.io/ns#agent2>')).

% proof steps
step((true:+'<https://eyereasoner.github.io/ns#obligatory>'(_)), '<https://eyereasoner.github.io/ns#obligatory>'('<https://eyereasoner.github.io/ns#complete:task>'('<https://eyereasoner.github.io/ns#agent2>', '<https://eyereasoner.github.io/ns#task1>')), true).
step((true:+'<https://eyereasoner.github.io/ns#obligatory>'(_)), '<https://eyereasoner.github.io/ns#obligatory>'('<https://eyereasoner.github.io/ns#escalate:task>'('<https://eyereasoner.github.io/ns#agent1>', '<https://eyereasoner.github.io/ns#task1>')), true).
step((true:+'<https://eyereasoner.github.io/ns#permitted>'(_)), '<https://eyereasoner.github.io/ns#permitted>'('<https://eyereasoner.github.io/ns#execute:task>'('<https://eyereasoner.github.io/ns#agent2>', '<https://eyereasoner.github.io/ns#task1>')), true).
step((true:+'<https://eyereasoner.github.io/ns#violation>'(_)), '<https://eyereasoner.github.io/ns#violation>'('<https://eyereasoner.github.io/ns#task1>'), true).
step((true:+'<https://eyereasoner.github.io/ns#sanction>'(_)), '<https://eyereasoner.github.io/ns#sanction>'('<https://eyereasoner.github.io/ns#agent2>'), true).
