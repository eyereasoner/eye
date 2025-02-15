:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#believes>'('<https://eyereasoner.github.io/ns#Fabian>', ('<https://eyereasoner.github.io/ns#notNecessarilyA>'(A, '<https://eyereasoner.github.io/ns#gold>'):+'<https://eyereasoner.github.io/ns#glitter>'(A)))).
answer('<https://eyereasoner.github.io/ns#notNecessarilyA>'('<https://eyereasoner.github.io/ns#northStar>', '<https://eyereasoner.github.io/ns#gold>')).

% proof steps
step((A:+'<https://eyereasoner.github.io/ns#says>'('<https://eyereasoner.github.io/ns#Einstein>', A)), '<https://eyereasoner.github.io/ns#says>'('<https://eyereasoner.github.io/ns#Einstein>', ('<https://eyereasoner.github.io/ns#notNecessarilyA>'(B, '<https://eyereasoner.github.io/ns#gold>'):+'<https://eyereasoner.github.io/ns#glitter>'(B))), ('<https://eyereasoner.github.io/ns#notNecessarilyA>'(B, '<https://eyereasoner.github.io/ns#gold>'):+'<https://eyereasoner.github.io/ns#glitter>'(B))).
step((true:+'<https://eyereasoner.github.io/ns#believes>'('<https://eyereasoner.github.io/ns#Fabian>', _)), '<https://eyereasoner.github.io/ns#believes>'('<https://eyereasoner.github.io/ns#Fabian>', ('<https://eyereasoner.github.io/ns#notNecessarilyA>'(A, '<https://eyereasoner.github.io/ns#gold>'):+'<https://eyereasoner.github.io/ns#glitter>'(A))), true).
step(('<https://eyereasoner.github.io/ns#notNecessarilyA>'(A, '<https://eyereasoner.github.io/ns#gold>'):+'<https://eyereasoner.github.io/ns#glitter>'(A)), '<https://eyereasoner.github.io/ns#glitter>'('<https://eyereasoner.github.io/ns#northStar>'), '<https://eyereasoner.github.io/ns#notNecessarilyA>'('<https://eyereasoner.github.io/ns#northStar>', '<https://eyereasoner.github.io/ns#gold>')).
step((true:+'<https://eyereasoner.github.io/ns#notNecessarilyA>'(_, _)), '<https://eyereasoner.github.io/ns#notNecessarilyA>'('<https://eyereasoner.github.io/ns#northStar>', '<https://eyereasoner.github.io/ns#gold>'), true).
