:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#goldbach>'(4, [2, 2])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(6, [3, 3])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(8, [3, 5])).

% proof steps
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(4, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(4, [2, 2]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(6, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(6, [3, 3]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(8, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(8, [3, 5]), true).
