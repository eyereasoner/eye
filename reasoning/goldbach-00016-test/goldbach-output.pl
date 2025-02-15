:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#goldbach>'(4, [2, 2])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(6, [3, 3])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(8, [3, 5])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(10, [3, 7])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(12, [5, 7])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(14, [3, 11])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(16, [3, 13])).

% proof steps
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(4, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(4, [2, 2]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(6, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(6, [3, 3]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(8, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(8, [3, 5]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(10, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(10, [3, 7]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(12, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(12, [5, 7]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(14, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(14, [3, 11]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(16, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(16, [3, 13]), true).
