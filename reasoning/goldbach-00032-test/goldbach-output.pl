:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#goldbach>'(4, [2, 2])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(6, [3, 3])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(8, [3, 5])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(10, [3, 7])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(12, [5, 7])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(14, [3, 11])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(16, [3, 13])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(18, [5, 13])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(20, [3, 17])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(22, [3, 19])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(24, [5, 19])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(26, [3, 23])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(28, [5, 23])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(30, [7, 23])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(32, [3, 29])).

% proof steps
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(4, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(4, [2, 2]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(6, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(6, [3, 3]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(8, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(8, [3, 5]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(10, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(10, [3, 7]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(12, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(12, [5, 7]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(14, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(14, [3, 11]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(16, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(16, [3, 13]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(18, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(18, [5, 13]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(20, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(20, [3, 17]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(22, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(22, [3, 19]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(24, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(24, [5, 19]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(26, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(26, [3, 23]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(28, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(28, [5, 23]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(30, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(30, [7, 23]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(32, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(32, [3, 29]), true).
