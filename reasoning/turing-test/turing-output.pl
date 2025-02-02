:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#compute>'([1, 0, 1, 0, 0, 1], [1, 0, 1, 0, 1, 0, #])).
answer('<https://eyereasoner.github.io/ns#compute>'([1, 0, 1, 1, 1, 1], [1, 1, 0, 0, 0, 0, #])).
answer('<https://eyereasoner.github.io/ns#compute>'([1, 1, 1, 1, 1, 1], [1, 0, 0, 0, 0, 0, 0, #])).
answer('<https://eyereasoner.github.io/ns#compute>'([], [1, #])).

% proof steps
step((true:+'<https://eyereasoner.github.io/ns#compute>'([1, 0, 1, 0, 0, 1], _)), '<https://eyereasoner.github.io/ns#compute>'([1, 0, 1, 0, 0, 1], [1, 0, 1, 0, 1, 0, #]), true).
step((true:+'<https://eyereasoner.github.io/ns#compute>'([1, 0, 1, 1, 1, 1], _)), '<https://eyereasoner.github.io/ns#compute>'([1, 0, 1, 1, 1, 1], [1, 1, 0, 0, 0, 0, #]), true).
step((true:+'<https://eyereasoner.github.io/ns#compute>'([1, 1, 1, 1, 1, 1], _)), '<https://eyereasoner.github.io/ns#compute>'([1, 1, 1, 1, 1, 1], [1, 0, 0, 0, 0, 0, 0, #]), true).
step((true:+'<https://eyereasoner.github.io/ns#compute>'([], _)), '<https://eyereasoner.github.io/ns#compute>'([], [1, #]), true).
