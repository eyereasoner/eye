:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#pi>'(100000, 3.141592653589792)).

% proof steps
step((true:+'<https://eyereasoner.github.io/ns#pi>'(100000, _)), '<https://eyereasoner.github.io/ns#pi>'(100000, 3.141592653589792), true).
