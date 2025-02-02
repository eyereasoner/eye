:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#control1>'('<https://eyereasoner.github.io/ns#actuator1>', 39.27346198678276)).
answer('<https://eyereasoner.github.io/ns#control1>'('<https://eyereasoner.github.io/ns#actuator2>', 26.08)).

% proof steps
step((true:+'<https://eyereasoner.github.io/ns#control1>'(_, _)), '<https://eyereasoner.github.io/ns#control1>'('<https://eyereasoner.github.io/ns#actuator1>', 39.27346198678276), true).
step((true:+'<https://eyereasoner.github.io/ns#control1>'(_, _)), '<https://eyereasoner.github.io/ns#control1>'('<https://eyereasoner.github.io/ns#actuator2>', 26.08), true).
