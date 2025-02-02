:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#mustHave>'('<https://eyereasoner.github.io/ns#alice>', '<https://eyereasoner.github.io/ns#dogLicense>')).

% proof steps
step((true:+'<https://eyereasoner.github.io/ns#mustHave>'(_, _)), '<https://eyereasoner.github.io/ns#mustHave>'('<https://eyereasoner.github.io/ns#alice>', '<https://eyereasoner.github.io/ns#dogLicense>'), true).
