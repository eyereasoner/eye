% '<https://eyereasoner.github.io/ns#person1>' thinking and picking up
% '<https://eyereasoner.github.io/ns#person3>' thinking and picking up
% '<https://eyereasoner.github.io/ns#person1>' eating and putting down
% '<https://eyereasoner.github.io/ns#person3>' eating and putting down
% '<https://eyereasoner.github.io/ns#person4>' thinking and picking up
% '<https://eyereasoner.github.io/ns#person2>' thinking and picking up
% '<https://eyereasoner.github.io/ns#person2>' eating and putting down
% '<https://eyereasoner.github.io/ns#person4>' eating and putting down
% '<https://eyereasoner.github.io/ns#person5>' thinking and picking up
% '<https://eyereasoner.github.io/ns#person5>' eating and putting down
% '<https://eyereasoner.github.io/ns#person1>' thinking and picking up
% '<https://eyereasoner.github.io/ns#person3>' thinking and picking up
% '<https://eyereasoner.github.io/ns#person1>' eating and putting down
% '<https://eyereasoner.github.io/ns#person3>' eating and putting down
% '<https://eyereasoner.github.io/ns#person2>' thinking and picking up
% '<https://eyereasoner.github.io/ns#person4>' thinking and picking up
% '<https://eyereasoner.github.io/ns#person2>' eating and putting down
% '<https://eyereasoner.github.io/ns#person4>' eating and putting down
% '<https://eyereasoner.github.io/ns#person5>' thinking and picking up
% '<https://eyereasoner.github.io/ns#person5>' eating and putting down
:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#got>'('<https://eyereasoner.github.io/ns#all>', '<https://eyereasoner.github.io/ns#dinner>')).

% proof steps
step((true:+'<https://eyereasoner.github.io/ns#got>'('<https://eyereasoner.github.io/ns#all>', '<https://eyereasoner.github.io/ns#dinner>')), '<https://eyereasoner.github.io/ns#got>'('<https://eyereasoner.github.io/ns#all>', '<https://eyereasoner.github.io/ns#dinner>'), true).
