% '<https://eyereasoner.github.io/ns#person1>' thinking for 0.1 seconds
% '<https://eyereasoner.github.io/ns#person3>' thinking for 0.15 seconds
% '<https://eyereasoner.github.io/ns#person2>' thinking for 0.2 seconds
% '<https://eyereasoner.github.io/ns#person1>' eating for 0.1 seconds
% '<https://eyereasoner.github.io/ns#person5>' thinking for 0.25 seconds
% '<https://eyereasoner.github.io/ns#person4>' thinking for 0.25 seconds
% '<https://eyereasoner.github.io/ns#person3>' eating for 0.1 seconds
% '<https://eyereasoner.github.io/ns#person5>' eating for 0.1 seconds
% '<https://eyereasoner.github.io/ns#person2>' eating for 0.2 seconds
% '<https://eyereasoner.github.io/ns#person4>' eating for 0.2 seconds
% '<https://eyereasoner.github.io/ns#person1>' thinking for 0.1 seconds
% '<https://eyereasoner.github.io/ns#person3>' thinking for 0.15 seconds
% '<https://eyereasoner.github.io/ns#person1>' eating for 0.1 seconds
% '<https://eyereasoner.github.io/ns#person2>' thinking for 0.2 seconds
% '<https://eyereasoner.github.io/ns#person4>' thinking for 0.25 seconds
% '<https://eyereasoner.github.io/ns#person5>' thinking for 0.25 seconds
% '<https://eyereasoner.github.io/ns#person3>' eating for 0.1 seconds
% '<https://eyereasoner.github.io/ns#person4>' eating for 0.2 seconds
% '<https://eyereasoner.github.io/ns#person2>' eating for 0.2 seconds
% '<https://eyereasoner.github.io/ns#person5>' eating for 0.1 seconds
:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#got>'('<https://eyereasoner.github.io/ns#all>', '<https://eyereasoner.github.io/ns#dinner>')).

% proof steps
step((true:+'<https://eyereasoner.github.io/ns#got>'('<https://eyereasoner.github.io/ns#all>', '<https://eyereasoner.github.io/ns#dinner>')), '<https://eyereasoner.github.io/ns#got>'('<https://eyereasoner.github.io/ns#all>', '<https://eyereasoner.github.io/ns#dinner>'), true).
