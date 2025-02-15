:- op(1200, xfx, :+).

answer((false:+'<https://eyereasoner.github.io/ns#is>'('<https://eyereasoner.github.io/ns#ground>', '<https://eyereasoner.github.io/ns#wet>'))).
answer((false:+'<https://eyereasoner.github.io/ns#is>'('<https://eyereasoner.github.io/ns#it>', '<https://eyereasoner.github.io/ns#raining>'))).

% proof steps
step(((false:+A):+(B:+A), (false:+B)), (('<https://eyereasoner.github.io/ns#is>'('<https://eyereasoner.github.io/ns#ground>', '<https://eyereasoner.github.io/ns#wet>'):+'<https://eyereasoner.github.io/ns#is>'('<https://eyereasoner.github.io/ns#it>', '<https://eyereasoner.github.io/ns#raining>')), (false:+'<https://eyereasoner.github.io/ns#is>'('<https://eyereasoner.github.io/ns#ground>', '<https://eyereasoner.github.io/ns#wet>'))), (false:+'<https://eyereasoner.github.io/ns#is>'('<https://eyereasoner.github.io/ns#it>', '<https://eyereasoner.github.io/ns#raining>'))).
step((true:+(false:+_)), (false:+'<https://eyereasoner.github.io/ns#is>'('<https://eyereasoner.github.io/ns#ground>', '<https://eyereasoner.github.io/ns#wet>')), true).
step((true:+(false:+_)), (false:+'<https://eyereasoner.github.io/ns#is>'('<https://eyereasoner.github.io/ns#it>', '<https://eyereasoner.github.io/ns#raining>')), true).
