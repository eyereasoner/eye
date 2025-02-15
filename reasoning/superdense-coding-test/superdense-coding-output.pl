:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#sdcoding>'(1, 1)).
answer('<https://eyereasoner.github.io/ns#sdcoding>'(3, 3)).
answer('<https://eyereasoner.github.io/ns#sdcoding>'(0, 0)).
answer('<https://eyereasoner.github.io/ns#sdcoding>'(2, 2)).

% proof steps
step(('<https://eyereasoner.github.io/ns#sdconot>'(A, B):+'<https://eyereasoner.github.io/ns#sdc>'(A, B)), '<https://eyereasoner.github.io/ns#sdc>'(0, 1), '<https://eyereasoner.github.io/ns#sdconot>'(0, 1)).
step(('<https://eyereasoner.github.io/ns#sdconot>'(A, B):+'<https://eyereasoner.github.io/ns#sdc>'(A, B)), '<https://eyereasoner.github.io/ns#sdc>'(0, 3), '<https://eyereasoner.github.io/ns#sdconot>'(0, 3)).
step(('<https://eyereasoner.github.io/ns#sdconot>'(A, B):+'<https://eyereasoner.github.io/ns#sdc>'(A, B)), '<https://eyereasoner.github.io/ns#sdc>'(1, 0), '<https://eyereasoner.github.io/ns#sdconot>'(1, 0)).
step(('<https://eyereasoner.github.io/ns#sdconot>'(A, B):+'<https://eyereasoner.github.io/ns#sdc>'(A, B)), '<https://eyereasoner.github.io/ns#sdc>'(1, 1), '<https://eyereasoner.github.io/ns#sdconot>'(1, 1)).
step(('<https://eyereasoner.github.io/ns#sdconot>'(A, B):+'<https://eyereasoner.github.io/ns#sdc>'(A, B)), '<https://eyereasoner.github.io/ns#sdc>'(1, 2), '<https://eyereasoner.github.io/ns#sdconot>'(1, 2)).
step(('<https://eyereasoner.github.io/ns#sdconot>'(A, B):+'<https://eyereasoner.github.io/ns#sdc>'(A, B)), '<https://eyereasoner.github.io/ns#sdc>'(2, 1), '<https://eyereasoner.github.io/ns#sdconot>'(2, 1)).
step(('<https://eyereasoner.github.io/ns#sdconot>'(A, B):+'<https://eyereasoner.github.io/ns#sdc>'(A, B)), '<https://eyereasoner.github.io/ns#sdc>'(2, 3), '<https://eyereasoner.github.io/ns#sdconot>'(2, 3)).
step(('<https://eyereasoner.github.io/ns#sdconot>'(A, B):+'<https://eyereasoner.github.io/ns#sdc>'(A, B)), '<https://eyereasoner.github.io/ns#sdc>'(3, 1), '<https://eyereasoner.github.io/ns#sdconot>'(3, 1)).
step(('<https://eyereasoner.github.io/ns#sdconot>'(A, B):+'<https://eyereasoner.github.io/ns#sdc>'(A, B)), '<https://eyereasoner.github.io/ns#sdc>'(3, 3), '<https://eyereasoner.github.io/ns#sdconot>'(3, 3)).
step(('<https://eyereasoner.github.io/ns#sdconot>'(A, B):+'<https://eyereasoner.github.io/ns#sdc>'(A, B)), '<https://eyereasoner.github.io/ns#sdc>'(3, 0), '<https://eyereasoner.github.io/ns#sdconot>'(3, 0)).
step(('<https://eyereasoner.github.io/ns#sdconot>'(A, B):+'<https://eyereasoner.github.io/ns#sdc>'(A, B)), '<https://eyereasoner.github.io/ns#sdc>'(3, 2), '<https://eyereasoner.github.io/ns#sdconot>'(3, 2)).
step(('<https://eyereasoner.github.io/ns#sdconot>'(A, B):+'<https://eyereasoner.github.io/ns#sdc>'(A, B)), '<https://eyereasoner.github.io/ns#sdc>'(0, 0), '<https://eyereasoner.github.io/ns#sdconot>'(0, 0)).
step(('<https://eyereasoner.github.io/ns#sdconot>'(A, B):+'<https://eyereasoner.github.io/ns#sdc>'(A, B)), '<https://eyereasoner.github.io/ns#sdc>'(2, 0), '<https://eyereasoner.github.io/ns#sdconot>'(2, 0)).
step(('<https://eyereasoner.github.io/ns#sdconot>'(A, B):+'<https://eyereasoner.github.io/ns#sdc>'(A, B)), '<https://eyereasoner.github.io/ns#sdc>'(2, 2), '<https://eyereasoner.github.io/ns#sdconot>'(2, 2)).
step((true:+'<https://eyereasoner.github.io/ns#sdcoding>'(_, _)), '<https://eyereasoner.github.io/ns#sdcoding>'(1, 1), true).
step((true:+'<https://eyereasoner.github.io/ns#sdcoding>'(_, _)), '<https://eyereasoner.github.io/ns#sdcoding>'(3, 3), true).
step((true:+'<https://eyereasoner.github.io/ns#sdcoding>'(_, _)), '<https://eyereasoner.github.io/ns#sdcoding>'(0, 0), true).
step((true:+'<https://eyereasoner.github.io/ns#sdcoding>'(_, _)), '<https://eyereasoner.github.io/ns#sdcoding>'(2, 2), true).
