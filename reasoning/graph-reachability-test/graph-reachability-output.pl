:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#isReachable>'(a, f)).
answer(\+'<https://eyereasoner.github.io/ns#isReachable>'(b, e)).
answer('<https://eyereasoner.github.io/ns#isReachable>'(c, g)).

% proof steps
step((true:+'<https://eyereasoner.github.io/ns#isReachable>'(a, f)), '<https://eyereasoner.github.io/ns#isReachable>'(a, f), true).
step((true:+ \+'<https://eyereasoner.github.io/ns#isReachable>'(b, e)), \+'<https://eyereasoner.github.io/ns#isReachable>'(b, e), true).
step((true:+'<https://eyereasoner.github.io/ns#isReachable>'(c, g)), '<https://eyereasoner.github.io/ns#isReachable>'(c, g), true).
