% inference fuse

:- op(1200, xfx, :+).

'<https://eyereasoner.github.io/ns#color>'('<https://eyereasoner.github.io/ns#stone>', '<https://eyereasoner.github.io/ns#black>').
'<https://eyereasoner.github.io/ns#color>'('<https://eyereasoner.github.io/ns#stone>', '<https://eyereasoner.github.io/ns#white>').

false :+
    '<https://eyereasoner.github.io/ns#color>'(X, '<https://eyereasoner.github.io/ns#black>'),
    '<https://eyereasoner.github.io/ns#color>'(X, '<https://eyereasoner.github.io/ns#white>').
