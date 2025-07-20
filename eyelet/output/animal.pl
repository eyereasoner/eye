:- op(1200, xfx, :+).

answer(動物(人間)).
answer(動物(ジョー)).

step((true:+動物(A)),動物(人間),true).
step((true:+動物(A)),動物(ジョー),true).
