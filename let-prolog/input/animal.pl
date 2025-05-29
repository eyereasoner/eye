% example from https://github.com/sasagawa888/nprolog?tab=readme-ov-file#unicode

:- op(1200, xfx, :+).

人間(ジョー).       % human(joe).
動物(人間).         % animal(human).

動物(X) :- 人間(X). % animal(X) :- human(X).

% query
true :+ 動物(_).
