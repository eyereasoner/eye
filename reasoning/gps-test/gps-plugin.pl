% --------------------------------------------
% Goal driven Parallel Sequences -- Jos De Roo
% --------------------------------------------

:- op(1200, xfx, :+).

% find paths in the state space from initial state to goal state within limits
'<urn:example:findpath>'(_SCOPE, [Goal, Path, Duration, Cost, Belief, Comfort, Limits]) :-
    findpaths([], Goal, [], 0.0, 0.0, 1.0, 1.0, Path, Duration, Cost, Belief, Comfort, Limits).

findpaths(_Maps, Goal, Path, Duration, Cost, Belief, Comfort, Path, Duration, Cost, Belief, Comfort, _Limits) :-
    Goal,
    !.
findpaths(Maps_s, Goal, Path_s, Duration_s, Cost_s, Belief_s, Comfort_s, Path, Duration, Cost, Belief, Comfort, Limits) :-
    Limits = [MaxDuration, MaxCost, MinBelief, MinComfort, MaxStagecount],
    clause('<urn:example:description>'(Map, [From, Transition, To, Action, Duration_n, Cost_n, Belief_n, Comfort_n]), Where),
    From,
    Where,
    '<urn:example:description>'(Map, [From, Transition, To, Action, Duration_n, Cost_n, Belief_n, Comfort_n]),
    append(Maps_s, [Map], Maps_t),
    stagecount(Maps_t, Stagecount),
    Stagecount =< MaxStagecount,
    Duration_t is Duration_s+Duration_n,
    Duration_t =< MaxDuration,
    Cost_t is Cost_s+Cost_n,
    Cost_t =< MaxCost,
    Belief_t is Belief_s*Belief_n,
    Belief_t >= MinBelief,
    Comfort_t is Comfort_s*Comfort_n,
    Comfort_t >= MinComfort,
    append(Path_s, [Action], Path_t),
    becomes(From, To),
    (   findpaths(Maps_t, Goal, Path_t, Duration_t, Cost_t, Belief_t, Comfort_t, Path, Duration, Cost, Belief, Comfort, Limits)
    ->  becomes(To, From)
    ;   becomes(To, From),
        fail
    ).

% counting the number of stages (a stage is a sequence of steps in the same map)
stagecount([], 1).
stagecount([C, E|_], B) :-
    C \= E,
    !,
    stagecount(_, G),
    B is G+1.
stagecount([_|D], B) :-
    stagecount(D, B).
