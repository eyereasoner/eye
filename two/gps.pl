% Goal driven Parallel Sequences -- Jos De Roo
% See background paper https://arxiv.org/pdf/2010.12027.pdf

:- use_module(library(lists)).
:- use_module(library(iso_ext)).

% find paths in the state space from initial state to goal state within limits
findpath(Goal,Path,Duration,Cost,Belief,Comfort,Limits) :-
    findpaths([],Goal,[],0.0,0.0,1.0,1.0,Path,Duration,Cost,Belief,Comfort,Limits).

findpaths(_Maps,Goal,Path,Duration,Cost,Belief,Comfort,Path,Duration,Cost,Belief,Comfort,_Limits) :-
    Goal,
    !.
findpaths(Maps_s,Goal,Path_s,Duration_s,Cost_s,Belief_s,Comfort_s,Path,Duration,Cost,Belief,Comfort,Limits) :-
    Limits = [MaxDuration,MaxCost,MinBelief,MinComfort,MaxStagecount],
    clause(description(Map,From,Transition,To,Action,Duration_n,Cost_n,Belief_n,Comfort_n),Where),
    From,
    Where,
    description(Map,From,Transition,To,Action,Duration_n,Cost_n,Belief_n,Comfort_n),
    append(Maps_s,[Map],Maps_t),
    stagecount(Maps_t,Stagecount),
    Stagecount =< MaxStagecount,
    Duration_t is Duration_s+Duration_n,
    Duration_t =< MaxDuration,
    Cost_t is Cost_s+Cost_n,
    Cost_t =< MaxCost,
    Belief_t is Belief_s*Belief_n,
    Belief_t >= MinBelief,
    Comfort_t is Comfort_s*Comfort_n,
    Comfort_t >= MinComfort,
    append(Path_s,[Action],Path_t),
    becomes(From,To),
    call_cleanup(findpaths(Maps_t,Goal,Path_t,Duration_t,Cost_t,Belief_t,Comfort_t,Path,Duration,Cost,Belief,Comfort,Limits),becomes(To,From)).

% counting the number of stages (a stage is a sequence of steps in the same map)
stagecount([],1).
stagecount([C,E|_],B) :-
    C \= E,
    !,
    stagecount(_,G),
    B is G+1.
stagecount([_|D],B) :-
    stagecount(D,B).

% linear logic implication
becomes(A,B) :-
    catch(A,_,fail),
    conj_list(A,D),
    forall(
        member(E,D),
        retract(E)
    ),
    conj_list(B,G),
    forall(
        member(H,G),
        assertz(H)
    ).

conj_list(true,[]) :-
    !.
conj_list(A,[A]) :-
    A \= (_,_),
    A \= false,
    !.
conj_list((A,B),[A|C]) :-
    conj_list(B,C).


% test data
:- dynamic(description/9).
:- dynamic(location/2).

% map of Belgium
description(map_be,location(S,brugge),true,location(S,oostende),drive_brugge_oostende,900.0,0.004,0.98,1.0).
description(map_be,location(S,gent),true,location(S,brugge),drive_gent_brugge,1500.0,0.006,0.96,0.99).

% current state
location(i1,gent).

% test cases
case(findpath(location(_SUBJECT,oostende),_PATH,_DURATION,_COST,_BELIEF,_COMFORT,[3600.0,5.0,0.2,0.4,1])).

test :-
    case(A),
    A,
    write(A),
    write('.'),
    nl,
    fail.
test :-
    halt.
