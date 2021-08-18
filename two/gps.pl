% Goal driven Parallel Sequences -- Jos De Roo
% See background paper https://arxiv.org/pdf/2010.12027.pdf

:- use_module(library(lists)).
:- use_module(library(iso_ext)).

nsp(etc_,'http://josd.github.io/eye/two/cases#').

% find paths in the state space from initial state to goal state within limits
etc_findpath(Goal,Path,Duration,Cost,Belief,Comfort,Limits) :-
    etc_findpaths([],Goal,[],0.0,0.0,1.0,1.0,Path,Duration,Cost,Belief,Comfort,Limits).

etc_findpaths(_Maps,Goal,Path,Duration,Cost,Belief,Comfort,Path,Duration,Cost,Belief,Comfort,_Limits) :-
    Goal,
    !.
etc_findpaths(Maps_s,Goal,Path_s,Duration_s,Cost_s,Belief_s,Comfort_s,Path,Duration,Cost,Belief,Comfort,Limits) :-
    Limits = [MaxDuration,MaxCost,MinBelief,MinComfort,MaxStagecount],
    clause(etc_description(Map,From,Transition,To,Action,Duration_n,Cost_n,Belief_n,Comfort_n),Where),
    From,
    Where,
    etc_description(Map,From,Transition,To,Action,Duration_n,Cost_n,Belief_n,Comfort_n),
    append(Maps_s,[Map],Maps_t),
    etc_stagecount(Maps_t,Stagecount),
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
    etc_becomes(From,To),
    call_cleanup(etc_findpaths(Maps_t,Goal,Path_t,Duration_t,Cost_t,Belief_t,Comfort_t,Path,Duration,Cost,Belief,Comfort,Limits),etc_becomes(To,From)).

% counting the number of stages (a stage is a sequence of steps in the same map)
etc_stagecount([],1).
etc_stagecount([C,E|_],B) :-
    C \= E,
    !,
    etc_stagecount(_,G),
    B is G+1.
etc_stagecount([_|D],B) :-
    etc_stagecount(D,B).

% linear logic implication
etc_becomes(A,B) :-
    catch(A,_,fail),
    etc_conj_list(A,C),
    forall(member(D,C),retract(D)),
    etc_conj_list(B,E),
    forall(member(F,E),assertz(F)).

etc_conj_list(true,[]).
etc_conj_list(A,[A]) :-
    A \= (_,_),
    A \= false,
    !.
etc_conj_list((A,B),[A|C]) :-
    etc_conj_list(B,C).


% test data
:- dynamic(etc_description/9).
:- dynamic(etc_location/2).

% map of Belgium
etc_description(etc_map_be,etc_location(S,etc_gent),true,etc_location(S,etc_brugge),etc_drive_gent_brugge,1500.0,0.006,0.96,0.99).
etc_description(etc_map_be,etc_location(S,etc_gent),true,etc_location(S,etc_kortrijk),etc_drive_gent_kortrijk,1600.0,0.007,0.96,0.99).
etc_description(etc_map_be,etc_location(S,etc_kortrijk),true,etc_location(S,etc_brugge),etc_drive_kortrijk_brugge,1600.0,0.007,0.96,0.99).
etc_description(etc_map_be,etc_location(S,etc_brugge),true,etc_location(S,etc_oostende),etc_drive_brugge_oostende,900.0,0.004,0.98,1.0).

% current state
etc_location(etc_i1,etc_gent).

% test cases
case(etc_findpath(etc_location(_SUBJECT,etc_oostende),_PATH,_DURATION,_COST,_BELIEF,_COMFORT,[5000.0,5.0,0.2,0.4,1])).

test :-
    case(A),
    A,
    write(A),
    write('.\n'),
    fail.
test :-
    halt.
