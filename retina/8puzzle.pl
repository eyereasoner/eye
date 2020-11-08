% See Chaper 9 of "Thinking as Computation" from Hector J. Levesque

:- initialization(main).

main :-
    initial_state(I),
    stages([G|GL]),
    plan_all(G,I,L,GL),
    writeln(L),
    halt.

% initial_state(State)
initial_state([1,6,3,8,2,7,5,4,0]).

% legal_move(BeforeState,Move,AfterState)
legal_move([0,B,C,X,E,F,G,H,I],up(X),[X,B,C,0,E,F,G,H,I]).
legal_move([A,0,C,D,X,F,G,H,I],up(X),[A,X,C,D,0,F,G,H,I]).
legal_move([A,B,0,D,E,X,G,H,I],up(X),[A,B,X,D,E,0,G,H,I]).
legal_move([A,B,C,0,E,F,X,H,I],up(X),[A,B,C,X,E,F,0,H,I]).
legal_move([A,B,C,D,0,F,G,X,I],up(X),[A,B,C,D,X,F,G,0,I]).
legal_move([A,B,C,D,E,0,G,H,X],up(X),[A,B,C,D,E,X,G,H,0]).
legal_move([0,X,C,D,E,F,G,H,I],left(X),[X,0,C,D,E,F,G,H,I]).
legal_move([A,0,X,D,E,F,G,H,I],left(X),[A,X,0,D,E,F,G,H,I]).
legal_move([A,B,C,0,X,F,G,H,I],left(X),[A,B,C,X,0,F,G,H,I]).
legal_move([A,B,C,D,0,X,G,H,I],left(X),[A,B,C,D,X,0,G,H,I]).
legal_move([A,B,C,D,E,F,0,X,I],left(X),[A,B,C,D,E,F,X,0,I]).
legal_move([A,B,C,D,E,F,G,0,X],left(X),[A,B,C,D,E,F,G,X,0]).
legal_move(S1,down(X),S2) :-
    legal_move(S2,up(X),S1).
legal_move(S1,right(X),S2) :-
    legal_move(S2,left(X),S1).

% stages(ListofStages)
stages([init,r1,r1c1,goal]).

% goal_stage(Stage,State)
goal_stage(init,_).
goal_stage(r1,[1,2,3|_]).
goal_stage(r1c1,[1,2,3,4,_,_,7,_,_]).
goal_stage(goal,[1,2,3,4,5,6,7,8,0]).

% plan_all(G,S,L,GL): L passes from G through each stage in GL.
plan_all(_,_,[],[]).
plan_all(G1,S1,L,[G2|GL]) :-
   append(L1,L2,L),
   reach(G1,S1,L1,G2,S2),
   plan_all(G2,S2,L2,GL).

% reach(G1,S1,L,G2,S2): L moves from S1 in stage G1 to S2 in G2.
reach(_,S,[],G2,S) :-           % S attains stage G2.
    goal_stage(G2,S).
reach(G1,S1,[M|L],G2,S3) :-     % move from S1 to S2.
   legal_move(S1,M,S2),
   goal_stage(G1,S2),
   reach(G1,S2,L,G2,S3).
