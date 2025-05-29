% 🌱 New state created: state1 from state1
% 🌱 New state created: state2 from state1
% 🌱 New state created: state3 from state1
:- op(1200, xfx, :+).

answer(evolve(state1, state1)).
answer((retract(state([state3, 0.9], true)), assertz(state([state3, 0.4], true)), agrees(alice, bob))).
answer((measured(alice, state1), amplitude(alice, 0.9))).
answer(evolve(state1, state2)).
answer(evolve(state1, state3)).

step((true:+evolve(state1, _)), evolve(state1, state1), true).
step((true:+retract(state([state3, 0.9], true)), assertz(state([state3, 0.4], true)), agrees(alice, bob)), (retract(state([state3, 0.9], true)), assertz(state([state3, 0.4], true)), agrees(alice, bob)), true).
step((true:+measured(alice, _), amplitude(alice, _)), (measured(alice, state1), amplitude(alice, 0.9)), true).
step((true:+evolve(state1, _)), evolve(state1, state2), true).
step((true:+evolve(state1, _)), evolve(state1, state3), true).
