% 🌱 New state created: state1 from state1
evolve(state1,state1).
retract(state([state3,0.9],true)).
assertz(state([state3,0.4],true)).
agrees(alice,bob).
measured(alice,state1).
amplitude(alice,0.9) .

step(rule(evolve(state1, A), answer(evolve(state1, A))), evolve(state1, state1), answer(evolve(state1, state1))).
step(rule((retract(state([state3, 0.9], true)), assertz(state([state3, 0.4], true)), agrees(alice, bob)), answer((retract(state([state3, 0.9], true)), assertz(state([state3, 0.4], true)), agrees(alice, bob)))), (retract(state([state3, 0.9], true)), assertz(state([state3, 0.4], true)), agrees(alice, bob)), answer((retract(state([state3, 0.9], true)), assertz(state([state3, 0.4], true)), agrees(alice, bob)))).
step(rule((retract(state([state3, 0.9], true)), assertz(state([state3, 0.4], true)), agrees(alice, bob)), answer((retract(state([state3, 0.9], true)), assertz(state([state3, 0.4], true)), agrees(alice, bob)))), (retract(state([state3, 0.9], true)), assertz(state([state3, 0.4], true)), agrees(alice, bob)), answer((retract(state([state3, 0.9], true)), assertz(state([state3, 0.4], true)), agrees(alice, bob)))).
step(rule((retract(state([state3, 0.9], true)), assertz(state([state3, 0.4], true)), agrees(alice, bob)), answer((retract(state([state3, 0.9], true)), assertz(state([state3, 0.4], true)), agrees(alice, bob)))), (retract(state([state3, 0.9], true)), assertz(state([state3, 0.4], true)), agrees(alice, bob)), answer((retract(state([state3, 0.9], true)), assertz(state([state3, 0.4], true)), agrees(alice, bob)))).
step(rule((measured(alice, A), amplitude(alice, B)), answer((measured(alice, A), amplitude(alice, B)))), (measured(alice, state1), amplitude(alice, 0.9)), answer((measured(alice, state1), amplitude(alice, 0.9)))).
step(rule((measured(alice, A), amplitude(alice, B)), answer((measured(alice, A), amplitude(alice, B)))), (measured(alice, state1), amplitude(alice, 0.9)), answer((measured(alice, state1), amplitude(alice, 0.9)))).
