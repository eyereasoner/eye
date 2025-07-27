% 🌱 New state created: state1 from state1
evolve(state1,state1).
retract(state([state3,0.9],true)).
assertz(state([state3,0.4],true)).
agrees(alice,bob).
measured(alice,state1).
amplitude(alice,0.9) .
