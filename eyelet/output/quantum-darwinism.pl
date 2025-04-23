% 🌱 New state created: state1 from state1
% 🌱 New state created: state2 from state1
% 🌱 New state created: state3 from state1
:- op(1200, xfx, :+).

answer('urn:example:evolve'(state1,state1)).
answer((retract('urn:example:state'([state3,0.9],true)),assertz('urn:example:state'([state3,0.4],true)),'urn:example:agrees'(alice,bob))).
answer(('urn:example:measured'(alice,state1),'urn:example:amplitude'(alice,0.9))).
answer('urn:example:evolve'(state1,state2)).
answer('urn:example:evolve'(state1,state3)).

step((true:+'urn:example:evolve'(state1,A)),'urn:example:evolve'(state1,state1),true).
step((true:+retract('urn:example:state'([state3,0.9],true)),assertz('urn:example:state'([state3,0.4],true)),'urn:example:agrees'(alice,bob)),(retract('urn:example:state'([state3,0.9],true)),assertz('urn:example:state'([state3,0.4],true)),'urn:example:agrees'(alice,bob)),true).
step((true:+'urn:example:measured'(alice,A),'urn:example:amplitude'(alice,B)),('urn:example:measured'(alice,state1),'urn:example:amplitude'(alice,0.9)),true).
step((true:+'urn:example:evolve'(state1,A)),'urn:example:evolve'(state1,state2),true).
step((true:+'urn:example:evolve'(state1,A)),'urn:example:evolve'(state1,state3),true).
