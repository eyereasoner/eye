:- op(1200, xfx, :+).

answer('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(large,'Size')).
answer('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(medium,'Size')).
answer('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(small,'Size')).
answer(q(o,s)).

step((A:+'http://www.w3.org/2002/07/owl#inverseOf'(B,C),call(B,D,E),A=..[C,E,D]),('http://www.w3.org/2002/07/owl#inverseOf'(p,q),call(p,s,o),q(o,s)=.."qos"),q(o,s)).
step(('http://www.w3.org/2002/07/owl#inverseOf'(A,B):+'http://www.w3.org/2002/07/owl#inverseOf'(B,A)),'http://www.w3.org/2002/07/owl#inverseOf'(p,q),'http://www.w3.org/2002/07/owl#inverseOf'(q,p)).
step(('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A,B):+'http://www.w3.org/2002/07/owl#oneOf'(B,C),member(A,C)),('http://www.w3.org/2002/07/owl#oneOf'('Size',[large,medium,small]),member(large,[large,medium,small])),'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(large,'Size')).
step(('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A,B):+'http://www.w3.org/2002/07/owl#oneOf'(B,C),member(A,C)),('http://www.w3.org/2002/07/owl#oneOf'('Size',[large,medium,small]),member(medium,[large,medium,small])),'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(medium,'Size')).
step(('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A,B):+'http://www.w3.org/2002/07/owl#oneOf'(B,C),member(A,C)),('http://www.w3.org/2002/07/owl#oneOf'('Size',[large,medium,small]),member(small,[large,medium,small])),'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(small,'Size')).
step((true:+'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A,B)),'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(large,'Size'),true).
step((true:+'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A,B)),'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(medium,'Size'),true).
step((true:+'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A,B)),'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(small,'Size'),true).
step((true:+q(A,B)),q(o,s),true).
