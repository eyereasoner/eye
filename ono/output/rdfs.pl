:- op(1200, xfx, :+).

answer('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(s,'C')).
answer(q(s,o)).

step(('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A,B):+'http://www.w3.org/2000/01/rdf-schema#domain'(C,B),call(C,A,D)),('http://www.w3.org/2000/01/rdf-schema#domain'(p,'C'),call(p,s,o)),'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(s,'C')).
step((A:+'http://www.w3.org/2000/01/rdf-schema#subPropertyOf'(B,C),call(B,D,E),A=..[C,D,E]),('http://www.w3.org/2000/01/rdf-schema#subPropertyOf'(p,q),call(p,s,o),q(s,o)=.."qso"),q(s,o)).
step((true:+'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A,B)),'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(s,'C'),true).
step((true:+q(A,B)),q(s,o),true).
