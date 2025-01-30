:- op(1200, xfx, :+).

answer(('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, good(cobbler)):+true)).

% proof steps
step(('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, good(cobbler)):+true), true, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(sk_0, good(cobbler))).
step((true:+('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, good(_)):+true)), ('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, good(cobbler)):+true), true).
