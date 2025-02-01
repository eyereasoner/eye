:- op(1200, xfx, :+).

answer('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(z, n10000)).

% proof steps
step((true:+'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, n10000)), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(z, n10000), true).
