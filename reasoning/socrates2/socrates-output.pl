:- op(1200, xfx, :+).

answer('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('Socrates', 'Human')).
answer('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('Socrates', 'Mortal')).

% proof steps
step((type(A, 'Mortal'):+type(A, 'Human')), type('Socrates', 'Human'), type('Socrates', 'Mortal')).
step((true:+'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, _)), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('Socrates', 'Human'), true).
step((true:+'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, _)), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('Socrates', 'Mortal'), true).
