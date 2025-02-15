:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#is>'('<https://eyereasoner.github.io/ns#test>', true)).

% proof steps
step((('<https://eyereasoner.github.io/ns#is>'('<https://eyereasoner.github.io/ns#test>', true):+'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, '<https://eyereasoner.github.io/ns#Dog>')):+'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, '<https://eyereasoner.github.io/ns#Cat>')), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<https://eyereasoner.github.io/ns#Minka>', '<https://eyereasoner.github.io/ns#Cat>'), ('<https://eyereasoner.github.io/ns#is>'('<https://eyereasoner.github.io/ns#test>', true):+'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, '<https://eyereasoner.github.io/ns#Dog>'))).
step(('<https://eyereasoner.github.io/ns#is>'('<https://eyereasoner.github.io/ns#test>', true):+'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, '<https://eyereasoner.github.io/ns#Dog>')), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<https://eyereasoner.github.io/ns#Charly>', '<https://eyereasoner.github.io/ns#Dog>'), '<https://eyereasoner.github.io/ns#is>'('<https://eyereasoner.github.io/ns#test>', true)).
step((true:+'<https://eyereasoner.github.io/ns#is>'('<https://eyereasoner.github.io/ns#test>', true)), '<https://eyereasoner.github.io/ns#is>'('<https://eyereasoner.github.io/ns#test>', true), true).
