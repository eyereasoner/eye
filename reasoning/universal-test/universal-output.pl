:- op(1200, xfx, :+).

answer('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<https://eyereasoner.github.io/ns#Pat>', '<http://www.w3.org/2000/01/rdf-schema#Resource>')).
answer('<https://eyereasoner.github.io/ns#loves>'('<https://eyereasoner.github.io/ns#Bob>', '<https://eyereasoner.github.io/ns#Lonely>'(skolem('<https://eyereasoner.github.io/ns#Bob>')))).

% proof steps
step((true:+'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<https://eyereasoner.github.io/ns#Pat>', '<http://www.w3.org/2000/01/rdf-schema#Resource>')), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<https://eyereasoner.github.io/ns#Pat>', '<http://www.w3.org/2000/01/rdf-schema#Resource>'), true).
step((true:+'<https://eyereasoner.github.io/ns#loves>'('<https://eyereasoner.github.io/ns#Bob>', _)), '<https://eyereasoner.github.io/ns#loves>'('<https://eyereasoner.github.io/ns#Bob>', '<https://eyereasoner.github.io/ns#Lonely>'(skolem('<https://eyereasoner.github.io/ns#Bob>'))), true).
