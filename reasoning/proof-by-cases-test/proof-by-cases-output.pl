:- op(1200, xfx, :+).

answer('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<https://eyereasoner.github.io/ns#water>', '<https://eyereasoner.github.io/ns#InorganicCompound>')).
answer('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<https://eyereasoner.github.io/ns#water>', '<https://eyereasoner.github.io/ns#Observablee>')).

% proof steps
step(('<https://eyereasoner.github.io/ns#allPossibleCases>'(A, [type(A, '<https://eyereasoner.github.io/ns#Solid>'), type(A, '<https://eyereasoner.github.io/ns#Liquid>'), type(A, '<https://eyereasoner.github.io/ns#Gas>')]):+type(A, '<https://eyereasoner.github.io/ns#InorganicCompound>')), type('<https://eyereasoner.github.io/ns#water>', '<https://eyereasoner.github.io/ns#InorganicCompound>'), '<https://eyereasoner.github.io/ns#allPossibleCases>'('<https://eyereasoner.github.io/ns#water>', [type('<https://eyereasoner.github.io/ns#water>', '<https://eyereasoner.github.io/ns#Solid>'), type('<https://eyereasoner.github.io/ns#water>', '<https://eyereasoner.github.io/ns#Liquid>'), type('<https://eyereasoner.github.io/ns#water>', '<https://eyereasoner.github.io/ns#Gas>')])).
step((true:+'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, _)), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<https://eyereasoner.github.io/ns#water>', '<https://eyereasoner.github.io/ns#InorganicCompound>'), true).
step((true:+'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, _)), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<https://eyereasoner.github.io/ns#water>', '<https://eyereasoner.github.io/ns#Observablee>'), true).
