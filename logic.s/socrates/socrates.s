:- dynamic('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'/2).
:- dynamic('<http://www.w3.org/2000/01/rdf-schema#subClassOf>'/2).

% Socrates is a human
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<http://example.org/ns#Socrates>', '<http://example.org/ns#Human>').

% humans are mortal
'<http://www.w3.org/2000/01/rdf-schema#subClassOf>'('<http://example.org/ns#Human>', '<http://example.org/ns#Mortal>').

% rdfs subclass
'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:A', '_:B', '_:S'],
    (
        '<http://www.w3.org/2000/01/rdf-schema#subClassOf>'('_:A', '_:B'),
        '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('_:S', '_:A'),
        '<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'([],
            '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('_:S', '_:B')
        )
    )
).

% query
'<http://www.w3.org/2000/10/swap/log#onQuerySurface>'(['_:P', '_:S', '_:O'],
    '_:P'('_:S', '_:O')
).
