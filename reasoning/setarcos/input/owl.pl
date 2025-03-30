% OWL RL
% See https://www.w3.org/TR/owl2-profiles/#OWL_2_RL

:- op(1200, xfx, :+).

:- discontiguous((:+)/2).

:- dynamic('http://www.w3.org/2002/07/owl#inverseOf'/2).

% owl:AllDifferent
false :+
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, 'http://www.w3.org/2002/07/owl#AllDifferent'),
    'http://www.w3.org/2002/07/owl#members'(A, B),
    member(C, B),
    member(D, B),
    C \= D,
    'http://www.w3.org/2002/07/owl#sameAs'(C, D).

false :+
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, 'http://www.w3.org/2002/07/owl#AllDifferent'),
    'http://www.w3.org/2002/07/owl#distinctMembers'(A, B),
    member(C, B),
    member(D, B),
    C \= D,
    'http://www.w3.org/2002/07/owl#sameAs'(C, D).

% owl:AllDisjointClasses
false :+
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, 'http://www.w3.org/2002/07/owl#AllDisjointClasses'),
    'http://www.w3.org/2002/07/owl#members'(A, B),
    member(C, B),
    member(D, B),
    C \= D,
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(E, C),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(E, D).

% owl:AllDisjointProperties
false :+
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, 'http://www.w3.org/2002/07/owl#AllDisjointProperties'),
    'http://www.w3.org/2002/07/owl#members'(A, B),
    member(C, B),
    member(D, B),
    C \= D,
    call(C, E, F),
    call(D, E, F).

% owl:AsymmetricProperty
false :+
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, 'http://www.w3.org/2002/07/owl#AsymmetricProperty'),
    call(A, B, C),
    call(A, C, B).

% owl:FunctionalProperty.
'http://www.w3.org/2002/07/owl#sameAs'(A, B) :+
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(C, 'http://www.w3.org/2002/07/owl#FunctionalProperty'),
    call(C, D, A),
    call(C, D, B).

false :+
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, 'http://www.w3.org/2002/07/owl#FunctionalProperty'),
    call(A, B, C),
    call(A, B, D),
    'http://www.w3.org/2002/07/owl#differentFrom'(C, D).

% owl:InverseFunctionalProperty
'http://www.w3.org/2002/07/owl#sameAs'(A, B) :+
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(C, 'http://www.w3.org/2002/07/owl#InverseFunctionalProperty'),
    call(C, A, D),
    call(C, B, D).

false :+
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, 'http://www.w3.org/2002/07/owl#InverseFunctionalProperty'),
    call(A, B, C),
    call(A, D, C),
    'http://www.w3.org/2002/07/owl#differentFrom'(B, D).

% owl:IrreflexiveProperty
false :+
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, 'http://www.w3.org/2002/07/owl#IrreflexiveProperty'),
    call(A, B, B).

% owl:NegativePropertyAssertion
false :+
    'http://www.w3.org/2002/07/owl#sourceIndividual'(A, B),
    'http://www.w3.org/2002/07/owl#assertionProperty'(A, C),
    'http://www.w3.org/2002/07/owl#targetIndividual'(A, D),
    call(C, B, D).

false :+
    'http://www.w3.org/2002/07/owl#sourceIndividual'(A, B),
    'http://www.w3.org/2002/07/owl#assertionProperty'(A, C),
    'http://www.w3.org/2002/07/owl#targetValue'(A, D),
    call(C, B, D).

% owl:Nothing
false :+
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(_, 'http://www.w3.org/2002/07/owl#Nothing').

% owl:SymmetricProperty
Z :+
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, 'http://www.w3.org/2002/07/owl#SymmetricProperty'),
    call(A, C, B),
    Z =.. [A, B, C].

% owl:TransitiveProperty
Z :+
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, 'http://www.w3.org/2002/07/owl#TransitiveProperty'),
    call(A, B, D),
    call(A, D, C),
    Z =.. [A, B, C].

% owl:allValuesFrom
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, B) :+
    'http://www.w3.org/2002/07/owl#allValuesFrom'(C, B),
    'http://www.w3.org/2002/07/owl#onProperty'(C, D),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(E, C),
    call(D, E, A).

'http://www.w3.org/2000/01/rdf-schema#subClassOf'(A, B) :+
    'http://www.w3.org/2002/07/owl#allValuesFrom'(A, C),
    'http://www.w3.org/2002/07/owl#onProperty'(A, D),
    'http://www.w3.org/2002/07/owl#allValuesFrom'(B, E),
    'http://www.w3.org/2002/07/owl#onProperty'(B, D),
    'http://www.w3.org/2000/01/rdf-schema#subClassOf'(C, E).

'http://www.w3.org/2000/01/rdf-schema#subClassOf'(A, B) :+
    'http://www.w3.org/2002/07/owl#allValuesFrom'(A, C),
    'http://www.w3.org/2002/07/owl#onProperty'(A, D),
    'http://www.w3.org/2002/07/owl#allValuesFrom'(B, C),
    'http://www.w3.org/2002/07/owl#onProperty'(B, E),
    'http://www.w3.org/2000/01/rdf-schema#subPropertyOf'(D, E).

% owl:complementOf
'http://www.w3.org/2002/07/owl#complementOf'(A, B) :+
    'http://www.w3.org/2002/07/owl#complementOf'(B, A).

'http://www.w3.org/2002/07/owl#disjointWith'(A, B) :+
    'http://www.w3.org/2002/07/owl#complementOf'(B, A).

false :+
    'http://www.w3.org/2002/07/owl#complementOf'(A, B),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(C, A),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(C, B).

% owl:differentFrom
'http://www.w3.org/2002/07/owl#differentFrom'(A, B) :+
    'http://www.w3.org/2002/07/owl#differentFrom'(B, A).

% owl:disjointUnionOf
'http://www.w3.org/2002/07/owl#disjointWith'(A, B),
    'http://www.w3.org/2002/07/owl#unionOf'(C, D) :+
    'http://www.w3.org/2002/07/owl#disjointUnionOf'(C, D),
    member(A, D),
    member(B, D),
    A \= B.

% owl:disjointWith
'http://www.w3.org/2002/07/owl#differentFrom'(A, B) :+
    'http://www.w3.org/2002/07/owl#disjointWith'(C, D),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, C),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(B, D).

false :+
    'http://www.w3.org/2002/07/owl#disjointWith'(A, B),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(C, A),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(C, B).

% owl:distinctMembers
'http://www.w3.org/2002/07/owl#differentFrom'(A, B) :+
    'http://www.w3.org/2002/07/owl#distinctMembers'(_, C),
    member(A, C),
    member(B, C),
    A \= B.

% owl:equivalentClass
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, B) :+
    'http://www.w3.org/2002/07/owl#equivalentClass'(C, B),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, C).

'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, B) :+
    'http://www.w3.org/2002/07/owl#equivalentClass'(B, C),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, C).

'http://www.w3.org/2002/07/owl#equivalentClass'(A, B) :+
    'http://www.w3.org/2002/07/owl#equivalentClass'(B, A).

'http://www.w3.org/2002/07/owl#equivalentClass'(A, B) :+
    'http://www.w3.org/2002/07/owl#equivalentClass'(A, C),
    'http://www.w3.org/2002/07/owl#equivalentClass'(C, B).

'http://www.w3.org/2000/01/rdf-schema#subClassOf'(A, B),
    'http://www.w3.org/2000/01/rdf-schema#subClassOf'(B, A) :+
    'http://www.w3.org/2002/07/owl#equivalentClass'(A, B).

'http://www.w3.org/2002/07/owl#equivalentClass'(A, B) :+
    'http://www.w3.org/2000/01/rdf-schema#subClassOf'(A, B),
    'http://www.w3.org/2000/01/rdf-schema#subClassOf'(B, A).

% owl:equivalentProperty
Z :+
    'http://www.w3.org/2002/07/owl#equivalentProperty'(D, A),
    call(D, B, C),
    Z =.. [A, B, C].

Z :+
    'http://www.w3.org/2002/07/owl#equivalentProperty'(A, D),
    call(D, B, C),
    Z =.. [A, B, C].

'http://www.w3.org/2002/07/owl#equivalentProperty'(A, B) :+
    'http://www.w3.org/2002/07/owl#equivalentProperty'(B, A).

'http://www.w3.org/2002/07/owl#equivalentProperty'(A, B) :+
    'http://www.w3.org/2002/07/owl#equivalentProperty'(A, C),
    'http://www.w3.org/2002/07/owl#equivalentProperty'(C, B).

'http://www.w3.org/2000/01/rdf-schema#subPropertyOf'(A, B),
    'http://www.w3.org/2000/01/rdf-schema#subPropertyOf'(B, A) :+
    'http://www.w3.org/2002/07/owl#equivalentProperty'(A, B).

'http://www.w3.org/2002/07/owl#equivalentProperty'(A, B) :+
    'http://www.w3.org/2000/01/rdf-schema#subPropertyOf'(A, B),
    'http://www.w3.org/2000/01/rdf-schema#subPropertyOf'(B, A).

% owl:hasKey
'http://www.w3.org/2002/07/owl#sameAs'(A, B) :+
    'http://www.w3.org/2002/07/owl#hasKey'(C, D),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, C),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(B, C),
    A \= B,
    findall(E,
        (   member(E, D),
            call(E, A, F),
            call(E, B, F)
        ),
        G
    ),
    sort(G, H),
    sort(D, H).

% owl:hasValue
Z :+
    'http://www.w3.org/2002/07/owl#hasValue'(D, C),
    'http://www.w3.org/2002/07/owl#onProperty'(D, A),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(B, D),
    Z =.. [A, B, C].

'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, B) :+
    'http://www.w3.org/2002/07/owl#hasValue'(B, C),
    'http://www.w3.org/2002/07/owl#onProperty'(B, D),
    call(D, A, C).

'http://www.w3.org/2000/01/rdf-schema#subClassOf'(A, B) :+
    'http://www.w3.org/2002/07/owl#hasValue'(A, C),
    'http://www.w3.org/2002/07/owl#onProperty'(A, D),
    'http://www.w3.org/2002/07/owl#hasValue'(B, C),
    'http://www.w3.org/2002/07/owl#onProperty'(B, E),
    'http://www.w3.org/2000/01/rdf-schema#subPropertyOf'(D, E).

false :+
    'http://www.w3.org/2002/07/owl#hasValue'(A, B),
    'http://www.w3.org/2002/07/owl#onProperty'(A, C),
    call(C, D, E),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(D, A),
    'http://www.w3.org/2002/07/owl#differentFrom'(B, E).

% owl:intersectionOf
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, B) :+
    'http://www.w3.org/2002/07/owl#intersectionOf'(C, D),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, C),
    member(B, D).

'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, B) :+
    'http://www.w3.org/2002/07/owl#intersectionOf'(B, C),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#first'(C, D),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, D),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest'(C, E),
    findall(F,
        (   member(F, E),
            'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, F)
        ),
        E
    ).

'http://www.w3.org/2000/01/rdf-schema#subClassOf'(A, B) :+
    'http://www.w3.org/2002/07/owl#intersectionOf'(A, C),
    member(B, C).

% owl:inverseOf
Z :+
    'http://www.w3.org/2002/07/owl#inverseOf'(D, A),
    call(D, C, B),
    Z =.. [A, B, C].

Z :+
    'http://www.w3.org/2002/07/owl#inverseOf'(A, D),
    call(D, C, B),
    Z =.. [A, B, C].

'http://www.w3.org/2002/07/owl#inverseOf'(A, B) :+
    'http://www.w3.org/2002/07/owl#inverseOf'(B, A).

% owl:maxCardinality
'http://www.w3.org/2002/07/owl#sameAs'(A, B) :+
    'http://www.w3.org/2002/07/owl#maxCardinality'(C, 1),
    'http://www.w3.org/2002/07/owl#onProperty'(C, D),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(E, C),
    call(D, E, A),
    call(D, E, B).

false :+
    'http://www.w3.org/2002/07/owl#maxCardinality'(A, 0),
    'http://www.w3.org/2002/07/owl#onProperty'(A, B),
    call(B, C, _),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(C, A).

false :+
    'http://www.w3.org/2002/07/owl#maxCardinality'(A, 1),
    'http://www.w3.org/2002/07/owl#onProperty'(A, B),
    call(B, C, D),
    call(B, C, E),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(C, A),
    'http://www.w3.org/2002/07/owl#differentFrom'(E, D).

% owl:maxQualifiedCardinality
'http://www.w3.org/2002/07/owl#sameAs'(A, B) :+
    'http://www.w3.org/2002/07/owl#maxQualifiedCardinality'(C, 1),
    'http://www.w3.org/2002/07/owl#onProperty'(C, D),
    'http://www.w3.org/2002/07/owl#onClass'(C, E),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(F, C),
    call(D, F, A),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, E),
    call(D, F, B),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(B, E).

'http://www.w3.org/2002/07/owl#sameAs'(A, B) :+
    'http://www.w3.org/2002/07/owl#maxQualifiedCardinality'(C, 1),
    'http://www.w3.org/2002/07/owl#onProperty'(C, D),
    'http://www.w3.org/2002/07/owl#onClass'(C, 'http://www.w3.org/2002/07/owl#Thing'),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(E, C),
    call(D, E, A),
    call(D, E, B).

false :+
    'http://www.w3.org/2002/07/owl#maxQualifiedCardinality'(A, 0),
    'http://www.w3.org/2002/07/owl#onProperty'(A, B),
    'http://www.w3.org/2002/07/owl#onClass'(A, A),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(C, A),
    call(B, C, D),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(D, A).

false :+
    'http://www.w3.org/2002/07/owl#maxQualifiedCardinality'(A, 0),
    'http://www.w3.org/2002/07/owl#onProperty'(A, B),
    'http://www.w3.org/2002/07/owl#onClass'(A, 'http://www.w3.org/2002/07/owl#Thing'),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(C, A),
    call(B, C, _).

% owl:oneOf
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, B) :+
    'http://www.w3.org/2002/07/owl#oneOf'(B, C),
    member(A, C).

% owl:propertyChainAxiom
Z :+
    'http://www.w3.org/2002/07/owl#propertyChainAxiom'(A, D),
    propertyChainExtension(D, B, C),
    Z =.. [A, B, C].

propertyChainExtension([A], B, C) :-
    !,
    call(A, B, C).
propertyChainExtension([A|B], C, D) :-
    call(A, C, F),
    propertyChainExtension(B, F, D).

% owl:propertyDisjointWith
false :+
    'http://www.w3.org/2002/07/owl#propertyDisjointWith'(A, B),
    call(A, C, D),
    call(B, C, D).

% owl:sameAs
'http://www.w3.org/2002/07/owl#sameAs'(A, B) :+
    'http://www.w3.org/2002/07/owl#sameAs'(B, A).

'http://www.w3.org/2002/07/owl#sameAs'(A, B) :+
    'http://www.w3.org/2002/07/owl#sameAs'(A, C),
    'http://www.w3.org/2002/07/owl#sameAs'(C, B).

false :+
    'http://www.w3.org/2002/07/owl#sameAs'(A, B),
    'http://www.w3.org/2002/07/owl#differentFrom'(A, B).

Z :+
    'http://www.w3.org/2002/07/owl#sameAs'(D, A),
    call(D, B, C),
    Z =.. [A, B, C].

% owl:someValuesFrom
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, B) :+
    'http://www.w3.org/2002/07/owl#someValuesFrom'(B, C),
    'http://www.w3.org/2002/07/owl#onProperty'(B, D),
    call(D, A, E),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(E, C).

'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, B) :+
    'http://www.w3.org/2002/07/owl#someValuesFrom'(B, 'http://www.w3.org/2002/07/owl#Thing'),
    'http://www.w3.org/2002/07/owl#onProperty'(B, C),
    call(C, A, _).

'http://www.w3.org/2000/01/rdf-schema#subClassOf'(A, B) :+
    'http://www.w3.org/2002/07/owl#someValuesFrom'(A, C),
    'http://www.w3.org/2002/07/owl#onProperty'(A, D),
    'http://www.w3.org/2002/07/owl#someValuesFrom'(B, E),
    'http://www.w3.org/2002/07/owl#onProperty'(B, D),
    'http://www.w3.org/2000/01/rdf-schema#subClassOf'(C, E).

'http://www.w3.org/2000/01/rdf-schema#subClassOf'(A, B) :+
    'http://www.w3.org/2002/07/owl#someValuesFrom'(A, C),
    'http://www.w3.org/2002/07/owl#onProperty'(A, D),
    'http://www.w3.org/2002/07/owl#someValuesFrom'(B, C),
    'http://www.w3.org/2002/07/owl#onProperty'(B, E),
    'http://www.w3.org/2000/01/rdf-schema#subPropertyOf'(D, E).

% owl:unionOf
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, B) :+
    'http://www.w3.org/2002/07/owl#unionOf'(B, C),
    member(D, C),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, D).
'http://www.w3.org/2000/01/rdf-schema#subClassOf'(A, B) :+
    'http://www.w3.org/2002/07/owl#unionOf'(B, C),
    member(A, C).

% test data
'http://www.w3.org/2002/07/owl#inverseOf'('urn:example:p', 'urn:example:q').
'urn:example:p'('urn:example:s', 'urn:example:o').
'http://www.w3.org/2002/07/owl#oneOf'('Size', ['urn:example:large', 'urn:example:medium', 'urn:example:small']).

% query
true :+ 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(_, _).
true :+ 'urn:example:q'(_, _).
