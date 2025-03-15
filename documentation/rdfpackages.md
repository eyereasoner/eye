# rdfpackages

rdfpackages can contain data, logic and proofs described in RDF.

## forward rules described in RDF
e.g.
```
# rdfs subclass
_:bng_1 log:implies _:bng_2.

_:bng_1 {
    var:A rdfs:subClassOf var:B.
    var:S a var:A.
}

_:bng_2 {
    var:S a var:B.
}
```

## backward rules described in RDF
e.g.
```
# is the age of a person above some duration?
_:bng_1 log:isImpliedBy _:bng_2.

_:bng_1 {
    var:S :ageAbove var:A.
}

_:bng_2 {
    var:S :birthDay var:B.
    [] rdf:value ""; time:localTime var:D.
    (var:D var:B) math:difference var:F.
    var:F math:greaterThan var:A.
}
```

## queries described in RDF
e.g.
```
# query for people above 80 years old
_:bng_3 log:query _:bng_3.

_:bng_3 {
    var:S :ageAbove "P80Y"^^xsd:duration.
}
```

For any rdfpackage with graph statement `N G` the graph term `G` is closed.

The `var:` prefix is `<http://www.w3.org/2000/10/swap/var#>` and is used for
variables that are interpreted as universally quantified variables except for
forward rule conclusion-only variables which are interpreted existentially.

Literal subjects are described as
```
[] rdf:value "aha"; :p :o.
```
