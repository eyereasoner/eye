![EYE](https://josd.github.io/images/eye.png)

# language to reason - latre

## triptych

- backward rules use a left arrow `<=`
- forward rules use a right arrow `=>`
- queries use a topward arrow `=^`

## variables

- `_:x` blank nodes in triples and quads
- `?x` quickvars in rules and queries
- quantified `var:x` variables in proofs

## Socrates example

```
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
@prefix : <http://example.org/#>.

:Socrates a :Human.
:Human rdfs:subClassOf :Mortal.

{?S a ?B} <= {?A rdfs:subClassOf ?B. ?S a ?A}.
{?A rdfs:subClassOf ?B. ?S a ?A} => {?S a ?B}.

# query
{?S a ?B} =^ {?S a ?B}.
```

gives answer

```
@prefix : <http://example.org/#>.

:Socrates a :Human.
:Socrates a :Mortal.
```

with proof https://github.com/eyereasoner/eye/blob/master/reasoning/socrates/socrates-proof.ltr
