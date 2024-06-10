![EYE](https://josd.github.io/images/eye.png)

# language to reason - latre

## triptych

- backward rules use a left arrow `<=`
- forward rules use a right arrow `=>`
- queries use a topward arrow `=^`

## variables

- `_:x` blank nodes in data and quads
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
and proof
```
@prefix skolem: <https://eyereasoner.github.io/.well-known/genid/8b98b360-9a70-4845-b52c-c675af60ad01#>.
@prefix r: <http://www.w3.org/2000/10/swap/reason#>.
@prefix : <http://example.org/#>.
@prefix n3: <http://www.w3.org/2004/06/rei#>.
@prefix var: <http://www.w3.org/2000/10/swap/var#>.
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.

skolem:proof a r:Proof, r:Conjunction;
    r:component skolem:lemma1;
    r:component skolem:lemma2;
    r:gives {
        :Socrates a :Human.
        :Socrates a :Mortal.
    }.

skolem:lemma1 a r:Inference;
    r:gives {
        :Socrates a :Human.
    };
    r:evidence (
        skolem:lemma3
    );
    r:binding [ r:variable [ n3:uri "http://www.w3.org/2000/10/swap/var#x_0"]; r:boundTo [ n3:uri "http://example.org/#Socrates"]];
    r:binding [ r:variable [ n3:uri "http://www.w3.org/2000/10/swap/var#x_1"]; r:boundTo [ n3:uri "http://example.org/#Human"]];
    r:rule skolem:lemma4.

skolem:lemma2 a r:Inference;
    r:gives {
        :Socrates a :Mortal.
    };
    r:evidence (
        skolem:lemma5
    );
    r:binding [ r:variable [ n3:uri "http://www.w3.org/2000/10/swap/var#x_0"]; r:boundTo [ n3:uri "http://example.org/#Socrates"]];
    r:binding [ r:variable [ n3:uri "http://www.w3.org/2000/10/swap/var#x_1"]; r:boundTo [ n3:uri "http://example.org/#Mortal"]];
    r:rule skolem:lemma4.

skolem:lemma3 a r:Extraction;
    r:gives {
        :Socrates a :Human.
    };
    r:because [ a r:Parsing; r:source <https://eyereasoner.github.io/eye/reasoning/socrates/socrates.ltr>].

skolem:lemma4 a r:Extraction;
    r:gives {
        @forAll var:x_0, var:x_1. {
            var:x_0 a var:x_1.
        } => {
            var:x_0 a var:x_1.
        }.
    };
    r:because [ a r:Parsing; r:source <https://eyereasoner.github.io/eye/reasoning/socrates/socrates.ltr>].

skolem:lemma5 a r:Inference;
    r:gives {
        :Socrates a :Mortal.
    };
    r:evidence (
        skolem:lemma6
        skolem:lemma3
    );
    r:binding [ r:variable [ n3:uri "http://www.w3.org/2000/10/swap/var#x_0"]; r:boundTo [ n3:uri "http://example.org/#Human"]];
    r:binding [ r:variable [ n3:uri "http://www.w3.org/2000/10/swap/var#x_1"]; r:boundTo [ n3:uri "http://example.org/#Mortal"]];
    r:binding [ r:variable [ n3:uri "http://www.w3.org/2000/10/swap/var#x_2"]; r:boundTo [ n3:uri "http://example.org/#Socrates"]];
    r:rule skolem:lemma7.

skolem:lemma6 a r:Extraction;
    r:gives {
        :Human rdfs:subClassOf :Mortal.
    };
    r:because [ a r:Parsing; r:source <https://eyereasoner.github.io/eye/reasoning/socrates/socrates.ltr>].

skolem:lemma7 a r:Extraction;
    r:gives {
        @forAll var:x_0, var:x_1, var:x_2. {
            var:x_2 a var:x_1.
        } <= {
            var:x_0 rdfs:subClassOf var:x_1.
            var:x_2 a var:x_0.
        }.
    };
    r:because [ a r:Parsing; r:source <https://eyereasoner.github.io/eye/reasoning/socrates/socrates.ltr>].
```
