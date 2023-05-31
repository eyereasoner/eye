![EYE](https://josd.github.io/images/eye.png)

# RDF Surfaces aka BLOGIC

The EYE reasoner provides an implementation of [RDF Surfaces](https://github.com/w3c-cg/rdfsurfaces), a sublanguage of Notation3 to express a revised RDF logic as envisoned by Pat Hayes in this  2009 ISWC Invited Talk: [BLOGIC](https://www.slideshare.net/PatHayes/blogic-iswc-2009-invited-talk).

RDF Surfaces provides the capabilities to:

- Express a classic negation in RDF (stating that a triple is not true).
- Expressing disjunctions (e.g. stating that `:Alice :likes :Bob` **OR** `:Alice :likes :Charly` (or both) are true).
- Expressing disjunctions as antecedent and consequent in logical implications.
- Explicit scoping of logical quantifiers.
- Contradiction checks in data and rules (blowing a *fuse* when logical contradictions are found).
- Forward and backward chaining.
- Filtering and templating of output results.

In order to run a RDF Surfaces script the EYE reasoner needs to be started with the `--blogic` command line option:

```
eye --quiet --nope --blogic socrates.n3s
```

Note that for running RDF Surfaces scripts no *query* arguments need to be provided to the EYE reasoner. All queries are defined in the RDF surfaces language itself.

The listing `socrates.n3s` below implements the Socrate example in RDF surfaces:

```
@prefix log: <http://www.w3.org/2000/10/swap/log#>.
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
@prefix : <http://example.org/socrates#>.

:Socrates a :Human.
:Human rdfs:subClassOf :Mortal.

(_:S _:A _:B) log:onNegativeSurface {
    _:S a _:A .
    _:A rdfs:subClassOf _:B . 

    () log:onNegativeSurface {
        _:S a _:B .
    } .
} .

# Define what to send to the output
(_:S _:O) log:onQuerySurface {
    _:S a _:O .
} .
```

When running the EYE reasoner on this script the result should be:

```
@prefix log: <http://www.w3.org/2000/10/swap/log#>.
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
@prefix : <http://example.org/socrates#>.

:Socrates a :Human .
:Socrates a :Mortal .
```

## More RDF Surfaces examples

- https://github.com/eyereasoner/eye/tree/master/reasoning/blogic
- https://github.com/eyereasoner/Notation3-By-Example/tree/main/blogic
