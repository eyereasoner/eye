# eyelet-trigua

## eye reasoning in RDF TriG

- eyelet-trigua supports reasoning with forward rules described in RDF TriG
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

- eyelet-trigua supports reasoning with backward rules described in RDF TriG
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

- eyelet-trigua supports reasoning with queries described in RDF TriG
  e.g.
    ```
    # query for people above 80 years old
    _:bng_3 log:impliesAnswer _:bng_3.

    _:bng_3 {
        var:S :ageAbove "P80Y"^^xsd:duration.
    }
    ```

> [!NOTE]
> A forward rule with `log:implies false` is an inference fuse.

> [!NOTE]
> The `var:` prefix is `<http://www.w3.org/2000/10/swap/var#>` and is used for
  variables that are interpreted universally except for forward rule
  conclusion-only variables which are interpreted existentially.

> [!NOTE]
> Literal subjects are described as e.g. `[] rdf:value "aha"; :p :o.`
