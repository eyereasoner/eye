# eyelet

## reasoning with logic described in RDF Core

- eyelet supports reasoning with forward rules described in RDF Core as
  e.g.
    ```
    # subclass rule
    [ log:and (
        [ log:triple (var:A rdfs:subClassOf var:B)]
        [ log:triple (var:S rdf:type var:A)]
    )] log:implies [ log:and (
        [ log:triple (var:S rdf:type var:B)]
    )].
    ```

- eyelet supports reasoning with backward rules described in RDF Core as
  e.g.
    ```
    # is the age of a person above some duration?
    [ log:and (
        [ log:triple (var:S :ageAbove var:A)]
    )] log:isImpliedBy [ log:and (
        [ log:triple (var:S :birthDay var:B)]
        [ log:triple ("" time:localTime var:D)]
        [ log:triple ((var:D var:B) math:difference var:F)]
        [ log:triple (var:F math:greaterThan var:A)]
    )].
    ```

- eyelet supports querying with queries described in RDF Core as
  e.g.
    ```
    # who is a what?
    [ log:and (
        [ log:triple (var:WHO rdf:type var:WHAT)]
    )] log:query [ log:and (
        [ log:triple (var:WHO rdf:type var:WHAT)]
    )].
    ```

> [!NOTE]
> A forward rule with `log:implies false` is an inference fuse.

> [!NOTE]
> The `var:` prefix is `<http://www.w3.org/2000/10/swap/var#>` and is used for
  variables that are interpreted universally except for forward rule
  conclusion-only variables which are interpreted existentially.
