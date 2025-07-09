# eyelet

## eyelet is N3 Logic using RDF Turtle syntax

- eyelet supports reasoning with forward rules described in RDF Turtle
  e.g.
    ```
    # subclass rule
    [ log:graph (
        [ log:triple (var:A rdfs:subClassOf var:B)]
        [ log:triple (var:S rdf:type var:A)]
    )] log:implies [ log:graph (
        [ log:triple (var:S rdf:type var:B)]
    )].
    ```

- eyelet supports reasoning with backward rules described in RDF Turtle
  e.g.
    ```
    # is the age of a person above some duration?
    [ log:graph (
        [ log:triple (var:S :ageAbove var:A)]
    )] log:isImpliedBy [ log:graph (
        [ log:triple (var:S :birthDay var:B)]
        [ log:triple ("" time:localTime var:D)]
        [ log:triple ((var:D var:B) math:difference var:F)]
        [ log:triple (var:F math:greaterThan var:A)]
    )].
    ```

- eyelet supports reasoning with RDF Surfaces described in RDF Turtle
  e.g.
    ```
    # all cars are green or blue
    (_:A) log:onNegativeSurface [ log:graph (
        [ log:triple (_:A rdf:type :Car)]
        [ log:triple (() log:onNegativeSurface [ log:graph (
            [ log:triple (_:A :is :green)]
        )])]
        [ log:triple (() log:onNegativeSurface [ log:graph (
            [ log:triple (_:A :is :blue)]
        )])]
    )].
    ```

- eyelet supports querying with queries described in RDF Turtle
  e.g.
    ```
    # who is a what?
    [ log:graph (
        [ log:triple (var:WHO rdf:type var:WHAT)]
    )] log:impliesAnswer [ log:graph (
        [ log:triple (var:WHO rdf:type var:WHAT)]
    )].
    ```

> [!NOTE]
> A forward rule with `log:implies false` is an inference fuse.

> [!NOTE]
> The `var:` prefix is `<http://www.w3.org/2000/10/swap/var#>` and is used for
  variables that are interpreted universally except for forward rule
  conclusion-only variables which are interpreted existentially.
