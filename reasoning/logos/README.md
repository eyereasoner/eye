# logos

## Reasoning with logic expressed in SPARQL

- logos supports reasoning with rules expressed in SPARQL as
  e.g.
    ```
    CONSTRUCT {
        ?S a ?B.
    }

    WHERE {
        ?A rdfs:subClassOf ?B.
        ?S a ?A.
    }
    ```

- logos supports querying with queries expressed in SPARQL as
  e.g.
    ```
    CONSTRUCT {
        ?WHO a ?WHAT.
    }

    WHERE {
        ?WHO a ?WHAT.
    }
    ```
