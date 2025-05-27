# eyelet-sparql

- assert triples
    ```
    CONSTRUCT {
        triples
    } WHERE {}
    ```

- infer triples either with `--sparql-forward` or `--sparql-backward`
    ```
    CONSTRUCT {
        triples
    } WHERE {
        premise
    }
    ```

- detect inconsistencies
    ```
    CONSTRUCT {
        (vars) log:allPossibleCases ().
    } WHERE {
        inconsistency
    }
    ```

- answer queries with `--query`
    ```
    CONSTRUCT {
        answer
    } WHERE {
        query
    }
    ```
