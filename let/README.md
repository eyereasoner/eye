# eyelet

- assert triples with `--sparql-forward`
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

- detect inconsistencies with `--sparql-fuse`
    ```
    CONSTRUCT {} WHERE {
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
