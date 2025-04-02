# logos - reasoning with webized prolog

- webized prolog basically means that prolog atoms can be `<` `>` quoted IRIs
- besides top-down reasoning with `conclusion :- premise` rules, it also supports bottom-up reasoning with `conclusion :+ premise` rules
- variables are interpreted universally except for `conclusion :+ premise` conclusion-only variables which are interpreted existentially
- linear implication is done with `becomes(from_conj, to_conj)`
- bottom-up reasoning can use `stable(n)` to fail if the deductive closure at level `n` is not yet stable
- queries are posed as `true :+ premise`
- inference fuses are defined as `false :+ premise`
