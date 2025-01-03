# Prolog with IRI atoms - plato

- Besides top-down reasoning with `conclusion :- premise` rules, plato also does bottom-up reasoning with `conclusion :+ premise` rules.
- Bottum-up reasoning can use `stable(n)` to fail if the deductive closure at level `n` is not yet stable.
- Proofs steps are `step((conclusion :+ premise), premise_inst, conclusion_inst)` and `conclusion_inst` is asserted.
- Variables are interpreted as universally quantified variables except for `conclusion :+ premise` conclusion-only variables which are interpreted existentially.
- Queries are posed as `true :+ premise` and answered as `answer(premise_inst)`.
- Inference fuses are defined as `false :+ premise` and blown as `fuse(premise_inst)` with return code 2.

## Rationale for bottom-up reasoning

- conclusion can be a conjunction
- conclusion can be `false` to blow an inference fuse
- conclusion can be `true` to pose a query
- conclusion-only variables are existentials
- generating proofs using `step/3` proof steps
- avoiding loops that could occur with top-down reasoning
