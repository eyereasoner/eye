![EYE](https://josd.github.io/images/eye.png)

# The eye --prolog switch

- Reasoning using webized prolog which basically means that atoms can be `<` `>` quoted IRIs.
- Besides top-down reasoning with `conclusion :- premise` rules, it also does bottom-up reasoning with `conclusion :+ premise` rules.
- Bottom-up reasoning can use `stable(n)` to fail if the deductive closure at level `n` is not yet stable.
- Variables are interpreted as universally quantified variables except for `conclusion :+ premise` conclusion-only variables which are interpreted existentially.
- Queries are posed as `true :+ premise` and answered as `answer(premise_inst)`.
- Inference fuses are defined as `false :+ premise` and blown as `fuse(premise_inst)` with return code 2.

## Rationale for bottom-up reasoning with `conclusion :+ premise` rules

- conclusion can be a conjunction
- conclusion can be `false` to blow an inference fuse
- conclusion can be `true` to pose a query
- conclusion-only variables are existentials
- proving the bottom-up reasoning
- avoiding loops that could occur with top-down reasoning
