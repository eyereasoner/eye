# let
- a declarative language based on Prolog syntax, leveraging tree structures both for representing data (as terms) and for program execution (through proof and search trees)
- aka arvol inspired by https://en.wikipedia.org/wiki/Arvol_Looking_Horse and also by the middle letters RVO https://en.wikipedia.org/wiki/Roger_Van_Overstraeten who founded imec
- Beatriz Esteves: it is similar to the way one spells 'tree' in Portuguese, which is also a good analogy, strong roots, strong fruits :)

## let reasoning

- besides top-down reasoning with `conclusion :- premise` rules, let also supports bottom-up reasoning with `conclusion :+ premise` rules
- variables are interpreted universally except for `conclusion :+ premise` conclusion-only variables which are interpreted existentially
- linear implication is done with `becomes(from_conjunction, to_conjunction)`
- bottom-up reasoning can use `stable(n)` to fail if the deductive closure at level `n` is not yet stable
- bottom-up reasoning steps are performed as `step((conclusion :+ premise), premise_inst, conclusion_inst)`
- queries are posed as `true :+ premise` and answered as `answer(premise_inst)`
- inference fuses are defined as `false :+ premise` and blown as `fuse(premise_inst)` with return code 2

## Rationale for bottom-up reasoning

- conclusion can be a conjunction
- conclusion can be `false` to blow an inference fuse
- conclusion can be `true` to pose a query
- conclusion-only variables are existentials
- performing bottom-up proof steps `step/3`
- avoiding loops that could occur with top-down reasoning

## Testing

- run [./test](./test) to go from [./input/](./input/) to [./output/](./output/)

