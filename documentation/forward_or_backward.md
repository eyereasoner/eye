![EYE](https://josd.github.io/images/eye.png)

# Forward or backward 

The EYE reasoner can be used to create _forward chaining_ and _backward chaining_ rules. 

## Forward chaining

A forward rule tries to find out which rules fits the data. When such 
a rule is found, then the reasoner executes the consequence of rule and 
produces more data (this is done in a loop until no more data can be created).

For instance, if we have as input data:

```(turtle)
@prefix : <http://example.org/ns#>.

:Alice :plays :Piano .        #[T1]
:Bob :likes :Walking .        #[T2]
:Charly :writes :Music .      #[T3]
```

and three rules:

```
{ ?S :plays :Piano . } => { ?S a :PianoPlayer . } . #[R1]
{ ?S :likes :Walking . } => { ?S a :Walker . } .    #[R2]
{ ?S :writes :Music . } => { ?S a :Composer . } .   #[R3]
``` 

then

- `T1` will match rule `R1` 
- `T2` will match rule `R2` 
- `T3` will match rule `R3` 

The  EYE reasoner will generate for each of these rules new data (and the process starts all over again):

```
:Alice a :PianoPlayer .
:Bob a :Walker .
:Charly a :Composer .
```

Forwards chaining is repeated; matching against new data, until no new data is produced.
## Backward chaining

A backward rule works in the opposite direction: the EYE reasoner tries to find out which data fits a rule. When such a fit is found, then the rule holds. Backward rules can be used to add user defined "built-ins". As an example, consider the following backward rule:

```
{
    ?S :is :Musical .
}
<=
{
    ?S :plays :Piano .
}
```

The backward holds when the data contains a match for `?S :plays :Piano`. The backward rule doesn't produce any new data, but can be used in forward rules as a new "built-in":

```
{
    ?S :is :Musical .   # Using our new backward rule
}
=>
{
    ?S :loves :Music .
}
```

The example backward rule is trivial and could also have been written as a forward rule. But, these backward rules can become quite complex and can contain calculations and recursions that are not easily be written as forward rules. One can think of backward rules as rules in Prolog:

```
# Prolog
Head :- Body

# N3
{ Head-Triple } <= { Body-Triples } .
```

Where the `Head-Triple` should contain one and only one triple pattern (which can contain variables), and the `Body-Triple` can contain one or more triples (or other built-ins, backward rules) or the `true` or `false` boolean.

Backward rules can have multiple signatures. E.g., the backward rule below can be used to check if a number is even or odd:

```
@prefix : <http://example.org/ns#>.
@prefix math: <http://www.w3.org/2000/10/swap/math#> .

{
    ?S :isEven true .
}
<=
{
    (?S 2) math:remainder 0 .
} .

{
    ?S :isEven false .
}
<=
{
    (?S 2) math:remainder 1 .
} .
```

This rule can be used in a forward rule as follows:

```
{ 2 :isEven true .  } => { 2 :is :Even .  } .
{ 17 :isEven false .  } => { 17 :is :Odd .  } .
```

The `reasoning/` directory contains many examples of backward rules with arbitrary complexity.