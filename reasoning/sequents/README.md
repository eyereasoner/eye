# Sequents

Implications with disjunction in the conclusion giving FOL expressivity.

## Examples:

### all cars are green or blue
{
    ?A a :Car.
} => ($ {
    ?A :is :green.
} {
    ?A :is :blue.
} $).

### negation
{
    ?A a :Car.
    ?A a :Horse.
} => ($ $).
