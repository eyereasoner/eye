# eyelet-clogic

## reasoning with coherent logic

## examples:

```
# all cars are green or blue
{
    ?A a :Car.
} => ($ {
    ?A :is :green.
} {
    ?A :is :blue.
} $).
```
```
# negation
{
    ?A a :Car.
    ?A a :Horse.
} => ($ $).
```
