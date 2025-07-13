# eyelet

## FOL proof engine in N3

### examples:

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
