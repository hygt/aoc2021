## Advent of Code 2021 in Scala 3

This is a modest attempt at solving the Advent of Code 2021 in Scala 3.1,
to get a better feel of the newer language features and the current state of
tooling support.

This also allows me to explore interesting approaches using Cats and other
libraries, whether purely functional or not.

### Setup requirements

Java: 11+

Mill: I recommend using the [wrapper](https://github.com/lefou/millw).

### Run

```shell

$ mill aoc.test.day 1
...
aoc.Test01:
  + part 1 - sample 0.007s
  + part 1 - result: 1466 0.0s
  + part 2 - sample 0.0s
  + part 2 - result: 1491 0.0s
```
