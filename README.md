an alternative http4s dsl

I want to explore two things:
- Use rich data structures to build routes. Current http4s dsl uses `PartialFunction` as a building block. `PartialFunction`s are opaque and don't have enough structure for programmers to inspect. In other words, once they are created, not a lot of operations can be done on them, except for linear chaining.
- See if the dsl can benefit from some type level programming.
