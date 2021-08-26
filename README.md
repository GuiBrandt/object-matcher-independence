# [WIP] Object matcher independence

An experiment on automated proof of independence of JSON object matchers using a SAT solver.

Basically, this aims to provide an algorithm that, given two "object matchers" defined in
terms of conjunctions (`&&`) and negations (`!`) (and combinations thereof) of attribute
equality tests (e.g. `attribute1 == "value1"`), determines whether there can exist any object
that satisfies both matchers at the same time.

This problem is equivalent to SAT, which is easily seen by replacing all equality tests by
free variables (of course, making sure that any two identical tests map to identical variables
and any two distinct tests do not).
 
## Overview

This implementation essentially converts a given object matcher into CNF and then uses
[MiniSat](http://minisat.se/) to solve SAT for it.

### Transforming an Object Matcher into CNF

There exist algorithms to convert any logical expression into CNF, for example:
https://www.cs.jhu.edu/~jason/tutorials/convert-to-CNF.html

Since the object matchers are essentially just logical expressions where equality tests are
like free variables, we can use one such algorithm to convert them into CNF.

However, there's one more feature of equality tests that affects how matchers are converted into
logical expressions: they're mutually exclusive.
This means that if `x = (attribute1 == "value1")` and `y = (attribute1 == "value2")`, then
`x -> !y`, and `y -> !x`.

So, besides converting the logical expression corresponding to the object matcher into CNF, we
generate all such mutually exclusivity clauses and prepend them to the generated CNF expression.

As an example, the following matchers:
- `attribute1 ∈ {"value1", "value2"}`
- `attribute2 = "value2" && attribute1 = "value1"`
Give rise to the following CNF expression:
```
(!(attribute1 == "value1") || !(attribute1 == "value2")) && (!(attribute1 == "value1") || !(attribute1 == "value3")) && (!(attribute1 == "value2") || !(attribute1 == "value3")) && (!(attribute1 == "value1") || !(attribute1 == "value2")) && (attribute1 == "value1" || attribute1 == "value2") && (attribute2 == "value2") && (attribute1 == "value3")
```
Which, simplified, would be:
```
(!x || !y) && (!x || !z) && (!y || !z) && (x || y) && w && z
```
In a more readable form:
```
x -> !y
x -> !z
y -> !z
(x || y) && w && z
```
Which is not satisfiable, by the way, because you can't have neither `z && x` nor `z && y`.

Also, notice that the conditions `y -> !x`, `z -> !x` and `z -> !y` are implied by `x -> !y`,
`x -> !z` and `y -> !z`, respectively.
