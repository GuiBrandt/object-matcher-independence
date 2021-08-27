# Object matcher independence

An experiment on automated proof of independence of JSON object matchers using a SAT solver.

Basically, this aims to provide an algorithm that, given two "object matchers" defined in
terms of conjunctions (`&&`) and negations (`!`) (and combinations thereof) of attribute
equality tests (e.g. `attribute1 == "value1"`), determines whether there can exist any object
that satisfies both matchers at the same time.

This problem is equivalent to SAT, which is easily seen by replacing all equality tests by
free variables (of course, making sure that any two identical tests map to identical variables
and any two distinct tests do not).
 
## Overview

This implementation essentially converts a given object matcher into a boolean formula and then
uses [minisat-solver](https://hackage.haskell.org/package/minisat-solver-0.1) to solve SAT for it.
This yields a list of assignments that satisfy such formula (if any), which can then be converted
into a list of attribute assignments on the form `attributeX == "valueX"` or `attributeX != "valueX"`
and used to generate examples of objects that satisfy both matchers.

### Object Matchers

An object matcher is either:
- An attribute matcher, i.e. a predicate of the form `x ∈ S` for some attribute named `x` and set
  of strings `S`;
- The conjunction of two or more object matchers;
- The negation of an object matcher.

### Object Matcher ➝ Boolean Formula

It's straightforward to express any object matcher as a boolean formula by replacing attribute
matchers by disjunctions of attribute equalities then attribute equalities by variables.

Since conjunction and negation are [functionally complete][fcomplete], this means that object
matchers are too.

[fcomplete]: https://en.wikipedia.org/wiki/Functional_completeness

Additionally, since equalities are mutually exclusive, an attribute matcher `x ∈ S` implies
`∀ a, b ∈ S (x == a -> x != b)`. When converting object matchers to boolean formulas, this is taken
into account and mutually exclusivity clauses (which we call "functional dependencies") are added to
the formula before solving SAT.

For instance, the attribute matcher `x ∈ {"a", "b"}` would be translated into:
`(x == a -> x != b) ∧ (x == "a" ∨ x == "b")`. Notice that `x == a -> x != b` already implies
`x == b -> x != a`, so there's no need to add it to the formula.

In general, for any given attribute matcher `x ∈ S`, if `k = |S|`, then `k² / 2` functional dependencies
are added to the final formula.
For arbitrary object matchers, `k` is equal to the sum of the sizes of the unions of all values matched
for each attribute.

### Boolean Assignments ➝ Objects

The package used for solving SAT is conveniently parameterized by a type for variables. This means that
we can treat attribute equalities as variables, so it's fairly easy to convert answers from the SAT
problem into attribute equalities/inequalities (if a variable `x == "a"` must be true to satisfy the
formula, then `x = "a"`, otherwise `x != "a"`).

## Usage

Use `stack install` to install the executable into the user path.

The executable `objmind` (acronym for OBJect Matcher INDependence) reads a stream of JSON objects from
the standard input and outputs a report on whether they are independent, and what the maximal independent
sets are, in case they aren't. 

The JSON objects are converted to object matchers by parsing arrays as conjunctions, object of the form
`{"not": {...}}` as negations and objects of the form `{"attribute":"...", "values":[...]}` as attribute
matchers. Additional keys on objects are ignored and negations take priority over attribute matchers. 

For instance:

```shell
$ cat > matchers.in
{"attribute": "x", "values": ["1"]}
[
    {"attribute": "x", "values": ["2"]},
    {"attribute": "y", "values": ["3", "4"]}
]
{"not": {"attribute": "y", "values": ["3"]}}

$ objmind < matchers.in
Found overlapping matchers:

x = "1"
¬(y = "3")

Overlapping instances (at most 10 shown):
 - x = "1", y ≠ "3"

x = "2" ∧ y ∈ {"3", "4"}
¬(y = "3")

Overlapping instances (at most 10 shown):
 - x = "2", y = "4"

Maximal independent sets:
 - x = "1"
   x = "2" ∧ y ∈ {"3", "4"}
 - ¬(y = "3")
```
