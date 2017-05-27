# Type Theory

Basic usages of dependent types.

`Lectures/Printf.idr` contains simple example of type-safe implementation of `sprintf` function.

## Formal proofs

One of the main reasons for introducing Idris programming language during this course is to use it for formal proofs based on dependent types and Curry-Howard correspondence.

`Setoid.idr` defines setoid used in `Lectures/Diaconescu.idr` to prove Diaconescu theorem and in `Homework`. `Homework/MyInt.idr` defines an integer number as a difference of two natural numbers, `Homework/Rat.idr` defines a rational number as a fraction. Both files introduce equivalence relations, prove their reflexivity, symmetry, and transitivity properties, and define setoid instances.
