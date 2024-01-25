## Shawn's Abstract Interpreter

This is a repository for testing out ideas for building abstract interpreters.

Currently, we have an interval domain implemented with a simple while language.
Unit tests may be found in `src/test/WhileTest.scala` and run with `sbt test`.

### Example 1: terminating while loop
Below is an example of computing the interval analysis on a simple while program.
This example is printed by the unit test "Test simple terminating while loop".
Initially, each program location has the invariant `⊥` representing an unreachable location.
We first set the initial state of the program to an emtpy memory `{}`.
The analysis then computes abstractions of states at each location with standard abstract interpretation.
Each step is represented as a line below.
The last line is the fixed point representing every possible program state (and possibly some extras).

An interval analysis is used to represent numeric values in a program by an upper and lower bound, `[lower,upper]`.
For example, the abstract state `{x -> [0 , 1]}` represents that the x variable must be between the values of 0 and 1 inclusive.

A later version of this will show how we can handle more complex programs and add some nice features to improve precision.

```
{} Var(x) = Num(1) ⊥ ; WHILE Var(x) DO ⊥ Var(x) = Num(0) ⊥ ELIHW ⊥
{} Var(x) = Num(1) ⊥ ; WHILE Var(x) DO ⊥ Var(x) = Num(0) ⊥ ELIHW ⊥
{} Var(x) = Num(1) ⊥ ; WHILE Var(x) DO ⊥ Var(x) = Num(0) ⊥ ELIHW ⊥
{} Var(x) = Num(1) {x -> [1 , 1]} ; WHILE Var(x) DO ⊥ Var(x) = Num(0) ⊥ ELIHW ⊥
{} Var(x) = Num(1) {x -> [1 , 1]} ; WHILE Var(x) DO ⊥ Var(x) = Num(0) ⊥ ELIHW ⊥
{} Var(x) = Num(1) {x -> [1 , 1]} ; WHILE Var(x) DO {x -> [1 , 1]} Var(x) = Num(0) ⊥ ELIHW ⊥
{} Var(x) = Num(1) {x -> [1 , 1]} ; WHILE Var(x) DO {x -> [1 , 1]} Var(x) = Num(0) ⊥ ELIHW ⊥
{} Var(x) = Num(1) {x -> [1 , 1]} ; WHILE Var(x) DO {x -> [1 , 1]} Var(x) = Num(0) ⊥ ELIHW ⊥
{} Var(x) = Num(1) {x -> [1 , 1]} ; WHILE Var(x) DO {x -> [1 , 1]} Var(x) = Num(0) {x -> [0 , 0]} ELIHW ⊥
{} Var(x) = Num(1) {x -> [1 , 1]} ; WHILE Var(x) DO {x -> [1 , 1]} Var(x) = Num(0) {x -> [0 , 0]} ELIHW ⊥
{} Var(x) = Num(1) {x -> [1 , 1]} ; WHILE Var(x) DO {x -> [1 , 1]} Var(x) = Num(0) {x -> [0 , 0]} ELIHW {x -> [0 , 1]}
{} Var(x) = Num(1) {x -> [1 , 1]} ; WHILE Var(x) DO {x -> [1 , 1]} Var(x) = Num(0) {x -> [0 , 0]} ELIHW {x -> [0 , 1]}
{} Var(x) = Num(1) {x -> [1 , 1]} ; WHILE Var(x) DO {x -> [1 , 1]} Var(x) = Num(0) {x -> [0 , 0]} ELIHW {x -> [0 , 1]}
```

### TODO
[ ] add plus
[ ] Implement back edge detection for widening
[ ] Implement widening
[ ] Implement narrowing
[ ] Extract base transfer function language that can be shared by multiple object langauges (`Step` currently in `WhileInterpretable.scala`)
[ ] Add an abstract domain and find way to compose


### Build tool help

We recommend developing for this project using the Intellij IDE and importing as a SBT project.

You can compile code with `sbt compile`, run it with `sbt run`, test with `sbt test`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).
