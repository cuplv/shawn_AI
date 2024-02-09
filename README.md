## Shawn's Abstract Interpreter

This is a repository for testing out ideas for building abstract interpreters.

Currently, we have an interval domain implemented with a simple while language.
Unit tests may be found in `src/test/WhileTest.scala` and run with `sbt test`.
Unit tests can be a great way to understand the whole project so I suggest starting there.

### Architecture notes
The most important file to look at in the implementation is `src/main/Interpretable.scala`.
This file contains the `Interpreter` class that executes the defined abstract interpretation.
Traits `Loc` and `Interpretable` define an interface to some kind of underlying program (bytecode, AST, etc).
What is exposed from a program is methods for getting the initial location and transitions to other locations in the program.
Additionally, the `toStringWithInvar` is a debugging method that prints the program with the computed invariant (e.g. see the steps in example 1).
Semantics, the abstract domain, and operations such as join and widen are defined by implementing the `Transfer` class.

For example, we can enumerate locations in a simple while program:

`[1] Var(x) = Num(1) [2] ; WHILE Var(x) DO [3] Var(x) = Num(0) [4] ELIHW [5]`

Each location such as `[1]` represents a control flow location before, after, or between atomic steps.
The philosophy of the architecture above is that invariants are a map from location to abstract states.
Transfer goes from a source location to a target location under a given program.

### Example 1: simple terminating while loop
Below is an example of computing the interval analysis on the previous while program.
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

Notes
- The condition on the while loop follows C semantics: a zero value is false and any other value is true.
- This particular example terminates without widening but examples requiring widening will be implemented soon (see TODO).
- The last abstract domain has `x->[0,1]` which is imprecise as we should know `x->[0,0]`.  We need to implement narrowing before this works (see TODO).

### Example 2: terminating while loop requiring widening (TODO)

The following is an example of a while program that terminates but needs widening.
We assume that the `n` value comes from a non-deterministic source such as user input.

`{n -> (∞,∞)} i = 1 ⊥; WHILE i<n ⊥ DO i = i+1 ⊥ ELIHW ⊥`

Without the widening operator we may take the transfer function an arbitrarily large number of times without terminating.

`{n -> (∞,∞)} i = 1 {i->[1,1336], n -> (∞,∞)}; WHILE i<n DO {i->[1,1336], n -> (∞,∞)} i = i+1 {i->[1,1337], n -> (∞,∞)} ELIHW {i->[1,1337], n -> (∞,∞)}`

Since `1337 > T` the transfer and join operators will continuously change the abstract domain on each pass.
The key is to expand the abstraction in a way that terminates in a finite number of steps.
We call this the widen operator and apply it anywhere that there is a back edge in the program.
For example location `[2]` below has an edge backwards from location `[4]` so we widen when joining the state from `[4]`. 

`[1] Var(x) = Num(1) [2] ; WHILE Var(x) DO [3] Var(x) = Num(0) [4] ELIHW [5]`

Resulting in the following abstract state that terminates.


`{n -> (∞,∞)} i = 1 {i->[1,∞], n -> (∞,∞)}; WHILE i<n DO {i->[1,∞], n -> (∞,∞)} i = i+1 {i->[1,∞], n -> (∞,∞)} ELIHW {i->[1,∞], n -> (∞,∞)}`


### TODO
[ ] add plus
[ ] Implement back edge detection for widening
[ ] Implement widening
[ ] Implement narrowing
[ ] Figure out a cleaner way to separate the semantics of a program and the abstract domain/transfer functions
[ ] Extract base transfer function language that can be shared by multiple object langauges (`Step` currently in `WhileInterpretable.scala`)
[ ] Add an abstract domain and find way to compose
[ ] Document how to add object languages
[ ] Document how to add domains


### Build tool help

We recommend developing for this project using the Intellij IDE and importing as a SBT project.

In order to test out the project, we recommend using the docker scripts: 

1. Install docker (Windows/Mac: https://www.docker.com/products/docker-desktop/) (Ubuntu: https://docs.docker.com/engine/install/ubuntu/)
2. Build the docker container with `./buildDocker.sh`
3. Run the docker container with `./runDocker.sh`
4. Note that on Windows you can run the docker commands in these two scripts directly
5. Within the docker container you can run `sbt test` to see the output above and run the unit tests.
 
Other useful sbt commands:
You can compile code with `sbt compile`, run it with `sbt run`, test with `sbt test`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).
