This repository contains an in-progress port of the original Lightweight Modular
Staging (LMS) library to Scala 3.

Lightweight Modular Staging (LMS) is a runtime code generation approach. 
This framework, LMS-Core, provides a library of core components for building 
high performance code generators and embedded compilers in Scala. 

The original LMS source can be found [here](https://github.com/TiarkRompf/virtualization-lms-core).


### Progress:

Most core functionality has been successfully ported, and basic code generation
works as expected.

#### Missing features:
- Reification of complex data types
- In-process execution of staged code (`eval` and friends)
- Non-Scala backends

#### Bugs:
- Types with nested type parameters are buggy
- Writing overly-generic code is difficult-to-impossible due to changes to
  Scala 3's reflection semantics

#### Limitations:
- Staged mutable variables (e.g., something that should be mutated in the
  generated code) must be type-annotated, e.g.

  ```
  var x: Var[Int] = 1
  ```

  This is due to changes to how Scala 3 handles macros.


### Background:

- [LMS website](http://scala-lms.github.io)

- [LMS paper](http://infoscience.epfl.ch/record/150347/files/gpce63-rompf.pdf)

### How to build:

1. Install the [SBT](http://www.scala-sbt.org/) build tool.

2. Run `sbt test` to run the test suite.

3. Run `sbt publish-local` to install LMS-Core for use in other projects.


### License:

Copyright 2010-2016, EPFL and collaborators. Licensed under the revised BSD License.
