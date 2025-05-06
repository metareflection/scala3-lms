
## Hacks

The fact that trees will need to typecheck _before_ macro rewriting makes
rewriting control flow a bit more annoying. We can kind of cheat by having
an implicit conversion method from `Rep[Bool]` to `Bool` because we'll
rewrite it anyway, but it will make field accesses and especially pattern
matching very difficult.

- Because `Rep` and related types are trait members, it is very difficult to
  pattern-match against them in macros. Currently, we use string-matching,
  which is very brittle.

## Notes from lms-clean

`lms-clean` seems to define a much more general virtualization mechanism
(probably due to porting code from the old scala-virtualized). We should skip
that and generate the code much more directly.

## Compatibility

- vendor EmbeddedControls
- alias `Manifest` and `RefinedManifest` to `ClassTag[T]`

## Manual fixes

- tree-smashed `using` into several places (-rewrite suggested but broken)
- changed `x.toDouble` to use a manual function
