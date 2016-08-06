# [piece-table][https://github.com/adinapoli/piece-table]

This package is a work-in-progress exploration of the `Piece Table`
(aka Piece Chain) data structure, suitable as a backend for text
editors. It was created mostly to teach me this simple but effective
data structure, and to see how well it performs on Haskell.

The implementation tries to follow:

https://www.cs.unm.edu/~crowley/papers/sds.pdf

More sophisticated implementations could be achieved (example using
a RB-tree to store the pieces), but I suspect that in practice using
`Seq` from `Data.Sequence` is enough, as it's using Finger Trees under
the hood.

## What's done

- [x] Loading content via `mmap`
- [x] Partial mapping of files into memory with a `ViewPort`.
- [x] Efficient storage of buffers via `Data.Sequence` and `Vector`.
- [x] Insertions (single and multiple)
- [x] Debug rendering

## What's left

- [ ] Get rid of `unsafe` calls in `unsafeRender`
- [ ] Efficiently render only piece of text with a `ViewPort`
- [ ] Deletions
- [ ] Efficient updates
- [ ] Caching
- [ ] Come up with a good story for text encoding (or lack thereof?)
- [ ] Efficiently "commits" of the table into memory (aka saving you work).

# Build Instructions

``` sh
# Build the project.
stack build

# Run the test suite.
stack test

# Run the benchmarks.
stack bench

# Generate documentation.
stack haddock
```

Thanks again, and happy hacking!

[piece-table]: https://github.com/adinapoli/piece-table
