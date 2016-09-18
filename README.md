Random Access Zippers
=====================

*This is untested WIP open to issues and PRs.*

A data structure to represent sequences with fast random access.

`Data.Raz.Sequence` is intended to provide the same interface as
`Data.Sequence` from [`containers`](http://hackage.haskell.org/package/containers).

`Data.Raz.Core` is the actual zipper module.
It is a simple translation of the [OCaml implementation](https://github.com/cuplv/raz.ocaml)
the original paper appeared with.

- [The Random Access Zipper: Simple, Purely-Functional Sequences](https://arxiv.org/abs/1608.06009)
