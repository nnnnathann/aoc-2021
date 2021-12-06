Advent of Code 2021
=========== 

Running the programs:

You will need Haskell tooling, which you can
install via ghcup, or nix, or vscode extension

`cabal run day01`


Running tests:

`cabal test`

Code Structure
==============

```
.
├── days          # entrypoints for the day programs (main :: IO ())
├── dist-newstyle # build output
├── inputs        # text file input to the puzzles
├── lib           # main library functions (puzzle code)
├── specs         # printouts from the advent of code website
└── test          # tests
```