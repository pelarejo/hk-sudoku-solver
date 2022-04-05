# sudoku-solver

## Getting Started

This project is created with stack.
To get started run:

```sh
    stack build
    stack exec -- sudoku-solver-exe "./grids/easy.txt"
```

Other examples can be found in the grids folder.

## Future Improvement

1. The Reader is capable of handeling better error reporting.
1. During debugging, a Writer monad could have been usefull to log the algorithm.
1. We could migrate the choices management to a State monad, it would be a great exercice.
