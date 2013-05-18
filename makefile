
all: single multiple
single: sudoku-solver
multiple: concurrent

sudoku-solver: sudoku-solver.hs
	ghc sudoku-solver.hs

concurrent: concurrent.hs
	ghc concurrent.hs

run: concurrent
	./concurrent < concurrent.in
