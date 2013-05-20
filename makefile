
all: single multiple
single: sudoku-solver
multiple: concurrent
explicit: explicit-count

sudoku-solver: sudoku-solver.hs
	ghc sudoku-solver.hs

concurrent: concurrent.hs
	ghc concurrent.hs

explicit-count: explicit-count.hs
	ghc explicit-count.hs

declarative-count: declarative-count.hs
	ghc declarative-count.hs

run: concurrent
	./concurrent < concurrent.in

show: verslag.pdf
	mupdf $<

verslag.pdf: verslag.tex
	pdflatex $<
