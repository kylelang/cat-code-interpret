# -*- Makefile -*-

all: interpret.pdf

interpret.pdf: interpret.tex
	pdflatex interpret.tex
	bibtex interpret.aux
	pdflatex interpret.tex
	pdflatex interpret.tex

interpret.tex: interpret.Rnw
	Rscript -e "library(knitr); knit('interpret.Rnw')"

clean:
	rm -f *.aux *.bbl *.blg *.log
