# TODO Why on earth does this work in Frescobaldi, but fails here?
ly:
	lilypond test.ly

TEX_SRC=gregorio_comparison.tex

tex: $(TEX_SRC)
	rm -r tmp-ly && lualatex -shell-escape gregorio_comparison.tex

$(TEX_SRC): snippets.txt generate.scm
	guile generate.scm > $(TEX_SRC)
