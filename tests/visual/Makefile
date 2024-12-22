ly: test.pdf vaticana_test.pdf lower_level_api_test.pdf

# TODO Why on earth does this work in Frescobaldi, but fails here?
test_pdf: test.ly
	lilypond test.ly

vaticana_test.pdf: vaticana_test.ly
	lilypond vaticana_test.ly

lower_level_api_test.pdf: lower_level_api_test.ly
	lilypond lower_level_api_test.ly

TEX_SRC=gregorio_comparison.tex

tex: $(TEX_SRC)
	rm -r tmp-ly && lualatex -shell-escape gregorio_comparison.tex

$(TEX_SRC): gregorio_comparison/*.txt generate.scm
	guile generate.scm gregorio_comparison/*.txt > $(TEX_SRC)
