ly: test.pdf vaticana_test.pdf lower_level_api_test.pdf

test.pdf: test.ly
	lilypond test.ly

vaticana_test.pdf: vaticana_test.ly
	lilypond vaticana_test.ly

lower_level_api_test.pdf: lower_level_api_test.ly
	lilypond lower_level_api_test.ly

TEX_SRC=gregorio_comparison_all.tex

tex: $(TEX_SRC)
	rm -r tmp-ly && lualatex -shell-escape $(TEX_SRC)

$(TEX_SRC): gregorio_comparison/*.txt generate.scm
	guile generate.scm gregorio_comparison/*.txt > $(TEX_SRC)
