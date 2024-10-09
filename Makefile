test:
	ls tests/unit/*_test.scm | GUILE_LOAD_PATH=. xargs -L1 guile

test_visual:
	lilypond -dcompile-scheme-code -ddebug-eval tests/visual/test.ly
