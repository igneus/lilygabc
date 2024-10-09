test:
	GUILE_LOAD_PATH=. guile tests/unit/*_test.scm

test_visual:
	lilypond -dcompile-scheme-code -ddebug-eval tests/visual/test.ly
