test:
	GUILE_LOAD_PATH=. guile tests/unit/*_test.scm

test_visual:
	GUILE_LOAD_PATH=. lilypond -dcompile-scheme-code -ddebug-eval tests/visual/test.ly
