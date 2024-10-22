# Run unit tests for the pure Scheme modules
test-unit:
	ls tests/unit/*_test.scm | GUILE_LOAD_PATH=. xargs -L1 guile

# Build documents for visual tests
visual_ly:
	cd tests/visual && make ly

# Build document for lilygabc vs. Gregorio visual tests
visual_tex:
	cd tests/visual && make tex

# Run regression tests
test-regression:
	cd tests/regression && ./test.sh

# Dump the visual test examples as LilyPond data structures
# and compare them
test-structures:
	cd tests/lily_structures && ./test.sh --lily-only

# All automated tests
test: test-unit test-structures test-regression
