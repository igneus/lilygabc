# Run unit tests for the pure Scheme modules
test-unit:
	ls tests/unit/*_test.scm | GUILE_LOAD_PATH=. xargs -L1 guile

# Build documents for visual tests
visual_ly:
	cd tests/visual && $(MAKE) ly

# Build document for lilygabc vs. Gregorio visual tests
visual_tex:
	cd tests/visual && $(MAKE) tex

# Run regression tests on their own
# (they are also included in the structural test suite)
test-regression:
	cd tests/regression && ./test.sh

# Dump the visual test examples as LilyPond data structures
# and compare them
test-structures:
	cd tests/lily_structures && ./test.sh --lily-only ../visual/test.ly ../visual/vaticana_test.ly ../visual/error_test.ly ../visual/lower_level_api_test.ly

GABC2LY=bin/gabc2ly
RENDER=bin/rendergabc
INPUT=tests/examples/aquam_quam_ego.gabc

# Smoke test gabc2ly
test-gabc2ly:
	$(GABC2LY) --help
	$(GABC2LY) $(INPUT)
	$(GABC2LY) --scheme $(INPUT)
	$(GABC2LY) --vaticana $(INPUT)
	$(GABC2LY) --vaticana --scheme $(INPUT)

# Smoke test rendergabc
test-rendergabc:
	$(RENDER) -dcrop $(INPUT)
	$(RENDER) --vaticana --svg -dpaper-size="\"a6\"" $(INPUT)

test-scripts: test-gabc2ly test-rendergabc

# All automated tests
test: test-unit test-structures

# All tests, including the particularly slow ones
test-all: test test-gabc2ly
