#!/usr/bin/env bash

# See options.scm for available command line options.

set -o xtrace -o noglob

echo "Generating examples"
guile generate.scm "$@"

# Run LilyPond processes in parallel, wait for all to finish
# before reporting results.
echo "Running LilyPond"
lilypond expected.ly > expected.out & pid1=$!

# The GUILE_LOAD_PATH setting is not completely straightforward:
# the path of `(load "lilygabc.scm")` in lilygabc.ily is expanded
# to "../../lilygabc.scm" (taking either cwd or the main LilyPond
# file as a base) and that relative path is then searched
# relative to all registered load paths.
GUILE_LOAD_PATH=. lilypond actual.ly > actual.out & pid2=$!

wait "$pid1" &&
    wait "$pid2" &&
    guile report.scm "$@"
