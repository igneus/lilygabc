#!/usr/bin/env bash

# See options.scm for available command line options.

set -o xtrace -o noglob

echo "Generating examples"
guile generate.scm "$@"

# Run LilyPond processes in parallel, wait for all to finish
# before reporting results.
echo "Dumping music structures"
lilypond test_expected.ly > test_expected.out & pid1=$!
lilypond test_actual.ly > test_actual.out & pid2=$!

lilypond vaticana_test_expected.ly > vaticana_test_expected.out & pid3=$!
lilypond vaticana_test_actual.ly > vaticana_test_actual.out & pid4=$!

lilypond lower_level_api_test_expected.ly > lower_level_api_test_expected.out & pid5=$!
lilypond lower_level_api_test_actual.ly > lower_level_api_test_actual.out & pid6=$!

wait "$pid1" &&
    wait "$pid2" &&
    wait "$pid3" &&
    wait "$pid4" &&
    wait "$pid5" &&
    wait "$pid6" &&
    guile report.scm "$@"
