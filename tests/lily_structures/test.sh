#!/usr/bin/env bash

# See options.scm for available command line options.

set -o xtrace -o noglob
set -e

echo "Generating examples"
guile generate.scm "$@"

echo "Dumping music structures"

bash -c 'cd ../regression && ./process.sh > regression_actual.out' & pid9=$!
guile process.scm "$@"
wait "$pid9"

guile report.scm \
      --report-add ../regression/regression_expected.out:../regression/regression_actual.out \
      "$@"
