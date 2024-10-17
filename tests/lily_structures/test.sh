#!/usr/bin/env bash

# See options.scm for available command line options.

echo "Generating examples" &&
    guile generate.scm "$@" &&

    echo "Running LilyPond" &&
    lilypond expected.ly > expected.out &&

    # The GUILE_LOAD_PATH setting is not completely straightforward:
    # the path of `(load "lilygabc.scm")` in lilygabc.ily is expanded
    # to "../../lilygabc.scm" (taking either cwd or the main LilyPond
    # file as a base) and that relative path is then searched
    # relative to all registered load paths.
    GUILE_LOAD_PATH=. lilypond actual.ly > actual.out &&

    guile report.scm "$@"
