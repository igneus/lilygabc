#!/usr/bin/env bash

guile generate.scm ../examples/*.gabc |
    lilypond - $INPUT 2> /dev/null |
    sed -E "s/procedure [a-z0-9]+/procedure XXXXXXXX/" |
    (diff --color expected.txt - && echo "Output matches")

# TODO square notation
