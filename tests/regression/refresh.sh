#!/usr/bin/env bash

guile generate.scm ../examples/*.gabc |
    lilypond - 2> /dev/null |
    sed -E "s/procedure [a-z0-9]+/procedure XXXXXXXX/" > expected.txt
