#!/usr/bin/env bash

INPUT=../examples/aquam_quam_ego.gabc

echo "= Modern notation"
echo "== LilyPond"
../../bin/gabc2ly $INPUT 2> /dev/null |
    (diff --color standard.ly - && echo "Output matches")
echo "== Scheme"
../../bin/gabc2ly --scheme $INPUT 2> /dev/null |
    sed -E "s/procedure [a-z0-9]+/procedure XXXXXXXX/" |
    (diff --color standard.scm - && echo "Output matches")

# TODO square notation
