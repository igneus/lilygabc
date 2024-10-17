#!/usr/bin/env bash

INPUT=../examples/aquam_quam_ego.gabc

echo "= Modern notation"
echo "== LilyPond"
../../bin/gabc2ly $INPUT 2> /dev/null | (diff --color standard.ly - && echo "Output matches")
echo "== Scheme"
../../bin/gabc2scm $INPUT 2> /dev/null | diff --color standard.scm -

# TODO square notation
