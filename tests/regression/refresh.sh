#!/usr/bin/env bash

INPUT=../examples/aquam_quam_ego.gabc

../../bin/gabc2ly $INPUT > standard.ly
../../bin/gabc2ly --scheme $INPUT | sed -E "s/procedure [a-z0-9]+/procedure XXXXXXXX/" > standard.scm
