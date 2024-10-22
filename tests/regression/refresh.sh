#!/usr/bin/env bash

INPUT=../examples/aquam_quam_ego.gabc

../../bin/gabc2ly $INPUT > standard.ly
../../bin/gabc2scm $INPUT > standard.scm
