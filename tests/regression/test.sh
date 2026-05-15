#!/usr/bin/env bash

./process.sh > regression_actual.out

# (There is no regression.ly, but the script uses the filename
# only as pattern of derived file names, and the derived file names
# do exist.)
guile ../lily_structures/report.scm regression.ly
