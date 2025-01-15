#!/usr/bin/env bash

./process.sh > actual.out
guile ../lily_structures/report.scm expected.txt:actual.out
