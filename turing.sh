#!/bin/bash

ECLIPSE=eclipse # PATH to eclipse prolog (http://www.eclipseclp.org/)
"$ECLIPSE" -f turing.ecl -e main
mpost turing_run
epstopdf turing_run.1
rm -f turing_run.mp turing_run.log turing_run.1
