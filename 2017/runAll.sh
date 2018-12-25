#!/bin/bash
time for x in 0* 1* 2*; do cd $x; for y in *.groovy; do echo ${x}/${y}; groovy $y ;done ; cd -; done
