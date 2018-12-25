#!/bin/bash
time for x in day*; do cd $x; for y in *.groovy; do echo ${x}/${y}; groovy $y ;done ; cd -; done
