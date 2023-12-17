#!/bin/sh

module=$1

for x in `seq -w 1 25`; do
  echo "Day$x"
  grep -ir "answer was" $module/src/main/resources/$x | cut -d ">" -f3 | cut -d "<" -f1
done

