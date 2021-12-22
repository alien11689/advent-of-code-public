#!/bin/sh


for x in `seq 1 9`; do
  echo "Day $x"
  grep -ir "answer was" src/main/resources/0$x | cut -d ">" -f3 | cut -d "<" -f1
done

for x in `seq 10 25`; do
  echo "Day $x"
  grep -ir "answer was" src/main/resources/$x | cut -d ">" -f3 | cut -d "<" -f1
done

