#!/bin/sh

year=2021

mvn clean package

for x in `seq 1 9`; do
  echo "Day $x"
  java -cp target/${year}-1.0.0-SNAPSHOT-jar-with-dependencies.jar pl.touk.dpr.aoc${year}.Day0$x
done

for x in `seq 10 25`; do
  echo "Day $x"
  java -cp target/${year}-1.0.0-SNAPSHOT-jar-with-dependencies.jar pl.touk.dpr.aoc${year}.Day$x
done
