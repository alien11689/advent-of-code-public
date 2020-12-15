#!/bin/sh
mvn clean package

for x in `seq 1 9`; do
  echo "Day $x"
  java -cp target/2015-1.0.0-SNAPSHOT-jar-with-dependencies.jar pl.touk.dpr.aoc2015.day0$x.Main
done

for x in `seq 10 25`; do
  echo "Day $x"
  java -cp target/2015-1.0.0-SNAPSHOT-jar-with-dependencies.jar pl.touk.dpr.aoc2015.day$x.Main
done
