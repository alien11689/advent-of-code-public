#!/bin/sh

year=2015

mvn clean package > /dev/null
java -cp target/${year}-1.0.0-SNAPSHOT-jar-with-dependencies.jar pl.touk.dpr.aoc${year}.All
