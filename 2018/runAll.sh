#!/bin/sh

year=2018

mvn clean package > /dev/null
java -cp target/${year}-1.0.0-SNAPSHOT-jar-with-dependencies.jar dpr.aoc${year}.All
