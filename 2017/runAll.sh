#!/bin/sh

year=2017

mvn clean package
java -cp target/${year}-1.0.0-SNAPSHOT-jar-with-dependencies.jar pl.touk.dpr.aoc${year}.All
