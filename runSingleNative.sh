#!/bin/sh

year=$1

./mvnw clean package -Pnative -pl $year -am
./$year/target/AOC$year
