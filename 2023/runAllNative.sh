#!/bin/sh

year=2023

../mvnw clean package -Pnative -pl $year -am -f ../pom.xml
./target/AOC$year
