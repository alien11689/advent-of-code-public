#!/bin/bash

mvn clean package -DskipTests

for x in 2015 2016 2017 2018 2019 2020 2021; do
    echo "========="
    echo Year $x
    echo "========="
    java -cp $x/target/${x}-1.0.0-SNAPSHOT-jar-with-dependencies.jar pl.touk.dpr.aoc$x.All
done
