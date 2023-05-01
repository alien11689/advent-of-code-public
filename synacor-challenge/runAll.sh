#!/bin/bash

mvn clean package > /dev/null

jar=target/synacor-challenge-1.0.0-SNAPSHOT-jar-with-dependencies.jar

echo Coins
java -cp $jar dpr.synacorchallenge.Coins

echo Teleporter
java -cp $jar -Xss100m dpr.synacorchallenge.Teleporter

echo Orb
java -cp $jar dpr.synacorchallenge.Orb

echo Whole
java -cp $jar dpr.synacorchallenge.Main < actions

