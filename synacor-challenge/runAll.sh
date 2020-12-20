#!/bin/bash

mvn clean package

jar=target/synacor-challenge-1.0.0-SNAPSHOT-jar-with-dependencies.jar

echo Coins
java -cp $jar pl.touk.dpr.synacorchallenge.Coins

echo Teleporter
java -cp $jar -Xss100m pl.touk.dpr.synacorchallenge.Teleporter

echo Orb
java -cp $jar pl.touk.dpr.synacorchallenge.Orb

echo Whole
java -cp $jar pl.touk.dpr.synacorchallenge.Main < actions

