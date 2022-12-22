#!/bin/bash

years=${1:="2015 2016 2017 2018 2019 2020 2021 2022"}

for x in $years; do
    cd $x
    ./printAnswers.sh > extractedAnswers.txt
    ./runAll.sh > calculatedAnswers.txt
    cd -
done
