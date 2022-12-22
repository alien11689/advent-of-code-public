#!/bin/bash

input=$1
years=${input:="2015 2016 2017 2018 2019 2020 2021 2022"}

for x in $years; do
    echo "========="
    echo Year $x
    echo "========="
    cd $x
    ./printAnswers.sh > extractedAnswers.txt
    ./runAll.sh > calculatedAnswers.txt
    cd -
done
