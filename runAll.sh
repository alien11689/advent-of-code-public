#!/bin/bash

input=$1
years=${input:="2015 2016 2017 2018 2019 2020 2021 2022 2023 euler-project synacor-challenge"}

for x in $years; do
    echo "========="
    echo Running $x
    echo "========="
    ./runSingle.sh $x
done
