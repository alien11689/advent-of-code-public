#!/bin/bash

input=$1
modules=${input:="2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025 euler-project synacor-challenge"}

for x in $modules; do
    echo "========="
    echo Running $x
    echo "========="
    ./runSingle.sh $x | tee $x/calculatedAnswers.txt
done
