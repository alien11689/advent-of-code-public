#!/bin/bash

input=$1
years=${input:="2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025"}

for x in $years; do
    echo "========="
    echo Year $x
    echo "========="
    ./printSingleAdventAnswers.sh $x > $x/extractedAnswers.txt
    ./runSingle.sh $x | tee $x/calculatedAnswers.txt
    grep -v "Finished in" $x/calculatedAnswers.txt > $x/calculatedAnswersWithoutTime.txt
    diff $x/calculatedAnswersWithoutTime.txt $x/extractedAnswers.txt
done
