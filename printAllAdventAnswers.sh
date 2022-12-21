#!/bin/bash

for x in 2015 2016 2017 2018 2019 2020 2021 2022; do
    echo "========="
    echo Year $x
    echo "========="
    cd $x
    ./printAnswers.sh
    cd -
done
