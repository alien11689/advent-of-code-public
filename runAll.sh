#!/bin/bash

for x in 2015 2016 2017 2018 2019 2020 2021 2022 synacor-challenge; do
    echo $x
    cd $x
    ./runAll.sh
    cd -
done
