#!/bin/sh

year=$1
day=$2

session=`cat ~/.adventofcode`
BASEDIR=$(dirname "$0")
dest=${BASEDIR}/$year/src/main/resources/`printf '%02d' $day`/input.txt

curl https://adventofcode.com/$year/day/$day/input --cookie "session=$session" -o $dest
