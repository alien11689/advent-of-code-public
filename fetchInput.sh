#!/bin/sh

year=$1
day=$2
dest=$3
session=`cat ~/.adventofcode`

curl https://adventofcode.com/$year/day/$day/input --cookie "session=$session" -o $dest
