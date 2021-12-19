#!/bin/sh

grep -ir "answer was" src/main/resources | cut -d ">" -f3 | cut -d "<" -f1
