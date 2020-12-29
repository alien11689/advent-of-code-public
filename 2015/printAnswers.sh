#!/bin/sh

grep -ir "answer was" . | grep -v target | cut -d ">" -f3 | cut -d "<" -f1
