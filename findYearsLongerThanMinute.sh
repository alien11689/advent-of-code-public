#!/bin/bash

for x in 20*/calculatedAnswers.txt; do
	echo $x
	tail -1 $x | grep -e "Finished\|Day" | grep -B1 -e "PT.*M"
done

