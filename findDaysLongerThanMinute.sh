#!/bin/bash

for x in 20*/calculatedAnswers.txt; do
	echo $x
	sed '$d' $x | grep -e "Finished\|Day" | grep -B1 -e "PT.*M"
done

