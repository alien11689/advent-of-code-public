#!/bin/bash
for x in 0*/src 1*/src 2*/src; do 
	echo 
	cd $x
	for y in *.groovy; do 
		echo 
		time groovy $y
	done
	cd -
done
