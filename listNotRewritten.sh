#!/bin/sh

for x in `find . -name "*.groovy" | grep -v target`; do wc -l $x ;done | sort -k 2 
