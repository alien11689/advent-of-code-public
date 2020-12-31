#!/bin/bash

echo -n "Days: "
for x in `find . -name "*.groovy" | grep -v target`; do wc -l $x ;done | sort -k 2 | grep "2018\|2019" | cut -d\  -f2 | cut -d/ -f 2,6 | sort -u | wc -l

echo -n "Files: "
find . -name "*.groovy" | grep -v target| wc -l
echo -n "Lines: "
for x in `find . -name "*.groovy" | grep -v target`; do cat $x; done | wc -l

