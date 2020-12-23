#!/bin/bash

echo -n "Files: "
find . -name "*.groovy" | grep -v target| wc -l
echo -n "Lines: "
for x in `find . -name "*.groovy" | grep -v target`; do cat $x; done | wc -l
