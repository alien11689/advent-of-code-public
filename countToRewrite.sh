#!/bin/bash

find . -name "*.groovy" | grep -v target| wc -l

