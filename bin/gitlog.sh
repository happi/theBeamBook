#!/bin/bash
git config --global --add safe.directory /__w/theBeamBook/theBeamBook
git --no-pager log | git --no-pager shortlog -s -n | awk '{$1=""}1' | grep -v "Your Name" > $1
