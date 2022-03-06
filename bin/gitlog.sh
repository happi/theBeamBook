#!/bin/bash
git --no-pager log HEAD | git --no-pager shortlog -s -n
git --no-pager log HEAD
echo `git --no-pager shortlog -s HEAD`
echo Filter
echo `git --no-pager shortlog -s HEAD | sort -n -r | awk '{$1=""}1' | grep -v "Erik Stenman" | grep -v "Your Name" > $1`
git --no-pager log HEAD | git --no-pager shortlog -s -n | awk '{$1=""}1' | grep -v "Erik Stenman" | grep -v "Your Name" > $1
