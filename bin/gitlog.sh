#!/bin/bash
pwd
git status
echo git --no-pager shortlog -s HEAD
echo `git --no-pager shortlog -s HEAD`
echo Filter
echo `git --no-pager shortlog -s HEAD | sort -n -r | awk '{$1=""}1' | grep -v "Erik Stenman" | grep -v "Your Name" > $1`
git --no-pager shortlog -s HEAD | sort -n -r | awk '{$1=""}1' | grep -v "Erik Stenman" | grep -v "Your Name" > $1
