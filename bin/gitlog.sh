#!/bin/bash
pwd
git status
echo git --no-pager shortlog -s -n HEAD
echo `git --no-pager shortlog -s -n HEAD`
echo Filter
echo `git --no-pager shortlog -s -n HEAD | awk '{$1=""}1' | grep -v "Erik Stenman" | grep -v "Your Name" > $1`
git --no-pager shortlog -s -n HEAD | awk '{$1=""}1' | grep -v "Erik Stenman" | grep -v "Your Name" > $1
