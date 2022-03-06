#!/bin/bash
pwd
git status
echo git shortlog -s -n HEAD
echo `git shortlog -s -n HEAD`
echo Filter
echo `git shortlog -s -n HEAD | awk '{$1=""}1' | grep -v "Erik Stenman" | grep -v "Your Name" > $1`
git shortlog -s -n HEAD | awk '{$1=""}1' | grep -v "Erik Stenman" | grep -v "Your Name" > $1
