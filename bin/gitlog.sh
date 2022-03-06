#!/bin/bash
pwd
git status
echo git shortlog --no-pager -s -n HEAD
echo `git shortlog --no-pager -s -n HEAD`
echo Filter
echo `git shortlog --no-pager -s -n HEAD | awk '{$1=""}1' | grep -v "Erik Stenman" | grep -v "Your Name" > $1`
git shortlog --no-pager -s -n HEAD | awk '{$1=""}1' | grep -v "Erik Stenman" | grep -v "Your Name" > $1
