#!/bin/bash

git shortlog -s -n HEAD | awk '{$1=""}1' | grep -v "Erik Stenman" | grep -v "Your Name" > $1
