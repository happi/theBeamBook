#!/bin/bash

# Directory containing chapter markdown files
CHAPTERS_DIR="chapters"

# Loop through each Markdown file and count words
for chapter in $(ls $CHAPTERS_DIR/*.asciidoc | sort); do
    count=$(wc -w < "$chapter")
    filename=$(basename "$chapter")
    echo "$filename: $count words"
     (( total += count ))
done
echo "Total: $total words"