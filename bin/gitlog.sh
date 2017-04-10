#/bin/sh

git log | grep Author | sed -- 's/Author: /\* /g' | sed -- 's/<.*>//g' | sort -u | sed -- 's/\* Your Name//g' > $1 
