rm -f $1 
git log --date=short --pretty=format:"<logentry revision='%h'>%n <author email='%ae'>%an</author>%n <date>%ad</date>%n <msg xml:space='preserve'>%s</msg>%n</logentry>" | sed -e '1i<logs>' -e '$a</logs>' > $1 && rm -f $2 
xsltproc hg2revhistory.xsl $1 > $2 
echo git-log done
