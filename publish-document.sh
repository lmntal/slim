#!/bin/bash

if [ $(git status --short | wc -l) -ne 0 ] ; then
	echo "There are modified files. Please commit or delete these files before."
	exit 1
fi

git symbolic-ref -q HEAD 1>/dev/null 2>/dev/null
if [ $? -ne 0 ] ; then
	git symbolic-ref HEAD
	exit 1
fi

CUR_BRANCH=$(git symbolic-ref --short HEAD)
git branch -D gh-pages 1>/dev/null 2>/dev/null

echo -n "Automaking configure files, "
./autogen.sh 1>/dev/null 2>/dev/null
echo "done."
echo -n "Configuring, "
./configure 1>/dev/null 2>/dev/null
echo "done."
echo -n "Generating documents, "
make clean 1>/dev/null 2>/dev/null
make doxygen-doc 1>/dev/null 2>/dev/null
echo "done."

echo -n "Moving html files, "
TMP=$(mktemp -d)
mv doc/doxygen/html $TMP
ls | grep -v $TMP | xargs rm -rf
mv $TMP/html/* .
rm -rf $TMP
echo "done."

git checkout -B gh-pages 1>/dev/null 2>/dev/null
git add .
git commit -q -m "Generate documents with doxygen."

git push --force origin gh-pages
git checkout $CUR_BRANCH
