#!bin/sh
if [ -d ./out ]; then
    rm -rf out
fi
mkdir out
mkdir out/diff
for file in `ls input/`; do
    echo -n -e '\e[37;1m'
    echo -n $file
    ../../src/slim --hl --nd input/$file > o
    head -n -2 o > ./out/out_$file.txt
    diff ./out/out_$file.txt diffinfo_check/$file.txt > ./out/diff/diff_$file
    if [ -s ./out/diff/diff_$file ]; then
	echo -e '\e[31;1m:FAIL'
    else
	echo -e '\e[32;1m:PASS'
    fi
    rm o
done
