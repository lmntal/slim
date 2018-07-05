#!bin/sh
for file in `ls input/`; do
    ../../src/slim --hl --nd --dump-json input/$file > js_input/js_input_$file.txt
done
