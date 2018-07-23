#!bin/sh
for file in `ls js_input/`; do
    ../../../difference-application-of-slim-isomorphism-checking/diffIsomorphism/a.out < js_input/$file > convertedgraph_check/$file
done

