#!/bin/bash

#
# This little script is originally from
# https://guide.elm-lang.org/optimization/asset_size.html
#

set -e

JS=elm.js
MIN=elm.min.js

elm make src/Main.elm --optimize --output=$JS

uglifyjs $JS --compress \
  'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
  | uglifyjs --mangle --output=$MIN

echo "Compiled size:$(cat $JS  | wc -c) bytes"
echo "Minified size:$(cat $MIN | wc -c) bytes"
echo "Gzipped  size:$(cat $MIN | gzip -c | wc -c) bytes"

rm $JS
