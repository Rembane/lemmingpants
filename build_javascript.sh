#!/bin/bash

declare -a paths=("static/js/admin/main.js" "static/js/terminal/main.js")

finalsuffix=bundle.js

for f in "${paths[@]}"; do
    node_modules/rollup/bin/rollup -c -w --sourcemap --input $f --output "${f/main.js/$finalsuffix}" &
done

wait
