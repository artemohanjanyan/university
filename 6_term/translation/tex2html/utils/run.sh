#!/bin/bash

cat "utils/start.html" > "utils/sample.html"
cat "utils/sample.tex" | tex2html >> "utils/sample.html"
cat "utils/end.html" >> "utils/sample.html"
