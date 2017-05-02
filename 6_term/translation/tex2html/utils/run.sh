#!/bin/bash

cat "utils/start.html" > $2
cat $1 | tex2html >> $2
cat "utils/end.html" >> $2
