#!/bin/sh

cp -r ../../tex2html .
cat tex2html/src/Parser.y | depressed > tex2html/src/Parser.hs
rm tex2html/src/Parser.y
sed -i 's/containers/containers, mtl/' tex2html/tex2html.cabal
cd tex2html
stack clean
stack build --copy-bins
