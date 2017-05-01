module Main where

import Parser
import HtmlPrinter

main :: IO ()
main = interact (unlines . map ((++ "<br>\n") . formulaToString . parse) . lines)
