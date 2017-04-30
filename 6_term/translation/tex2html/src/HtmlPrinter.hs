module HtmlPrinter where

import Types

formulaToString :: Formula -> String
formulaToString = unlines . tag "math" . formulaLines

formulaLines :: Formula -> [String]
formulaLines = concatMap indexedFormulaLines

indexedFormulaLines :: IndexedFormula -> [String]
indexedFormulaLines (IndexedFormula body msub msup) =
    case (msub, msup) of
        (Just sub, Just sup) -> tag "msubsup" $ mrow bodyStr ++ iStr sub ++ iStr sup
        (Just sub, Nothing)  -> tag "msub"    $ mrow bodyStr ++ iStr sub
        (Nothing, Just sup)  -> tag "msup"    $ mrow bodyStr ++             iStr sup
        (Nothing, Nothing)   -> bodyStr
  where
    bodyStr = indexedBodyLines body
    mrow = tag "mrow"
    iStr = mrow . formulaLines

indexedBodyLines :: IndexedBody -> [String]
indexedBodyLines (IndexedAtom atom) = atomFormulaLines atom
indexedBodyLines (Command command args) = undefined

atomFormulaLines :: AtomFormula -> [String]
atomFormulaLines (AtomString str) = tag "mi" [str]
atomFormulaLines (AtomNumber num) = tag "mn" [num]
atomFormulaLines (AtomChar   c  ) = tag "mo" [[c]]

tag :: String -> [String] -> [String]
tag tagName str = ["<" ++ tagName ++ ">"] ++ map (' ' :) str ++ ["</" ++ tagName ++ ">"]
