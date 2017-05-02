module HtmlPrinter where

import           Types
import qualified Data.Set as Set

formulaToString :: Formula -> String
formulaToString = unlines . taga "math" [("display", "block")] . formulaLines

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

indexedBodyLines :: IndexedBody -> [String]
indexedBodyLines (IndexedAtom atom) = atomFormulaLines atom
indexedBodyLines (Command command args)
    | command == "choose" = tag "mrow"
        (tag "mo" ["("] ++
            (taga "mfrac" [("linethickness", "0")] $ concat $ map iStr args) ++
        tag "mo" [")"])
    | Set.member command commandSet = tag ("m" ++ command) $ concat $ map iStr args
    | otherwise                     = tag (getTag command) ["&" ++ command ++ ";"]
  where
    commandSet = Set.fromList ["frac", "sqrt"]

    miTags = Set.fromList ["pi", "gamma", "lambda"]
    getTag str = if Set.member str miTags
        then "mi"
        else "mo"

atomFormulaLines :: AtomFormula -> [String]
atomFormulaLines (AtomString str) = concatMap (tag "mi" . pure . pure) str
atomFormulaLines (AtomNumber num) = tag "mn" [num]
atomFormulaLines (AtomChar   c  ) = tag "mo" [[c]]

tag :: String -> [String] -> [String]
tag = flip taga []

taga :: String -> [(String, String)] -> [String] -> [String]
taga tagName args str =
    ["<" ++ tagName ++
        concatMap (\(name, param) -> " " ++ name ++ "=\"" ++ param ++ "\"") args ++
    ">"] ++
    map (' ' :) str ++
    ["</" ++ tagName ++ ">"]

mrow :: [String] -> [String]
mrow = tag "mrow"

iStr :: Formula -> [String]
iStr = mrow . formulaLines
