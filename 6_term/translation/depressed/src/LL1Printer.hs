module LL1Printer where

import Input
import Grammar
import Common

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Either (isRight)
import Data.List (groupBy, sort, intercalate, find)
import Control.Monad (when)

constantPrefix :: String -> String
constantPrefix funcName = "\
    \import Data.Either (isLeft)\n\
    \import Control.Monad.State.Strict\n\
    \data EndOfInput = EndOfInput deriving (Show, Eq, Ord)\n\n" ++
    funcName ++ " ts = fst $ runState runParser tokens\n\
    \  where\n\
    \    tokens = map Right ts ++ [Left EndOfInput]\n"

printParser :: Input -> IO ()
printParser input = do
    putStrLn "{-# LANGUAGE FlexibleContexts #-}"
    putStrLn $ inputPrefix input
    putStrLn $ constantPrefix funcName

    mapM_ printCtorChecker tokens

    let (startNonTerminal :-> _) = head grammarRules
    putStrLn $ "runParser = " ++ nonTerminalFuncName startNonTerminal

    let nonTerminalRules = groupBy (\(a :-> _) (b :-> _) -> a == b) sortedRules
    mapM_ printNonTerminal nonTerminalRules

    putStrLn $ inputSuffix input
  where
    info         = parserInfo input

    funcName     = parserName info
    tokens       = parserTokens info
    tokenMap     = Map.fromList $ map (\(Token a b c) -> (a, (b, c))) tokens

    grammar      = parserGrammar info
    grammarRules = let Grammar g = grammar in g
    sortedRules  = sort grammarRules

    first        = makeFirst grammar
    follow       = makeFollow grammar first

    nonTerminalFuncName   (NonTerminal str) = "runParser_" ++ str
    nonTerminalSwitchName (NonTerminal str) = "runParser_" ++ str ++ "_switch"

    printCtorChecker :: Token -> IO ()
    printCtorChecker (Token _ ctor hasValue) = do
        putStrLn $ "is" ++ ctor ++ " (Right (" ++ ctor ++
            (if hasValue then " _" else "") ++ ")) = True"
        putStrLn $ "is" ++ ctor ++ " _ = False"

    printNonTerminal :: [Rule] -> IO ()
    printNonTerminal rules = do
        let (nonTerminal :-> _) = head rules
        putStr $ nonTerminalFuncName nonTerminal ++ " = do\n\
            \    (curToken : _) <- get\n\
            \    " ++ nonTerminalSwitchName nonTerminal ++ " curToken\n"
        mapM_ printRule rules
        let epsilonRule = find (\(_ :-> (ss, _)) -> null ss) rules
        case epsilonRule of
            Just rule -> printEpsilonRule rule
            Nothing   -> pure ()
        putStrLn ""

    printRule :: Rule -> IO ()
    printRule (nonTerminal :-> (symbols, formula)) = do
        let firstSymbols =
                firstSymbolsToFollow $
                filter (/= Left Epsilon) $
                Set.toList $
                runFirst symbols first
        let symbolToCtor (Right (Terminal str)) = "is" ++ (fst $ tokenMap Map.! str);
            symbolToCtor _                      = ""
        let firstCtors = map (\s -> symbolToCtor s ++ " curToken") firstSymbols
        let condition = intercalate " || " ("False" : firstCtors)
        putStr $ nonTerminalSwitchName nonTerminal ++ " curToken\n\
            \    | " ++ condition ++ " = do\n"
        mapM_ printConsume $ zip symbols [1..]
        putStr "        pure $ "
        printFomula formula
        putStrLn ""

    printEpsilonRule :: Rule -> IO ()
    printEpsilonRule (nonTerminal :-> (_, formula)) = do
        let followSymbols = Set.toList $ follow Map.! nonTerminal
        let symbolToCtor (Right (Terminal str)) = "is" ++ (fst $ tokenMap Map.! str);
            symbolToCtor _                      = "isLeft"
        let followCtors = map (\s -> symbolToCtor s ++ " curToken") followSymbols
        let condition = intercalate " || " ("False" : followCtors)
        putStr $ nonTerminalSwitchName nonTerminal ++ " curToken\n\
            \    | " ++ condition ++ " = do\n"
        putStr "        pure $ "
        printFomula formula
        putStrLn ";"

    printFomula :: AttributeFormula -> IO ()
    printFomula (AttributeFormula formula) = mapM_ print' formula
      where
        print' (Left (AttributeArgument n)) = putStr $ "ret" ++ show n
        print' (Right c) = putStr [c]

    printConsume :: (Symbol, Int) -> IO ()
    printConsume (Symbol (Right (Terminal str)), n) = do
        putStrLn $ "        ret" ++ show n ++ " <- do"
        let (ctor, hasValue) = tokenMap Map.! str
        putStr "            "
        putStrLn $ "(current          : rest) <- get"
        putStr "            "
        putStrLn $ "if " ++ "is" ++ fst (tokenMap Map.! str) ++ " current"
        putStrLn $ "                then do"
        when hasValue $ putStrLn $ "                    let Right (" ++ ctor ++ " v) = current"
        putStrLn $ "                    modify tail"
        when hasValue $ putStrLn $ "                    pure v"
        putStrLn $ "                else error $ \"expected " ++ tail (init (show str)) ++
                " but found \" ++ show current"
    printConsume (Symbol (Left nonTerminal), n) = do
        putStrLn $ "        ret" ++ show n ++ " <- " ++ nonTerminalFuncName nonTerminal

    firstSymbolsToFollow fs = map firstToFollow $ filter isRight fs
