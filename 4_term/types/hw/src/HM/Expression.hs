{-# LANGUAGE FlexibleContexts #-}

module HM.Expression where

import           Control.Applicative ((*>), (<*))
import           Data.Char           (isSpace)
import           Text.Parsec
import           Text.Parsec.Char    (char)

import           Simple.Expression   (Type (..), TypeName, Var, embrace, token', varParser)

data HMExpression
    = Lambda Var HMExpression
    | HMExpression :$: HMExpression
    | Var Var
    | Let Var HMExpression HMExpression
    deriving (Show, Eq, Ord)
infixl 9 :$:

data Rank1Type
    = Monotype Type
    | ForAll TypeName Rank1Type
    deriving (Show, Eq, Ord)

------------
--- Show ---
------------

cleanShow :: HMExpression -> String
cleanShow = cleanShow' True True
  where
    cleanShow' :: Bool -> Bool -> HMExpression -> String
    cleanShow' flag1 _     (Lambda var expr)      = (if flag1 then id else embrace) $
                                                     "\\" ++ var ++ "." ++ show expr
    cleanShow' flag1 flag2 (expr1 :$: expr2)      = (if flag2 then id else embrace) $
                                                     cleanShow' False True expr1 ++ " " ++
                                                     cleanShow' flag1 False expr2
    cleanShow' _     _     (Var var)              = var
    cleanShow' flag1 _     (Let var varExpr expr) = "let " ++ var ++ " = " ++ cleanShow' True True varExpr ++
                                                     " in " ++ cleanShow' flag1 True expr

------------
-- Parser --
------------

hmParser :: Parsec String () HMExpression
hmParser = do
    _ <- many $ satisfy isSpace
    mLetE <- optionMaybe letParser
    case mLetE of
        Just letE -> pure letE
        Nothing   -> expressionParser

letParser :: Parsec String () HMExpression
letParser = do
    _       <- token' $ string "let"
    var     <- token' hmVarParser
    _       <- token' $ string "="
    varExpr <- token' hmParser
    _       <- token' $ string "in"
    expr    <- token' hmParser
    pure $ Let var varExpr expr

expressionParser :: Parsec String () HMExpression
expressionParser = do
    application <- optionMaybe applicationParser
    abstraction <- optionMaybe abstractionParser
    case (application, abstraction) of
        (Just appl, Just abstr) -> pure $ appl :$: abstr
        (Just appl, Nothing   ) -> pure $ appl
        (Nothing  , Just abstr) -> pure $ abstr
        (Nothing  , Nothing   ) -> fail ""

abstractionParser :: Parsec String () HMExpression
abstractionParser = do
    _ <- token' $ char '\\'
    vars <- many1 $ token' hmVarParser
    _ <- token' $ string "."
    expr <- token' expressionParser
    pure $ foldr ($) expr $ map Lambda vars

applicationParser :: Parsec String () HMExpression
applicationParser = chainl1 (token' atomParser) (pure (:$:))

atomParser :: Parsec String () HMExpression
atomParser = (token' (string "(") *> hmParser <* token' (string ")")) <|>
                Var <$> token' hmVarParser

hmVarParser :: Parsec String () Var
hmVarParser = do
    _ <- notFollowedBy $ string "in"
    varParser
