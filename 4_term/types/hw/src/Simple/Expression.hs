{-# LANGUAGE FlexibleContexts #-}

module Simple.Expression where

import           Control.Applicative ((*>), (<*))
import           Data.Char           (isDigit, isLower, isSpace)
import           Text.Parsec
import           Text.Parsec.Char    (char)

data Expression
    = Lambda Var Expression -- L for Lambda
    | Expression :$: Expression
    | Var Var -- V for Var
    deriving (Eq, Ord)
infixl 9 :$:
type Var = String

data Type
    = BaseType TypeName
    | Type :>: Type
    deriving (Eq, Ord)
infixr 9 :>:
type TypeName = String

data CurryExpression = Expression ::: Type
infix 1 :::

------------
--- Show ---
------------

show' :: Expression -> Bool -> Bool -> [Char]
show' (Lambda var expr) flag1 _     = (if flag1 then id else embrace) $ "\\" ++ var ++ "." ++ show expr
show' (expr1 :$: expr2) flag1 flag2 = (if flag2 then id else embrace) $ show' expr1 False True ++ " " ++ show' expr2 flag1 False
show' (Var var)         _     _     = var

embrace :: String -> String
embrace str = "(" ++ str ++ ")"

fullShow :: String -> Expression -> String
fullShow dot (Lambda var expr) = embrace $ "\\" ++ var ++ dot ++ fullShow dot expr
fullShow dot (expr1 :$: expr2) = embrace $ fullShow dot expr1 ++ " " ++ fullShow dot expr2
fullShow _   (Var name)        = name

parensShow :: Expression -> String
parensShow = fullShow "."

haskellShow :: Expression -> String
haskellShow = fullShow "->"

instance Show Expression where
    show expr = show' expr True True

instance Show Type where
    show (BaseType name)       = name
    show (t1@(_ :>: _) :>: t2) = embrace (show t1) ++ " -> " ++ show t2
    show (t1 :>: t2)           = show t1 ++ " -> " ++ show t2

instance Show CurryExpression where
    show (e ::: t) = show e ++ " : " ++ show t

------------
-- Parser --
------------

expressionParser :: Parsec String () Expression
expressionParser =
    (   do
        _ <- many $ satisfy isSpace
        application <- optionMaybe applicationParser
        abstraction <- optionMaybe abstractionParser
        case (application, abstraction) of
            (Just appl, Just abstr) -> pure $ appl :$: abstr
            (Just appl, Nothing   ) -> pure $ appl
            (Nothing  , Just abstr) -> pure $ abstr
            (Nothing  , Nothing   ) -> fail ""
    )

abstractionParser :: Parsec String () Expression
abstractionParser = do
    _ <- token' $ char '\\'
    vars <- many1 $ token' $ varParser
    _ <- token' $ string "."
    expr <- token' $ expressionParser
    pure $ foldr ($) expr $ map Lambda vars

applicationParser :: Parsec String () Expression
applicationParser = chainl1 (token' atomParser) (pure (:$:))

atomParser :: Parsec String () Expression
atomParser = (token' (string "(") *> expressionParser <* token' (string ")")) <|> do
    var <- token' $ varParser
    pure $ Var var

varParser :: Parsec String () Var
varParser = do
    l <- satisfy isLower <?> "variable"
    ls <- (many $ satisfy isLower <|> satisfy isDigit <|> char '\'') <?> "variable suffix"
    pure $ (l:ls)

token' :: Parsec String () a -> Parsec String () a
token' parser = parser <* many (satisfy isSpace)

substitutionParser :: Parsec String () (Expression, Var, Expression)
substitutionParser = do
    expression <- expressionParser
    _ <- char '['
    variable <- varParser
    _ <- string ":="
    substitution <- expressionParser
    _ <- char ']'
    pure (expression, variable, substitution)
