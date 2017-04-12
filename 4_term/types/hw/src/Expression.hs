{-# LANGUAGE FlexibleContexts #-}

module Expression where

import           Control.Applicative ((*>), (<*))
import           Data.Char           (isDigit, isLower, isSpace, isUpper)
import           Text.Parsec
import           Text.Parsec.Char    (char)

data Expression
    = L Var Expression -- L for Lambda
    | Expression :$: Expression
    | V Var -- V for Var
    | Let Var Expression Expression
    deriving (Eq, Ord)
infixl 9 :$:
type Var = String

data Type
    = BaseType TypeName
    | Type :>: Type
    | ForAll TypeName Type
    deriving (Eq, Ord)
infixr 9 :>:
type TypeName = String

data CurryExpression = Expression ::: Type
infix 1 :::

------------
--- Show ---
------------

show' (L var expr) flag1 flag2 = (if flag1 then id else embrace) $ "\\" ++ var ++ "." ++ show expr
show' (expr1 :$: expr2) flag1 flag2 = (if flag2 then id else embrace) $ show' expr1 False True ++ " " ++ show' expr2 flag1 False
show' (V var) _ _ = var
show' (Let var varExpr expr) flag1 _ = "let " ++ var ++ " = " ++ show' varExpr True True ++ " in " ++ show' expr flag1 True

embrace str = "(" ++ str ++ ")"

fullShow :: String -> Expression -> String
fullShow dot (L var expr) = embrace $ "\\" ++ var ++ dot ++ fullShow dot expr
fullShow dot (expr1 :$: expr2) = embrace $ fullShow dot expr1 ++ " " ++ fullShow dot expr2
fullShow dot (V name) = name
fullShow dot (Let var varExpr expr) = "let " ++ var ++ " = " ++ fullShow dot varExpr  ++ " in " ++ fullShow dot expr

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
    show (ForAll var t)        = "@" ++ var ++ "." ++ show t

instance Show CurryExpression where
    show (e ::: t) = show e ++ " : " ++ show t

------------
-- Parser --
------------

expr :: String -> Expression
expr str = expr
  where
    Right expr = parse expressionParser "" str

extendedParser :: Parsec String () Expression
extendedParser =
    (   do
        many $ satisfy isSpace
        abstraction <- optionMaybe abstractionParser
        case abstraction of
            Just abstr -> return abstr
            Nothing    -> do
                token' $ string "let"
                var <- varParser
                token' $ string "="
                varExpr <- extendedParser
                token' $ string "in"
                expr <- extendedParser
                return $ Let var varExpr expr
    )

expressionParser :: Parsec String () Expression
expressionParser =
    (   do
        many $ satisfy isSpace
        application <- optionMaybe applicationParser
        abstraction <- optionMaybe abstractionParser
        case (application, abstraction) of
            (Just appl, Just abstr) -> return $ appl :$: abstr
            (Just appl, Nothing   ) -> return $ appl
            (Nothing  , Just abstr) -> return $ abstr
            (Nothing  , Nothing   ) -> fail ""
    )

abstractionParser = do
    token' $ char '\\'
    vars <- many1 $ token' $ varParser
    token' $ string "."
    expr <- token' $ expressionParser
    return $ foldr ($) expr $ map L vars

applicationParser = chainl1 (token' atomParser) (return (:$:))

atomParser = (token' (string "(") *> expressionParser <* token' (string ")")) <|> do
    var <- token' $ varParser
    return $ V var

varParser = do
    l <- satisfy isLower <?> "variable"
    ls <- many $ satisfy isLower <|> satisfy isDigit <|> char '\''
    return $ (l:ls)

token' parser = parser <* many (satisfy isSpace)

substitutionParser = do
    expression <- expressionParser
    char '['
    variable <- varParser
    string ":="
    substitution <- expressionParser
    char ']'
    return (expression, variable, substitution)
