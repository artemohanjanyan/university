module Expression where

import Text.Parsec
import Text.Parsec.Char (char)
import Data.Char (isDigit, isSpace, isUpper, isLower)
import Control.Applicative ((*>), (<*))

data Expression
    = L Var Expression -- L for Lambda
    | Expression :$: Expression
    | V Var -- V for Var
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

show' (L var expr) flag1 flag2 = (if flag1 then id else embrace) $ "\\" ++ var ++ "." ++ show expr
show' (expr1 :$: expr2) flag1 flag2 = (if flag2 then id else embrace) $ show' expr1 False True ++ " " ++ show' expr2 flag1 False
show' (V var) _ _ = var

embrace str = "(" ++ str ++ ")"

parensShow :: Expression -> String
parensShow (L var expr) = embrace $ "\\" ++ show var ++ "." ++ parensShow expr
parensShow (expr1 :$: expr2) = embrace $ parensShow expr1 ++ " " ++ parensShow expr2
parensShow (V name) = name

instance Show Expression where
    show expr = show' expr True True

instance Show Type where
    show (BaseType name) = name
    show (t1@(_ :>: _) :>: t2) = embrace (show t1) ++ " -> " ++ show t2
    show (t1 :>: t2) = show t1 ++ " -> " ++ show t2

instance Show CurryExpression where
    show (e ::: t) = show e ++ " : " ++ show t

------------
-- Parser --
------------

expr :: String -> Expression
expr str = expr
  where
    Right expr = parse expressionParser "" str

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
    var <- token' $ varParser
    token' $ string "."
    expr <- token' $ expressionParser
    return $ L var expr

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
