module InputParser where

import           Grammar
import           Input

import           Control.Applicative
import           Control.Monad          (void)
import           Data.Char              (isSpace, isUpper)
import           Data.Maybe             (isJust)
import           Text.Megaparsec        hiding (Token, tokens)
import qualified Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String

sc :: Parser ()
sc = L.space (void spaceChar) lineComment blockComment
  where
    lineComment = L.skipLineComment "--"
    blockComment = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-------------
-- Grammar --
-------------

grammarParser :: Parser Grammar
grammarParser = Grammar . concat <$> some ruleParser

ruleParser :: Parser [Rule]
ruleParser = do
    lhs <- nonTerminalParser
    _ <- symbol ":"
    rhss <- sepBy1 productionParser (symbol "|")
    pure $ map (lhs :->) rhss
  where
    productionParser = (,) <$> many symbolParser <*> attributeFormulaParser

nonTerminalParser :: Parser NonTerminal
nonTerminalParser = lexeme (NonTerminal <$> strParser) <?> "non terminal"
  where
    strParser = (:) <$> upperChar <*> many (alphaNumChar <|> char '\'')

terminalParser :: Parser Terminal
terminalParser = lexeme (Terminal <$> strParser) <?> "terminal"
  where
    strParser = (:) <$> satisfy headPred <*> many (satisfy tailPred)
    headPred c = not (isSpace c) && not (isUpper c) && (c /= '{')
    tailPred c = not (isSpace c)

symbolParser :: Parser Symbol
symbolParser = Symbol <$> eitherP nonTerminalParser terminalParser <?> "symbol"

attributeFormulaParser :: Parser AttributeFormula
attributeFormulaParser = between (symbol "{") (symbol "}") expr <?> "attribute formula"
  where
    expr = AttributeFormula <$> (some $ eitherP arg (satisfy (/= '}')))
    arg = try $ char '$' *> ((AttributeArgument . fromInteger) <$> L.integer)

-----------
-- Input --
-----------

inputParser :: Parser Input
inputParser = between sc eof (Input <$> codeBlock <*> (sc *> parserInfoParser) <*> codeBlock)
  where
    codeBlock = leftCurly *> manyTill anyChar rightCurly
    leftCurly = void (char '{' *> eol)
    rightCurly = void $ try $ eol *> char '}' *> eol

parserInfoParser :: Parser ParserInfo
parserInfoParser = do
    _ <- symbol "%name"
    name <- lexeme $ (:) <$> lowerChar <*> many (alphaNumChar <|> char '\'')

    _ <- symbol "%tokentype"
    _ <- lexeme $ manyTill anyChar eol
    _ <- symbol "%error"
    _ <- lexeme $ manyTill anyChar eol

    _ <- symbol "%token"
    tokens <- someTill tokenParser (symbol "%%")

    grammar <- grammarParser

    pure $ ParserInfo name tokens grammar

tokenParser :: Parser Token
tokenParser = do
    Terminal str <- terminalParser
    _ <- symbol "{"
    NonTerminal ctor <- nonTerminalParser
    hasValue <- isJust <$> optional (symbol "$$")
    _ <- symbol "}"
    pure $ Token str ctor hasValue
