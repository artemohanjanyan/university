{
module Parser
    ( parse
    ) where
import Types
import Data.Char
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
    '$'             { TokenDollar           }
    '\\'            { TokenBackslash        }
    '^'             { TokenCaret            }
    '_'             { TokenUnderscore       }
    '\{'            { TokenLeftBrace        }
    '\}'            { TokenRightBrace       }
    word            { TokenWord         $$  }
    number          { TokenNumber       $$  }
    char            { TokenChar         $$  }

%%

Formula1        : '$' Formula '$'               { $2 }

Formula         : {- -}                         { [] }
                | IndexedFormula Formula        { $1 : $2 }

IndexedFormula  : IndexedBody IndexedRest       { uncurry (IndexedFormula $1) $2 }

IndexedBody     : AtomFormula                   { IndexedAtom $1 }
                | Command                       { $1 }

IndexedRest     : SubIndex SubRest              { ($1, $2) }
                | SupIndex SupRest              { ($2, $1) }
                | {- -}                         { (Nothing, Nothing) }
SubIndex        : '_' IndexArg                  { Just $2 }
SupIndex        : '^' IndexArg                  { Just $2 }
SubRest         : SupIndex                      { $1 }
                | {- -}                         { Nothing }
SupRest         : SubIndex                      { $1 }
                | {- -}                         { Nothing }
IndexArg        : '\{' Formula '\}'             { $2 }
                | AtomFormula                   { [IndexedFormula (IndexedAtom $1) Nothing Nothing] }

Command         : '\\' word CommandArgs         { Command $2 $3 }
CommandArgs     : {- empty -}                   { [] }
                | '\{' Formula '\}' CommandArgs { $2 : $4 }

AtomFormula     : word                          { AtomString $1 }
                | number                        { AtomNumber $1 }
                | char                          { AtomChar $1 }


{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
    = TokenDollar
    | TokenBackslash
    | TokenCaret
    | TokenUnderscore
    | TokenLeftBrace
    | TokenRightBrace
    | TokenChar Char
    | TokenWord String
    | TokenNumber String
    deriving (Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('$' : cs) = TokenDollar     : tokenize cs
tokenize ('\\': cs) = TokenBackslash  : tokenize cs
tokenize ('^' : cs) = TokenCaret      : tokenize cs
tokenize ('_' : cs) = TokenUnderscore : tokenize cs
tokenize ('{' : cs) = TokenLeftBrace  : tokenize cs
tokenize ('}' : cs) = TokenRightBrace : tokenize cs
tokenize (c   : cs)
    | isSpace c = tokenize cs
    | isAlpha c = tokenizeSpan TokenWord   isAlpha
    | isDigit c = tokenizeSpan TokenNumber isDigit
    | otherwise = TokenChar c : tokenize cs
  where
    tokenizeSpan ctor pred =
        let (tokenTail, rest) = span pred cs
        in ctor (c : tokenTail) : tokenize rest

parse :: String -> Formula
parse = parseTokens . tokenize
}
