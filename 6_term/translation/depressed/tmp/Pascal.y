{
module Pascal where
import Data.Char
import Data.List
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
    'and' { TokenAnd           }
    'or'  { TokenOr            }
    'xor' { TokenXor           }
    'not' { TokenNot           }
    '('   { TokenLeftBracket   }
    ')'   { TokenRightBracket  }
    var   { TokenVar        $$ }

%%

Disj            : Conj Disj1        { foldl (\a b -> fst b a (snd b)) $1 $2 }
Disj1           : 'or'  Conj Disj1  { (Or,  $2) : $3 }
                | 'xor' Conj Disj1  { (Xor, $2) : $3 }
                | {- -}             { [] }

Conj            : Unary Conj1       { foldl And $1 $2 }
Conj1           : 'and' Unary Conj1 { $2 : $3 }
                | {- -}             { [] }

Unary           : var               { Var $1 }
                | 'not' Unary       { Not $2 }
                | '(' Disj ')'      { $2 }

{

data Expr
    = And Expr Expr
    | Or Expr Expr
    | Xor Expr Expr
    | Not Expr
    | Var String
    deriving (Show)

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
    = TokenAnd
    | TokenOr
    | TokenXor
    | TokenNot
    | TokenLeftBracket
    | TokenRightBracket
    | TokenVar String
    deriving (Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize cs
    | Just _ <- stripPrefix "and" cs = TokenAnd : tokenize (drop 3 cs)
    | Just _ <- stripPrefix "or"  cs = TokenOr  : tokenize (drop 2 cs)
    | Just _ <- stripPrefix "xor" cs = TokenXor : tokenize (drop 3 cs)
    | Just _ <- stripPrefix "not" cs = TokenNot : tokenize (drop 3 cs)
tokenize ('(' : cs) = TokenLeftBracket  : tokenize cs
tokenize (')' : cs) = TokenRightBracket : tokenize cs
tokenize (' ' : cs) = tokenize cs
tokenize (c   : cs) = tokenizeSpan TokenVar isAlpha
  where
    tokenizeSpan ctor pred =
        let (tokenTail, rest) = span pred cs
        in ctor (c : tokenTail) : tokenize rest

parse :: String -> Expr
parse = parseTokens . tokenize
}
