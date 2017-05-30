module Input where

import           Grammar

data Input = Input
    { inputPrefix :: String
    , parserInfo  :: ParserInfo
    , inputSuffix :: String
    }
    deriving (Show)

data ParserInfo = ParserInfo
    { parserName    :: String
    , parserTokens  :: [Token]
    , parserGrammar :: Grammar
    }
    deriving (Show)

data Token = Token
    { tokenString   :: String
    , tokenCtor     :: String
    , tokenHasValue :: Bool
    }
    deriving (Show)
