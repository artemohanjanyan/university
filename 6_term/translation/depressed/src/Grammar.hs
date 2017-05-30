module Grammar where

newtype Grammar = Grammar [Rule]                      deriving (Show, Eq, Ord)

data Rule =
    NonTerminal :-> ([Symbol], AttributeFormula)
    deriving (Show, Eq, Ord)

newtype NonTerminal = NonTerminal String              deriving (Show, Eq, Ord)
newtype Terminal = Terminal String                    deriving (Show, Eq, Ord)
newtype Symbol = Symbol (Either NonTerminal Terminal) deriving (Show, Eq, Ord)

newtype AttributeFormula = AttributeFormula [Either AttributeArgument Char]
    deriving (Show, Eq, Ord)
newtype AttributeArgument = AttributeArgument Int     deriving (Show, Eq, Ord)
