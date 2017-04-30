module Types
    ( Formula
    , IndexedFormula (..)
    , IndexedBody (..)
    , AtomFormula (..)
    ) where

type Formula = [IndexedFormula]

data IndexedFormula
    = IndexedFormula IndexedBody (Maybe Formula) (Maybe Formula)
    deriving (Show)

data IndexedBody
    = IndexedAtom AtomFormula
    | Command String [Formula]
    deriving (Show)

data AtomFormula
    = AtomString String
    | AtomNumber String
    | AtomChar Char
    deriving (Show)
