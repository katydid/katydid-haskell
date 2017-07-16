module Parsers (
    Tree(..), Label(..)
) where

data Label
    = String String
    | Number Rational
    | Bool Bool
    deriving (Show, Eq, Ord)

class Tree a where
    getLabel :: a -> Label
    getChildren :: a -> [a]
