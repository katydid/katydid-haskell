-- |
-- This module describes the abstract tree that can be validated by Relapse.
-- The JSON and XML parsers both are both versions of this type class.

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
