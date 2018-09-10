{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- |
-- This module describes the abstract tree that can be validated by Relapse.
--
-- The JSON and XML parsers both are both versions of this type class.

module Data.Katydid.Parser.Parser (
    Tree(..), Label(..)
) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.ByteString (ByteString)

-- |
-- Label is a tagged union of all possible value types that can returned by a katydid parser: 
-- String, Int, Uint, Double, Bool and Bytes.
data Label
    = String Text
    | Int Int
    | Uint Word
    | Double Double
    | Bool Bool
    | Bytes ByteString
    deriving (Show, Eq, Ord, Generic, NFData)

-- |
-- Tree is the type class that should be implemented by a katydid parser.
-- This is implemented by the Json and XML parser.
class Tree a where
    getLabel :: a -> Label
    getChildren :: a -> [a]
