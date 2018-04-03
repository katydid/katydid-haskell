{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- |
-- This module describes the abstract tree that can be validated by Relapse.
--
-- The JSON and XML parsers both are both versions of this type class.

module Parsers (
    Tree(..), Label(..)
) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.ByteString (ByteString)

data Label
    = String Text
    | Int Int
    | Uint Word
    | Double Double
    | Bool Bool
    | Bytes ByteString
    deriving (Show, Eq, Ord, Generic, NFData)

class Tree a where
    getLabel :: a -> Label
    getChildren :: a -> [a]
