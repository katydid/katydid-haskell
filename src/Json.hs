{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

-- |
-- This module contains the Json Parser.

module Json (
    decodeJSON, JsonTree
) where

import qualified Data.Aeson as A
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as H

import qualified Data.Tree as DataTree
import Parsers

instance Tree JsonTree where
    getLabel (DataTree.Node l _) = l
    getChildren (DataTree.Node _ cs) = cs

-- |
-- JsonTree is a tree that can be validated by Relapse.
type JsonTree = DataTree.Tree Label

-- |
-- decodeJSON returns a JsonTree, given an input string.
decodeJSON :: String -> Either String [JsonTree]
decodeJSON s = case A.eitherDecode (fromString s) of
    (Left l) -> Left l
    (Right v) -> Right (uValue v)

uValue :: A.Value -> [JsonTree]
uValue A.Null = []
uValue (A.Bool b) = [DataTree.Node (Bool b) []]
uValue (A.Number r) = [DataTree.Node (Number (toRational r)) []]
uValue (A.String s) = [DataTree.Node (String (T.unpack s)) []]
uValue (A.Array vs) = uArray 0 (V.toList vs)
uValue (A.Object o) = uObject $ (H.toList o)

uArray :: Int -> [A.Value] -> [JsonTree]
uArray _ [] = []
uArray index (v:vs) = DataTree.Node (Number (toRational index)) (uValue v):uArray (index+1) vs

uObject :: [(T.Text, A.Value)] -> [JsonTree]
uObject = map uKeyValue

uKeyValue :: (T.Text, A.Value) -> JsonTree
uKeyValue (name, value) = DataTree.Node (String (T.unpack name)) (uValue value)
