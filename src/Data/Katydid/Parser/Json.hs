{-# LANGUAGE FlexibleInstances #-}

-- |
-- This module contains the Json Parser.

module Data.Katydid.Parser.Json
  ( decodeJSON
  , JsonTree
  )
where

import           Text.JSON                      ( decode
                                                , Result(..)
                                                , JSValue(..)
                                                , fromJSString
                                                , fromJSObject
                                                )
import           Data.Ratio                     ( denominator )
import           Data.Text                      ( pack )

import qualified Data.Tree                     as DataTree
import           Data.Katydid.Parser.Parser

instance Tree JsonTree where
    getLabel (DataTree.Node l _) = l
    getChildren (DataTree.Node _ cs) = cs

-- |
-- JsonTree is a tree that can be validated by Relapse.
type JsonTree = DataTree.Tree Label

-- |
-- decodeJSON returns a JsonTree, given an input string.
decodeJSON :: String -> Either String [JsonTree]
decodeJSON s = case decode s of
  (Error e) -> Left e
  (Ok    v) -> Right (uValue v)

uValue :: JSValue -> [JsonTree]
uValue JSNull           = []
uValue (JSBool b      ) = [DataTree.Node (Bool b) []]
uValue (JSRational _ r) = if denominator r /= 1
  then [DataTree.Node (Double (fromRational r :: Double)) []]
  else [DataTree.Node (Int $ truncate r) []]
uValue (JSString s ) = [DataTree.Node (String $ pack $ fromJSString s) []]
uValue (JSArray  vs) = uArray 0 vs
uValue (JSObject o ) = uObject $ fromJSObject o

uArray :: Int -> [JSValue] -> [JsonTree]
uArray _ [] = []
uArray index (v : vs) =
  DataTree.Node (Int index) (uValue v) : uArray (index + 1) vs

uObject :: [(String, JSValue)] -> [JsonTree]
uObject = map uKeyValue

uKeyValue :: (String, JSValue) -> JsonTree
uKeyValue (name, value) = DataTree.Node (String $ pack name) (uValue value)
