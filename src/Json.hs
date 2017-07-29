{-# LANGUAGE FlexibleInstances #-}

module Json (
    decodeJSON, JsonTree
) where

import Text.JSON (decode, Result(..), JSValue(..), fromJSString, fromJSObject)

import qualified Data.Tree as DataTree
import Parsers

instance Tree JsonTree where
    getLabel (DataTree.Node l _) = l
    getChildren (DataTree.Node _ cs) = cs

type JsonTree = DataTree.Tree Label

decodeJSON :: String -> Either String [JsonTree]
decodeJSON s = case decode s of
    (Error e) -> Left e
    (Ok v) -> Right (uValue v)

uValue :: JSValue -> [JsonTree]
uValue JSNull = []
uValue (JSBool b) = [DataTree.Node (Bool b) []]
uValue (JSRational _ r) = [DataTree.Node (Number r) []]
uValue (JSString s) = [DataTree.Node (String (fromJSString s)) []]
uValue (JSArray vs) = uArray 0 vs
uValue (JSObject o) = uObject $ fromJSObject o

uArray :: Int -> [JSValue] -> [JsonTree]
uArray index [] = []
uArray index (v:vs) = DataTree.Node (Number (toRational index)) (uValue v):uArray (index+1) vs

uObject :: [(String, JSValue)] -> [JsonTree]
uObject = map uKeyValue

uKeyValue :: (String, JSValue) -> JsonTree
uKeyValue (name, value) = DataTree.Node (String name) (uValue value)
