{-# LANGUAGE FlexibleInstances #-}

module ParsedTree where

import Text.JSON
import qualified Data.Tree as DataTree
import Text.XML.HXT.DOM.TypeDefs
import Data.Tree.NTree.TypeDefs

data MyLabel
	= String String
	| Number Rational
	| Bool Bool
	deriving (Show, Eq, Ord)

class ParsedTree a where
	getMyLabel :: a -> MyLabel
	getMyChildren :: a -> [a]

instance ParsedTree JsonTree where
	getMyLabel (DataTree.Node l _) = l
	getMyChildren (DataTree.Node _ cs) = cs

type JsonTree = DataTree.Tree MyLabel

unmarshal :: String -> [JsonTree]
unmarshal s = unmarshal' $ (decode s :: Result JSValue)

unmarshal' :: Result JSValue -> [JsonTree]
unmarshal' (Error s) = error s
unmarshal' (Ok v) = uValue v

uValue :: JSValue -> [JsonTree]
uValue JSNull = []
uValue (JSBool b) = [DataTree.Node (Bool b) []]
uValue (JSRational _ r) = [DataTree.Node (Number r) []]
uValue (JSString s) = [DataTree.Node (String (fromJSString s)) []]
uValue (JSArray vs) = concatMap uValue vs
uValue (JSObject o) = uObject $ fromJSObject o

uObject :: [(String, JSValue)] -> [JsonTree]
uObject keyValues = map uKeyValue keyValues

uKeyValue :: (String, JSValue) -> JsonTree
uKeyValue (name, value) = DataTree.Node (String name) (uValue value)

instance ParsedTree XmlTree where
	getMyLabel (NTree n _ ) = xmlLabel n
	getMyChildren (NTree n cs) = cs

xmlLabel :: XNode -> MyLabel
xmlLabel _ = error "xmlLabel: todo"



