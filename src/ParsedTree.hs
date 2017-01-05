module ParsedTree where

import Text.JSON
import Data.Tree

data Label
	= String String
	| Number Rational
	| Bool Bool
	deriving Show

type ParsedTree = Tree Label

unmarshal :: String -> [ParsedTree]
unmarshal s = unmarshal' $ (decode s :: Result JSValue)

unmarshal' :: Result JSValue -> [ParsedTree]
unmarshal' (Error s) = error s
unmarshal' (Ok v) = uValue v

uValue :: JSValue -> [ParsedTree]
uValue JSNull = []
uValue (JSBool b) = [Node (Bool b) []]
uValue (JSRational _ r) = [Node (Number r) []]
uValue (JSString s) = [Node (String (fromJSString s)) []]
uValue (JSArray vs) = concatMap uValue vs
uValue (JSObject o) = uObject $ fromJSObject o

uObject :: [(String, JSValue)] -> [ParsedTree]
uObject keyValues = map uKeyValue keyValues

uKeyValue :: (String, JSValue) -> ParsedTree
uKeyValue (name, value) = Node (String name) (uValue value)
