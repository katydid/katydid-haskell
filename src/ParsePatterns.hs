module ParsePatterns where

import Text.JSON (decode, Result(Error,Ok), JSValue(JSNull, JSBool, JSRational, JSString, JSArray, JSObject), fromJSString, fromJSObject)

import Patterns
import Values

decodeJSON :: String -> Refs
decodeJSON s = unmarshal $ decode s

unmarshal :: Result JSValue -> Refs
unmarshal (Error s) = error s
unmarshal (Ok (JSObject o)) = uRefs $ fromJSObject o
unmarshal (Ok j) = error $ "unexpected jsvalue = " ++ show j

uRefs :: [(String, JSValue)] -> Refs
uRefs (("TopPattern", (JSObject pattern)):pairs) = newRef "main" (uPattern (fromJSObject pattern)) `union` uRefs pairs
uRefs (("PatternDecls", (JSArray patternDecls)):pairs) = uPatternDecls patternDecls `union` uRefs pairs
uRefs (_:pairs) = uRefs pairs

uPatternDecls :: [JSValue] -> Refs
uPatternDecls ((JSObject o):patternDecls) = uPatternDecl (fromJSObject o) `union` uPatternDecls patternDecls

uPatternDecl :: [(String, JSValue)] -> Refs
uPatternDecl kvs = newRef (getString kvs "Name") (uPattern $ getObject kvs "Pattern")

uPattern :: [(String, JSValue)] -> Pattern
uPattern [("Empty", _)] = Empty
uPattern [("TreeNode", (JSObject v))] = uTreeNode (fromJSObject v)
uPattern [("LeafNode", (JSObject v))] = uLeafNode (fromJSObject v)
uPattern [("Concat", (JSObject v))] = uConcat (fromJSObject v)
uPattern [("Or", (JSObject v))] = uOr (fromJSObject v)
uPattern [("And", (JSObject v))] = uAnd (fromJSObject v)
uPattern [("ZeroOrMore", (JSObject v))] = uZeroOrMore (fromJSObject v)
uPattern [("Reference", (JSObject v))] = uReference (fromJSObject v)
uPattern [("Not", (JSObject v))] = uNot (fromJSObject v)
uPattern [("ZAny", (JSObject v))] = ZAny
uPattern [("Contains", (JSObject v))] = uContains (fromJSObject v)
uPattern [("Optional", (JSObject v))] = uOptional (fromJSObject v)
uPattern [("Interleave", (JSObject v))] = uInterleave (fromJSObject v)

uTreeNode :: [(String, JSValue)] -> Pattern
uTreeNode kvs = Node (uNameExpr $ getObject kvs "Name") (uPattern $ getObject kvs "Pattern")

uLeafNode :: [(String, JSValue)] -> Pattern
uLeafNode kvs = Node (uExpr $ getObject kvs "Expr") Empty

uReference :: [(String, JSValue)] -> Pattern
uReference kvs = Reference (getString kvs "Name")

uConcat :: [(String, JSValue)] -> Pattern
uConcat kvs = Concat (uPattern $ getObject kvs "LeftPattern") (uPattern $ getObject kvs "RightPattern")

uOr :: [(String, JSValue)] -> Pattern
uOr kvs = Concat (uPattern $ getObject kvs "LeftPattern") (uPattern $ getObject kvs "RightPattern")

uAnd :: [(String, JSValue)] -> Pattern
uAnd kvs = Concat (uPattern $ getObject kvs "LeftPattern") (uPattern $ getObject kvs "RightPattern")

uZeroOrMore :: [(String, JSValue)] -> Pattern
uZeroOrMore kvs = ZeroOrMore (uPattern $ getObject kvs "Pattern")

uNot :: [(String, JSValue)] -> Pattern
uNot kvs = Not (uPattern $ getObject kvs "Pattern")

uContains :: [(String, JSValue)] -> Pattern
uContains kvs = Not (uPattern $ getObject kvs "Pattern")

uOptional :: [(String, JSValue)] -> Pattern
uOptional kvs = Optional (uPattern $ getObject kvs "Pattern")

uInterleave :: [(String, JSValue)] -> Pattern
uInterleave kvs = Interleave (uPattern $ getObject kvs "LeftPattern") (uPattern $ getObject kvs "RightPattern") 

uNameExpr :: [(String, JSValue)] -> Value
uNameExpr = error "todo"

uExpr :: [(String, JSValue)] -> Value
uExpr = error "todo"
-- JSON helper functions

getField :: [(String, JSValue)] -> String -> JSValue
getField pairs name = let filtered = filter (\(k,_) -> (k == name)) pairs
	in case filtered of
	[] -> error $ "no field with name: " ++ name
	vs -> snd $ head $ vs

getString :: [(String, JSValue)] -> String -> String
getString pairs name = let v = getField pairs name in 
	case v of
	(JSString s) -> fromJSString s
	otherwise -> error $ name ++ " is not a string, but a " ++ show v

getObject :: [(String, JSValue)] -> String -> [(String, JSValue)]
getObject pairs name = let v = getField pairs name in 
	case v of
	(JSObject o) -> fromJSObject o
	otherwise -> error $ name ++ " is not an object, but a " ++ show v