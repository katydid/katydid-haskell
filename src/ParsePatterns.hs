module ParsePatterns where

import Text.JSON (decode, Result(Error,Ok), JSValue(JSNull, JSBool, JSRational, JSString, JSArray, JSObject), fromJSString, fromJSObject)

import Parsers
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
uPattern [("TreeNode", JSObject o)] = uTreeNode (fromJSObject o)
uPattern [("LeafNode", JSObject o)] = uLeafNode (fromJSObject o)
uPattern [("Concat", JSObject o)] = uConcat (fromJSObject o)
uPattern [("Or", JSObject o)] = uOr (fromJSObject o)
uPattern [("And", JSObject o)] = uAnd (fromJSObject o)
uPattern [("ZeroOrMore", JSObject o)] = uZeroOrMore (fromJSObject o)
uPattern [("Reference", JSObject o)] = uReference (fromJSObject o)
uPattern [("Not", JSObject o)] = uNot (fromJSObject o)
uPattern [("ZAny", JSObject o)] = ZAny
uPattern [("Contains", JSObject o)] = uContains (fromJSObject o)
uPattern [("Optional", JSObject o)] = uOptional (fromJSObject o)
uPattern [("Interleave", JSObject o)] = uInterleave (fromJSObject o)

uTreeNode :: [(String, JSValue)] -> Pattern
uTreeNode kvs = Node (uNameExpr $ getObject kvs "Name") (uPattern $ getObject kvs "Pattern")

uLeafNode :: [(String, JSValue)] -> Pattern
uLeafNode kvs = Node (uBoolExpr $ getObject kvs "Expr") Empty

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

uNameExpr :: [(String, JSValue)] -> BoolExpr
uNameExpr [("Name", JSObject o)] = uName (fromJSObject o)
uNameExpr [("AnyName", JSObject o)] = (BoolConst True)
uNameExpr [("AnyNameExcept", JSObject o)] = uNameExcept (fromJSObject o)
uNameExpr [("NameChoice", JSObject o)] = uNameChoice (fromJSObject o)

uName :: [(String, JSValue)] -> BoolExpr
uName kvs = uName' $ head $ filter (\(k,v) -> (k /= "Before")) kvs

uName' :: (String, JSValue) -> BoolExpr
uName' ("DoubleValue", (JSRational _ num)) = DoubleEqualFunc (DoubleConst num) DoubleVariable
uName' ("IntValue", (JSRational _ num)) = IntEqualFunc (IntConst $ truncate num) IntVariable
uName' ("UintValue", (JSRational _ num)) = UintEqualFunc (UintConst $ truncate num) UintVariable
uName' ("BoolValue", (JSBool b)) = BoolEqualFunc (BoolConst b) BoolVariable
uName' ("StringValue", (JSString s)) = StringEqualFunc (StringConst $ fromJSString s) StringVariable
uName' ("BytesValue", (JSString s)) = BytesEqualFunc (BytesConst $ fromJSString s) BytesVariable

uNameExcept :: [(String, JSValue)] -> BoolExpr
uNameExcept kvs = NotFunc (uNameExpr $ getObject kvs "Except")

uNameChoice :: [(String, JSValue)] -> BoolExpr
uNameChoice kvs = OrFunc (uNameExpr $ getObject kvs "Left") (uNameExpr $ getObject kvs "Right")

uBoolExpr :: [(String, JSValue)] -> BoolExpr
uBoolExpr kvs = let e = uExprs kvs in case e of
	(BoolExpr b) -> b
	otherwise -> error $ "not a bool expr, but a " ++ show e

uExprs :: [(String, JSValue)] -> Expr
uExprs kvs = uExpr $ head $ filter (\(k,v) -> k /= "RightArrow" && k /= "Comma") kvs 

uExpr :: (String, JSValue) -> Expr
uExpr ("Terminal", (JSObject o)) = uTerminal $ fromJSObject o
uExpr ("List", (JSObject o)) = uList $ fromJSObject o
uExpr ("Function", (JSObject o)) = uFunction $ fromJSObject o
uExpr ("BuiltIn", (JSObject o)) = uBuiltIn $ fromJSObject o

uTerminal :: [(String, JSValue)] -> Expr
uTerminal kvs = uTerminal' $ head $ filter (\(k,v) -> k /= "Before" && k /= "Literal") kvs

data Expr 
	= BoolExpr BoolExpr
	| DoubleExpr DoubleExpr
	| IntExpr IntExpr
	| UintExpr UintExpr 
	| StringExpr StringExpr
	| BytesExpr BytesExpr
	deriving Show

uTerminal' :: (String, JSValue) -> Expr
uTerminal' ("DoubleValue", JSRational _ n) = error "todo"

uList :: [(String, JSValue)] -> Expr
uList = error "todo"

uFunction :: [(String, JSValue)] -> Expr
uFunction = error "todo"

uBuiltIn :: [(String, JSValue)] -> Expr
uBuiltIn = error "todo"

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