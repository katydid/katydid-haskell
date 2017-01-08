module ParsePatterns where

import Text.JSON (decode, Result(Error,Ok), JSValue(JSNull, JSBool, JSRational, JSString, JSArray, JSObject), fromJSString, fromJSObject)

import Parsers
import Patterns
import Values

fromJson :: String -> Refs
fromJson s = unmarshal $ decode s

unmarshal :: Result JSValue -> Refs
unmarshal (Error s) = error s
unmarshal (Ok (JSObject o)) = uRefs $ fromJSObject o
unmarshal (Ok j) = error $ "unexpected jsvalue = " ++ show j

uRefs :: [(String, JSValue)] -> Refs
uRefs [] = emptyRef
uRefs (("TopPattern", (JSObject pattern)):pairs) = newRef "main" (uPattern (fromJSObject pattern)) `union` uRefs pairs
uRefs (("PatternDecls", (JSArray patternDecls)):pairs) = uPatternDecls patternDecls `union` uRefs pairs
uRefs (_:pairs) = uRefs pairs


uPatternDecls :: [JSValue] -> Refs
uPatternDecls [] = emptyRef
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

data Expr 
	= BoolExpr BoolExpr
	| DoubleExpr DoubleExpr
	| IntExpr IntExpr
	| UintExpr UintExpr 
	| StringExpr StringExpr
	| BytesExpr BytesExpr
	| BoolListExpr [BoolExpr]
	| DoubleListExpr [DoubleExpr]
	| IntListExpr [IntExpr]
	| UintListExpr [UintExpr]
	| StringListExpr [StringExpr]
	| BytesListExpr [BytesExpr]
	deriving Show

uBoolExpr :: [(String, JSValue)] -> BoolExpr
uBoolExpr kvs = let e = uExprs kvs in case e of
	(BoolExpr v) -> v
	otherwise -> error $ "not a BoolExpr, but a " ++ show e

uDoubleExpr :: [(String, JSValue)] -> DoubleExpr
uDoubleExpr kvs = let e = uExprs kvs in case e of
	(DoubleExpr v) -> v
	otherwise -> error $ "not a DoubleExpr, but a " ++ show e

uIntExpr :: [(String, JSValue)] -> IntExpr
uIntExpr kvs = let e = uExprs kvs in case e of
	(IntExpr v) -> v
	otherwise -> error $ "not a IntExpr, but a " ++ show e

uUintExpr :: [(String, JSValue)] -> UintExpr
uUintExpr kvs = let e = uExprs kvs in case e of
	(UintExpr v) -> v
	otherwise -> error $ "not a UintExpr, but a " ++ show e

uStringExpr :: [(String, JSValue)] -> StringExpr
uStringExpr kvs = let e = uExprs kvs in case e of
	(StringExpr v) -> v
	otherwise -> error $ "not a StringExpr, but a " ++ show e

uBytesExpr :: [(String, JSValue)] -> BytesExpr
uBytesExpr kvs = let e = uExprs kvs in case e of
	(BytesExpr v) -> v
	otherwise -> error $ "not a BytesExpr, but a " ++ show e

uExprs :: [(String, JSValue)] -> Expr
uExprs kvs = uExpr $ head $ filter (\(k,v) -> k /= "RightArrow" && k /= "Comma") kvs 

uExpr :: (String, JSValue) -> Expr
uExpr ("Terminal", (JSObject o)) = uTerminals $ fromJSObject o
uExpr ("List", (JSObject o)) = uList $ fromJSObject o
uExpr ("Function", (JSObject o)) = uFunction $ fromJSObject o
uExpr ("BuiltIn", (JSObject o)) = uBuiltIn $ fromJSObject o

uTerminals :: [(String, JSValue)] -> Expr
uTerminals kvs = uTerminal $ head $ filter (\(k,v) -> k /= "Before" && k /= "Literal") kvs

uTerminal :: (String, JSValue) -> Expr
uTerminal ("DoubleValue", JSRational _ n) = DoubleExpr (DoubleConst n)
uTerminal ("IntValue", JSRational _ n) = IntExpr (IntConst $ truncate n)
uTerminal ("UintValue", JSRational _ n) = UintExpr (UintConst $ truncate n)
uTerminal ("BoolValue", JSBool b) = BoolExpr (BoolConst b)
uTerminal ("StringValue", JSString s) = StringExpr (StringConst $ fromJSString s)
uTerminal ("BytesValue", JSString s) = BytesExpr (BytesConst $ fromJSString s)
uTerminal ("Variable", JSObject o) = uVariable $ (fromJSObject o)

uVariable :: [(String, JSValue)] -> Expr
uVariable [("Type", JSRational _ 101)] = DoubleExpr DoubleVariable
uVariable [("Type", JSRational _ 103)] = IntExpr IntVariable
uVariable [("Type", JSRational _ 104)] = UintExpr UintVariable
uVariable [("Type", JSRational _ 108)] = BoolExpr BoolVariable
uVariable [("Type", JSRational _ 109)] = StringExpr StringVariable
uVariable [("Type", JSRational _ 112)] = BytesExpr BytesVariable

uList :: [(String, JSValue)] -> Expr
uList kvs = case getInt kvs "Type" of
	101 -> DoubleListExpr $ map uDoubleExpr (getArrayOfObjects kvs "Elems")
	103 -> IntListExpr $ map uIntExpr (getArrayOfObjects kvs "Elems")
	104 -> UintListExpr $ map uUintExpr (getArrayOfObjects kvs "Elems")
	108 -> BoolListExpr $ map uBoolExpr (getArrayOfObjects kvs "Elems")
	109 -> StringListExpr $ map uStringExpr (getArrayOfObjects kvs "Elems")
	112 -> BytesListExpr $ map uBytesExpr (getArrayOfObjects kvs "Elems")

	201 -> DoubleListExpr $ map uDoubleExpr (getArrayOfObjects kvs "Elems")
	203 -> IntListExpr $ map uIntExpr (getArrayOfObjects kvs "Elems")
	204 -> UintListExpr $ map uUintExpr (getArrayOfObjects kvs "Elems")
	208 -> BoolListExpr $ map uBoolExpr (getArrayOfObjects kvs "Elems")
	209 -> StringListExpr $ map uStringExpr (getArrayOfObjects kvs "Elems")
	212 -> BytesListExpr $ map uBytesExpr (getArrayOfObjects kvs "Elems")

uFunction :: [(String, JSValue)] -> Expr
uFunction kvs = newFunction (getString kvs "Name") (map uExprs (getArrayOfObjects kvs "Params"))

newFunction :: String -> [Expr] -> Expr
newFunction "not" [BoolExpr b] = BoolExpr $ NotFunc b
newFunction "and" [BoolExpr b1, BoolExpr b2] = BoolExpr $ AndFunc b1 b2
newFunction "or" [BoolExpr b1, BoolExpr b2] = BoolExpr $ OrFunc b1 b2

newFunction "contains" [IntExpr i,IntListExpr is] = BoolExpr $ IntListContainsFunc i is
newFunction "contains" [StringExpr s, StringListExpr ss] = BoolExpr $ StringListContainsFunc s ss
newFunction "contains" [UintExpr u, UintListExpr us] = BoolExpr $ UintListContainsFunc u us
newFunction "contains" [StringExpr s, StringExpr ss] = BoolExpr $ StringContainsFunc s ss

newFunction "elem" [BytesListExpr es, IntExpr i] = BytesExpr $ BytesListElemFunc es i
newFunction "elem" [BoolListExpr es, IntExpr i] = BoolExpr $ BoolListElemFunc es i
newFunction "elem" [DoubleListExpr es, IntExpr i] = DoubleExpr $ DoubleListElemFunc es i
newFunction "elem" [IntListExpr es, IntExpr i] = IntExpr $ IntListElemFunc es i
newFunction "elem" [StringListExpr es, IntExpr i] = StringExpr $ StringListElemFunc es i
newFunction "elem" [UintListExpr es, IntExpr i] = UintExpr $ UintListElemFunc es i

newFunction "eq" [BytesExpr v1, BytesExpr v2] = BoolExpr $ BytesEqualFunc v1 v2
newFunction "eq" [BoolExpr v1, BoolExpr v2] = BoolExpr $ BoolEqualFunc v1 v2
newFunction "eq" [DoubleExpr v1, DoubleExpr v2] = BoolExpr $ DoubleEqualFunc v1 v2
newFunction "eq" [IntExpr v1, IntExpr v2] = BoolExpr $ IntEqualFunc v1 v2
newFunction "eq" [StringExpr v1, StringExpr v2] = BoolExpr $ StringEqualFunc v1 v2
newFunction "eq" [UintExpr v1, UintExpr v2] = BoolExpr $ UintEqualFunc v1 v2

newFunction "eqFold" _ = error "eqFold function is not supported"

newFunction "ge" [BytesExpr v1, BytesExpr v2] = BoolExpr $ BytesGreaterOrEqualFunc v1 v2
newFunction "ge" [DoubleExpr v1, DoubleExpr v2] = BoolExpr $ DoubleGreaterOrEqualFunc v1 v2
newFunction "ge" [IntExpr v1, IntExpr v2] = BoolExpr $ IntGreaterOrEqualFunc v1 v2
newFunction "ge" [UintExpr v1, UintExpr v2] = BoolExpr $ UintGreaterOrEqualFunc v1 v2

newFunction "gt" [BytesExpr v1, BytesExpr v2] = BoolExpr $ BytesGreaterThanFunc v1 v2
newFunction "gt" [DoubleExpr v1, DoubleExpr v2] = BoolExpr $ DoubleGreaterThanFunc v1 v2
newFunction "gt" [IntExpr v1, IntExpr v2] = BoolExpr $ IntGreaterThanFunc v1 v2
newFunction "gt" [UintExpr v1, UintExpr v2] = BoolExpr $ UintGreaterThanFunc v1 v2

newFunction "hasPrefix" [StringExpr v1, StringExpr v2] = BoolExpr $ StringHasPrefixFunc v1 v2
newFunction "hasSuffix" [StringExpr v1, StringExpr v2] = BoolExpr $ StringHasSuffixFunc v1 v2

newFunction "le" [BytesExpr v1, BytesExpr v2] = BoolExpr $ BytesLessOrEqualFunc v1 v2
newFunction "le" [DoubleExpr v1, DoubleExpr v2] = BoolExpr $ DoubleLessOrEqualFunc v1 v2
newFunction "le" [IntExpr v1, IntExpr v2] = BoolExpr $ IntLessOrEqualFunc v1 v2
newFunction "le" [UintExpr v1, UintExpr v2] = BoolExpr $ UintLessOrEqualFunc v1 v2

newFunction "length" [BytesListExpr vs] = IntExpr $ BytesListLengthFunc vs
newFunction "length" [BoolListExpr vs] = IntExpr $ BoolListLengthFunc vs
newFunction "length" [BytesExpr vs] = IntExpr $ BytesLengthFunc vs
newFunction "length" [DoubleListExpr vs] = IntExpr $ DoubleListLengthFunc vs
newFunction "length" [IntListExpr vs] = IntExpr $ IntListLengthFunc vs
newFunction "length" [StringListExpr vs] = IntExpr $ StringListLengthFunc vs
newFunction "length" [UintListExpr vs] = IntExpr $ UintListLengthFunc vs
newFunction "length" [StringExpr vs] = IntExpr $ StringLengthFunc vs

newFunction "lt" [BytesExpr v1, BytesExpr v2] = BoolExpr $ BytesLessThanFunc v1 v2
newFunction "lt" [DoubleExpr v1, DoubleExpr v2] = BoolExpr $ DoubleLessThanFunc v1 v2
newFunction "lt" [IntExpr v1, IntExpr v2] = BoolExpr $ IntLessThanFunc v1 v2
newFunction "lt" [UintExpr v1, UintExpr v2] = BoolExpr $ UintLessThanFunc v1 v2

newFunction "ne" [BytesExpr v1, BytesExpr v2] = BoolExpr $ BytesNotEqualFunc v1 v2
newFunction "ne" [BoolExpr v1, BoolExpr v2] = BoolExpr $ BoolNotEqualFunc v1 v2
newFunction "ne" [DoubleExpr v1, DoubleExpr v2] = BoolExpr $ DoubleNotEqualFunc v1 v2
newFunction "ne" [IntExpr v1, IntExpr v2] = BoolExpr $ IntNotEqualFunc v1 v2
newFunction "ne" [StringExpr v1, StringExpr v2] = BoolExpr $ StringNotEqualFunc v1 v2
newFunction "ne" [UintExpr v1, UintExpr v2] = BoolExpr $ UintNotEqualFunc v1 v2

newFunction "now" _ = error "now function is not supported"

newFunction "print" _ = error "print function is not supported"

newFunction "range" _ = error "range function is not supported"

newFunction "toLower" [StringExpr s] = StringExpr $ StringToLowerFunc s
newFunction "toUpper" [StringExpr s] = StringExpr $ StringToUpperFunc s

newFunction "type" [BytesExpr b] = BoolExpr $ BytesTypeFunc b
newFunction "type" [BoolExpr b] = BoolExpr $ BoolTypeFunc b
newFunction "type" [DoubleExpr b] = BoolExpr $ DoubleTypeFunc b
newFunction "type" [IntExpr b] = BoolExpr $ IntTypeFunc b
newFunction "type" [UintExpr b] = BoolExpr $ UintTypeFunc b
newFunction "type" [StringExpr b] = BoolExpr $ StringTypeFunc b

newFunction s t = error $ "unknown function: " ++ s ++ " for types: " ++ show t

uBuiltIn :: [(String, JSValue)] -> Expr
uBuiltIn kvs = let
	varExpr = uExprs $ getObject kvs "Expr"
	constExpr = constToVar varExpr
	name = funcName (getString (getObject kvs "Symbol") "Value")
	in newFunction name [constExpr, varExpr]

constToVar :: Expr -> Expr
constToVar (BoolExpr (BoolConst _)) = BoolExpr BoolVariable
constToVar (DoubleExpr (DoubleConst _)) = DoubleExpr DoubleVariable
constToVar (IntExpr (IntConst _)) = IntExpr IntVariable
constToVar (UintExpr (UintConst _)) = UintExpr UintVariable
constToVar (BytesExpr (BytesConst _)) = BytesExpr BytesVariable
constToVar (StringExpr (StringConst _)) = StringExpr StringVariable

funcName :: String -> String
funcName "==" = "eq"
funcName "!=" = "ne"
funcName "<" = "lt"
funcName ">" = "gt"
funcName "<=" = "le"
funcName ">=" = "ge"
funcName "~=" = "regex"
funcName "*=" = "contains"
funcName "^=" = "hasPrefix"
funcName "$=" = "hasSuffix"
funcName "::" = "type"

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
	otherwise -> error $ name ++ " is not a JSString, but a " ++ show v

getInt :: [(String, JSValue)] -> String -> Int
getInt pairs name = let v = getField pairs name in 
	case v of
	(JSRational _ n) -> truncate n
	otherwise -> error $ name ++ " is not a JSRational, but a " ++ show v

getArrayOfObjects :: [(String, JSValue)] -> String -> [[(String, JSValue)]]
getArrayOfObjects pairs name = let v = getField pairs name in 
	case v of
	(JSArray vs) -> map assertObject vs
	otherwise -> error $ name ++ " is not a JSArray, but a " ++ show v

assertObject :: JSValue -> [(String, JSValue)]
assertObject (JSObject o) = fromJSObject o
assertObject v = error $ "not an JSObject, but a " ++ show v

getObject :: [(String, JSValue)] -> String -> [(String, JSValue)]
getObject pairs name = let v = getField pairs name in 
	case v of
	(JSObject o) -> fromJSObject o
	otherwise -> error $ name ++ " is not an JSObject, but a " ++ show v