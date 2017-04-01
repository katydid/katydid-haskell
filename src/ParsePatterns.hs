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
uOr kvs = Or (uPattern $ getObject kvs "LeftPattern") (uPattern $ getObject kvs "RightPattern")

uAnd :: [(String, JSValue)] -> Pattern
uAnd kvs = And (uPattern $ getObject kvs "LeftPattern") (uPattern $ getObject kvs "RightPattern")

uZeroOrMore :: [(String, JSValue)] -> Pattern
uZeroOrMore kvs = ZeroOrMore (uPattern $ getObject kvs "Pattern")

uNot :: [(String, JSValue)] -> Pattern
uNot kvs = Not (uPattern $ getObject kvs "Pattern")

uContains :: [(String, JSValue)] -> Pattern
uContains kvs = Contains (uPattern $ getObject kvs "Pattern")

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
uName' ("DoubleValue", (JSRational _ num)) = DoubleEqualFunc (DoubleConst (fromRational num)) DoubleVariable
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
uTerminal ("DoubleValue", JSRational _ n) = DoubleExpr (DoubleConst (fromRational n))
uTerminal ("IntValue", JSRational _ n) = IntExpr (IntConst $ truncate n)
uTerminal ("UintValue", JSRational _ n) = UintExpr (UintConst $ truncate n)
uTerminal ("BoolValue", JSBool b) = BoolExpr (BoolConst b)
uTerminal ("StringValue", JSString s) = StringExpr (StringConst $ fromJSString s)
uTerminal ("BytesValue", JSString s) = BytesExpr (BytesConst $ fromJSString s) -- TODO bytes
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
uFunction kvs = case newFunction (getString kvs "Name") (map uExprs (getArrayOfObjects kvs "Params")) of
	(Right e) -> e
	(Left err) -> error err

newFunction :: String -> [Expr] -> Either String Expr
newFunction "not" [BoolExpr b] = Right $ BoolExpr $ NotFunc b
newFunction "and" [BoolExpr b1, BoolExpr b2] = Right $ BoolExpr $ AndFunc b1 b2
newFunction "or" [BoolExpr b1, BoolExpr b2] = Right $ BoolExpr $ OrFunc b1 b2

newFunction "contains" [IntExpr i,IntListExpr is] = Right $ BoolExpr $ IntListContainsFunc i is
newFunction "contains" [StringExpr s, StringListExpr ss] = Right $ BoolExpr $ StringListContainsFunc s ss
newFunction "contains" [UintExpr u, UintListExpr us] = Right $ BoolExpr $ UintListContainsFunc u us
newFunction "contains" [StringExpr s, StringExpr ss] = Right $ BoolExpr $ StringContainsFunc s ss

newFunction "elem" [BytesListExpr es, IntExpr i] = Right $ BytesExpr $ BytesListElemFunc es i
newFunction "elem" [BoolListExpr es, IntExpr i] = Right $ BoolExpr $ BoolListElemFunc es i
newFunction "elem" [DoubleListExpr es, IntExpr i] = Right $ DoubleExpr $ DoubleListElemFunc es i
newFunction "elem" [IntListExpr es, IntExpr i] = Right $ IntExpr $ IntListElemFunc es i
newFunction "elem" [StringListExpr es, IntExpr i] = Right $ StringExpr $ StringListElemFunc es i
newFunction "elem" [UintListExpr es, IntExpr i] = Right $ UintExpr $ UintListElemFunc es i

newFunction "eq" [BytesExpr v1, BytesExpr v2] = Right $ BoolExpr $ BytesEqualFunc v1 v2
newFunction "eq" [BoolExpr v1, BoolExpr v2] = Right $ BoolExpr $ BoolEqualFunc v1 v2
newFunction "eq" [DoubleExpr v1, DoubleExpr v2] = Right $ BoolExpr $ DoubleEqualFunc v1 v2
newFunction "eq" [IntExpr v1, IntExpr v2] = Right $ BoolExpr $ IntEqualFunc v1 v2
newFunction "eq" [StringExpr v1, StringExpr v2] = Right $ BoolExpr $ StringEqualFunc v1 v2
newFunction "eq" [UintExpr v1, UintExpr v2] = Right $ BoolExpr $ UintEqualFunc v1 v2

newFunction "eqFold" _ = Left "eqFold function is not supported"

newFunction "ge" [BytesExpr v1, BytesExpr v2] = Right $ BoolExpr $ BytesGreaterOrEqualFunc v1 v2
newFunction "ge" [DoubleExpr v1, DoubleExpr v2] = Right $ BoolExpr $ DoubleGreaterOrEqualFunc v1 v2
newFunction "ge" [IntExpr v1, IntExpr v2] = Right $ BoolExpr $ IntGreaterOrEqualFunc v1 v2
newFunction "ge" [UintExpr v1, UintExpr v2] = Right $ BoolExpr $ UintGreaterOrEqualFunc v1 v2

newFunction "gt" [BytesExpr v1, BytesExpr v2] = Right $ BoolExpr $ BytesGreaterThanFunc v1 v2
newFunction "gt" [DoubleExpr v1, DoubleExpr v2] = Right $ BoolExpr $ DoubleGreaterThanFunc v1 v2
newFunction "gt" [IntExpr v1, IntExpr v2] = Right $ BoolExpr $ IntGreaterThanFunc v1 v2
newFunction "gt" [UintExpr v1, UintExpr v2] = Right $ BoolExpr $ UintGreaterThanFunc v1 v2

newFunction "hasPrefix" [StringExpr v1, StringExpr v2] = Right $ BoolExpr $ StringHasPrefixFunc v1 v2
newFunction "hasSuffix" [StringExpr v1, StringExpr v2] = Right $ BoolExpr $ StringHasSuffixFunc v1 v2

newFunction "le" [BytesExpr v1, BytesExpr v2] = Right $ BoolExpr $ BytesLessOrEqualFunc v1 v2
newFunction "le" [DoubleExpr v1, DoubleExpr v2] = Right $ BoolExpr $ DoubleLessOrEqualFunc v1 v2
newFunction "le" [IntExpr v1, IntExpr v2] = Right $ BoolExpr $ IntLessOrEqualFunc v1 v2
newFunction "le" [UintExpr v1, UintExpr v2] = Right $ BoolExpr $ UintLessOrEqualFunc v1 v2

newFunction "length" [BytesListExpr vs] = Right $ IntExpr $ BytesListLengthFunc vs
newFunction "length" [BoolListExpr vs] = Right $ IntExpr $ BoolListLengthFunc vs
newFunction "length" [BytesExpr vs] = Right $ IntExpr $ BytesLengthFunc vs
newFunction "length" [DoubleListExpr vs] = Right $ IntExpr $ DoubleListLengthFunc vs
newFunction "length" [IntListExpr vs] = Right $ IntExpr $ IntListLengthFunc vs
newFunction "length" [StringListExpr vs] = Right $ IntExpr $ StringListLengthFunc vs
newFunction "length" [UintListExpr vs] = Right $ IntExpr $ UintListLengthFunc vs
newFunction "length" [StringExpr vs] = Right $ IntExpr $ StringLengthFunc vs

newFunction "lt" [BytesExpr v1, BytesExpr v2] = Right $ BoolExpr $ BytesLessThanFunc v1 v2
newFunction "lt" [DoubleExpr v1, DoubleExpr v2] = Right $ BoolExpr $ DoubleLessThanFunc v1 v2
newFunction "lt" [IntExpr v1, IntExpr v2] = Right $ BoolExpr $ IntLessThanFunc v1 v2
newFunction "lt" [UintExpr v1, UintExpr v2] = Right $ BoolExpr $ UintLessThanFunc v1 v2

newFunction "ne" [BytesExpr v1, BytesExpr v2] = Right $ BoolExpr $ BytesNotEqualFunc v1 v2
newFunction "ne" [BoolExpr v1, BoolExpr v2] = Right $ BoolExpr $ BoolNotEqualFunc v1 v2
newFunction "ne" [DoubleExpr v1, DoubleExpr v2] = Right $ BoolExpr $ DoubleNotEqualFunc v1 v2
newFunction "ne" [IntExpr v1, IntExpr v2] = Right $ BoolExpr $ IntNotEqualFunc v1 v2
newFunction "ne" [StringExpr v1, StringExpr v2] = Right $ BoolExpr $ StringNotEqualFunc v1 v2
newFunction "ne" [UintExpr v1, UintExpr v2] = Right $ BoolExpr $ UintNotEqualFunc v1 v2

newFunction "now" _ = Left "now function is not supported"

newFunction "print" _ = Left "print function is not supported"

newFunction "range" _ = Left "range function is not supported"

newFunction "toLower" [StringExpr s] = Right $ StringExpr $ StringToLowerFunc s
newFunction "toUpper" [StringExpr s] = Right $ StringExpr $ StringToUpperFunc s

newFunction "type" [BytesExpr b] = Right $ BoolExpr $ BytesTypeFunc b
newFunction "type" [BoolExpr b] = Right $ BoolExpr $ BoolTypeFunc b
newFunction "type" [DoubleExpr b] = Right $ BoolExpr $ DoubleTypeFunc b
newFunction "type" [IntExpr b] = Right $ BoolExpr $ IntTypeFunc b
newFunction "type" [UintExpr b] = Right $ BoolExpr $ UintTypeFunc b
newFunction "type" [StringExpr b] = Right $ BoolExpr $ StringTypeFunc b

newFunction "regex" [StringExpr v1, StringExpr v2] = Right $ BoolExpr $ RegexFunc v1 v2

newFunction s t = Left $ "unknown function: " ++ s ++ " for types: " ++ show t

uBuiltIn :: [(String, JSValue)] -> Expr
uBuiltIn kvs = let
	constExpr = uExprs $ getObject kvs "Expr"
	symbol = getString (getObject kvs "Symbol") "Value"
	in case newBuiltIn symbol constExpr of
		(Right e) -> e
		(Left err) -> error err

newBuiltIn :: String -> Expr -> Either String Expr
newBuiltIn symbol constExpr =
	let name = funcName symbol
	in if name /= "type" then
		newFunction name [(constToVar constExpr), constExpr]
	else
		newFunction name [constExpr]

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
funcName name = error $ "unexpected funcName: <" ++ name ++ ">"

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