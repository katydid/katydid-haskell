module ParsePatterns (
    Expr(..), newBuiltIn, newFunction, fromJson
) where

import Text.JSON (decode, Result(..), JSValue(..), fromJSString, fromJSObject)

import Patterns
import Values

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

fromJson :: String -> Either String Refs
fromJson s = unmarshal $ decode s

unmarshal :: Result JSValue -> Either String Refs
unmarshal (Error err) = fail err
unmarshal (Ok (JSObject o)) = uRefs $ fromJSObject o
unmarshal (Ok j) = fail $ "unexpected jsvalue = " ++ show j

uRefs :: [(String, JSValue)] -> Either String Refs
uRefs [] = return emptyRef
uRefs (("TopPattern", (JSObject pattern)):pairs) = do {
    p <- uPattern (fromJSObject pattern);
    rs <- uRefs pairs;
    return $ newRef "main" p `union` rs
}
uRefs (("PatternDecls", (JSArray patternDecls)):pairs) = do {
    p <- uPatternDecls patternDecls;
    rs <- uRefs pairs;
    return $ p `union` rs
}
uRefs (_:pairs) = uRefs pairs

uPatternDecls :: [JSValue] -> Either String Refs
uPatternDecls [] = return emptyRef
uPatternDecls ((JSObject o):patternDecls) = do {
    left <- uPatternDecl (fromJSObject o);
    right <- uPatternDecls patternDecls;
    return $ left `union` right
}

uPatternDecl :: [(String, JSValue)] -> Either String Refs
uPatternDecl kvs = do {
    name <- getString kvs "Name";
    p <- getObject kvs "Pattern";
    pattern <- uPattern p;
    return $ newRef name pattern
}

uPattern :: [(String, JSValue)] -> Either String Pattern
uPattern [("Empty", _)] = return Empty
uPattern [("TreeNode", JSObject o)] = uTreeNode (fromJSObject o)
uPattern [("LeafNode", JSObject o)] = uLeafNode (fromJSObject o)
uPattern [("Concat", JSObject o)] = uConcat (fromJSObject o)
uPattern [("Or", JSObject o)] = uOr (fromJSObject o)
uPattern [("And", JSObject o)] = uAnd (fromJSObject o)
uPattern [("ZeroOrMore", JSObject o)] = uZeroOrMore (fromJSObject o)
uPattern [("Reference", JSObject o)] = uReference (fromJSObject o)
uPattern [("Not", JSObject o)] = uNot (fromJSObject o)
uPattern [("ZAny", JSObject o)] = return ZAny
uPattern [("Contains", JSObject o)] = uContains (fromJSObject o)
uPattern [("Optional", JSObject o)] = uOptional (fromJSObject o)
uPattern [("Interleave", JSObject o)] = uInterleave (fromJSObject o)

uTreeNode :: [(String, JSValue)] -> Either String Pattern
uTreeNode kvs = do {
    name <- getObject kvs "Name";
    nameExpr <- uNameExpr name;
    p <- getObject kvs "Pattern";
    pattern <- uPattern p;
    return $ Node nameExpr pattern
}

uLeafNode :: [(String, JSValue)] -> Either String Pattern
uLeafNode kvs = getObject kvs "Expr" >>= uBoolExpr >>= return . (flip Node) Empty

uReference :: [(String, JSValue)] -> Either String Pattern
uReference kvs = getString kvs "Name" >>= return . Reference

uConcat :: [(String, JSValue)] -> Either String Pattern
uConcat kvs = do {
    left <- getObject kvs "LeftPattern";
    leftPattern <- uPattern left;
    right <- getObject kvs "RightPattern";
    rightPattern <- uPattern right;
    return $ Concat leftPattern rightPattern
}

uOr :: [(String, JSValue)] -> Either String Pattern
uOr kvs = do {
    left <- getObject kvs "LeftPattern";
    leftPattern <- uPattern left;
    right <- getObject kvs "RightPattern";
    rightPattern <- uPattern right;
    return $ Or leftPattern rightPattern
}

uAnd :: [(String, JSValue)] -> Either String Pattern
uAnd kvs = do {
    left <- getObject kvs "LeftPattern";
    leftPattern <- uPattern left;
    right <- getObject kvs "RightPattern";
    rightPattern <- uPattern right;
    return $ And leftPattern rightPattern
}

uZeroOrMore :: [(String, JSValue)] -> Either String Pattern
uZeroOrMore kvs = getObject kvs "Pattern" >>= uPattern >>= return . ZeroOrMore

uNot :: [(String, JSValue)] -> Either String Pattern
uNot kvs = getObject kvs "Pattern" >>= uPattern >>= return . Not

uContains :: [(String, JSValue)] -> Either String Pattern
uContains kvs = getObject kvs "Pattern" >>= uPattern >>= return . Contains

uOptional :: [(String, JSValue)] -> Either String Pattern
uOptional kvs = getObject kvs "Pattern" >>= uPattern >>= return . Optional

uInterleave :: [(String, JSValue)] -> Either String Pattern
uInterleave kvs = do {
    left <- getObject kvs "LeftPattern";
    leftPattern <- uPattern left;
    right <- getObject kvs "RightPattern";
    rightPattern <- uPattern right;
    return $ Interleave leftPattern rightPattern
}

uNameExpr :: [(String, JSValue)] -> Either String BoolExpr
uNameExpr [("Name", JSObject o)] = return $ uName (fromJSObject o)
uNameExpr [("AnyName", JSObject o)] = return $ BoolConst True
uNameExpr [("AnyNameEither", JSObject o)] = uNameEither (fromJSObject o)
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

uNameEither :: [(String, JSValue)] -> Either String BoolExpr
uNameEither kvs = getObject kvs "Either" >>= uNameExpr >>= return . NotFunc

uNameChoice :: [(String, JSValue)] -> Either String BoolExpr
uNameChoice kvs = do {
    left <- getObject kvs "Left";
    leftName <- uNameExpr left;
    right <- getObject kvs "Right";
    rightName <- uNameExpr right;
    return $ OrFunc leftName rightName
}

uBoolExpr :: [(String, JSValue)] -> Either String BoolExpr
uBoolExpr kvs = uExprs kvs >>= (\e ->
    case e of
        (BoolExpr v) -> return v
        _ -> fail $ "not a BoolExpr, but a " ++ show e
    )

uDoubleExpr :: [(String, JSValue)] -> Either String DoubleExpr
uDoubleExpr kvs = uExprs kvs >>= (\e ->
    case e of
        (DoubleExpr v) -> return v
        _ -> fail $ "not a DoubleExpr, but a " ++ show e
    )

uIntExpr :: [(String, JSValue)] -> Either String IntExpr
uIntExpr kvs = uExprs kvs >>= (\e ->
    case e of
        (IntExpr v) -> return v
        _ -> fail $ "not a IntExpr, but a " ++ show e
    )

uUintExpr :: [(String, JSValue)] -> Either String UintExpr
uUintExpr kvs = uExprs kvs >>= (\e -> 
    case e of
        (UintExpr v) -> return v
        _ -> fail $ "not a UintExpr, but a " ++ show e
    )

uStringExpr :: [(String, JSValue)] -> Either String StringExpr
uStringExpr kvs = uExprs kvs >>= (\e -> 
    case e of
        (StringExpr v) -> return v
        _ -> fail $ "not a StringExpr, but a " ++ show e
    )

uBytesExpr :: [(String, JSValue)] -> Either String BytesExpr
uBytesExpr kvs = uExprs kvs >>= (\e -> 
    case e of
        (BytesExpr v) -> return v
        _ -> fail $ "not a BytesExpr, but a " ++ show e
    )

uExprs :: [(String, JSValue)] -> Either String Expr
uExprs kvs = uExpr $ head $ filter (\(k,v) -> k /= "RightArrow" && k /= "Comma") kvs 

uExpr :: (String, JSValue) -> Either String Expr
uExpr ("Terminal", (JSObject o)) = return $ uTerminals $ fromJSObject o
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

uList :: [(String, JSValue)] -> Either String Expr
uList kvs = do {
    arr <- getArrayOfObjects kvs "Elems";
    typ <- getInt kvs "Type";
    case typ of
    101 -> fmap DoubleListExpr $ mapM uDoubleExpr arr
    103 -> fmap IntListExpr $ mapM uIntExpr arr
    104 -> fmap UintListExpr $ mapM uUintExpr arr
    108 -> fmap BoolListExpr $ mapM uBoolExpr arr
    109 -> fmap StringListExpr $ mapM uStringExpr arr
    112 -> fmap BytesListExpr $ mapM uBytesExpr arr
    201 -> fmap DoubleListExpr $ mapM uDoubleExpr arr
    203 -> fmap IntListExpr $ mapM uIntExpr arr
    204 -> fmap UintListExpr $ mapM uUintExpr arr
    208 -> fmap BoolListExpr $ mapM uBoolExpr arr
    209 -> fmap StringListExpr $ mapM uStringExpr arr
    212 -> fmap BytesListExpr $ mapM uBytesExpr arr
}

uFunction :: [(String, JSValue)] -> Either String Expr
uFunction kvs = do {
    name <- getString kvs "Name";
    arrayObjects <- getArrayOfObjects kvs "Params";
    exprs <- mapM uExprs arrayObjects;
    newFunction name exprs
}

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

uBuiltIn :: [(String, JSValue)] -> Either String Expr
uBuiltIn kvs = do {
    exprObject <- getObject kvs "Expr";
    symbolObject <- getObject kvs "Symbol";
    symbol <- getString symbolObject "Value";
    exprs <- uExprs exprObject;
    e <- newBuiltIn symbol exprs;
    return e
}

newBuiltIn :: String -> Expr -> Either String Expr
newBuiltIn symbol constExpr = funcName symbol >>= (\name ->
        if name /= "type" then
            newFunction name [(constToVar constExpr), constExpr]
        else
            newFunction name [constExpr]
    )

constToVar :: Expr -> Expr
constToVar (BoolExpr (BoolConst _)) = BoolExpr BoolVariable
constToVar (DoubleExpr (DoubleConst _)) = DoubleExpr DoubleVariable
constToVar (IntExpr (IntConst _)) = IntExpr IntVariable
constToVar (UintExpr (UintConst _)) = UintExpr UintVariable
constToVar (BytesExpr (BytesConst _)) = BytesExpr BytesVariable
constToVar (StringExpr (StringConst _)) = StringExpr StringVariable

funcName :: String -> Either String String
funcName "==" = return "eq"
funcName "!=" = return "ne"
funcName "<" = return "lt"
funcName ">" = return "gt"
funcName "<=" = return "le"
funcName ">=" = return "ge"
funcName "~=" = return "regex"
funcName "*=" = return "contains"
funcName "^=" = return "hasPrefix"
funcName "$=" = return "hasSuffix"
funcName "::" = return "type"
funcName name = fail $ "unexpected funcName: <" ++ name ++ ">"

-- JSON helper functions

getField :: [(String, JSValue)] -> String -> Either String JSValue
getField pairs name = let filtered = filter (\(k,_) -> (k == name)) pairs
    in case filtered of
    [] -> fail $ "no field with name: " ++ name
    vs -> return $ snd $ head $ vs

getString :: [(String, JSValue)] -> String -> Either String String
getString pairs name = getField pairs name >>= (\v -> 
    case v of
        (JSString s) -> return $ fromJSString s
        _ -> fail $ name ++ " is not a JSString, but a " ++ show v
    )

getInt :: [(String, JSValue)] -> String -> Either String Int
getInt pairs name = getField pairs name >>= (\v ->
    case v of
        (JSRational _ n) -> return $ truncate n
        _ -> fail $ name ++ " is not a JSRational, but a " ++ show v
    )

getArrayOfObjects :: [(String, JSValue)] -> String -> Either String [[(String, JSValue)]]
getArrayOfObjects pairs name = getField pairs name >>= (\v ->
    case v of
        (JSArray vs) -> mapM assertObject vs
        _ -> fail $ name ++ " is not a JSArray, but a " ++ show v
    )

assertObject :: JSValue -> Either String [(String, JSValue)]
assertObject (JSObject o) = return $ fromJSObject o
assertObject v = fail $ "not an JSObject, but a " ++ show v

getObject :: [(String, JSValue)] -> String -> Either String [(String, JSValue)]
getObject pairs name = getField pairs name >>= (\v -> 
    case v of
        (JSObject o) -> return $ fromJSObject o
        _ -> fail $ name ++ " is not an JSObject, but a " ++ show v
    )