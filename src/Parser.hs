-- |
-- This module parses the Relapse Grammar using the Parsec Library.

module Parser (
    -- * Parse Grammar
    parseGrammar
    -- * Internal functions
    -- | These functions are exposed for testing purposes.
    , grammar, pattern, nameExpr, expr, 
    idLit, bytesCastLit, stringLit, doubleCastLit, uintCastLit, intLit, ws
) where

import Text.ParserCombinators.Parsec
import Numeric (readDec, readOct, readHex, readFloat)
import Data.Char (chr)

import Expr
import Patterns

-- | parseGrammar parses the Relapse Grammar.
parseGrammar :: String -> Either ParseError Refs
parseGrammar s = parse (grammar <* eof) "" s

infixl 4 <++>
(<++>) :: CharParser () String -> CharParser () String -> CharParser () String
f <++> g = (++) <$> f <*> g

infixr 5 <::>
(<::>) :: CharParser () Char -> CharParser () String -> CharParser () String
f <::> g = (:) <$> f <*> g

empty :: CharParser () String
empty = return ""

opt :: CharParser () Char -> CharParser () String
opt p = (:"") <$> p <|> empty

_lineComment :: CharParser () ()
_lineComment = char '/' *> many (noneOf "\n") <* char '\n' *> return ()

_blockComment :: CharParser () ()
_blockComment = char '*' *> many (noneOf "*") <* char '*' <* char '/' *> return ()

_comment :: CharParser () ()
_comment = char '/' *> (_lineComment <|> _blockComment)

_ws :: CharParser () ()
_ws = _comment <|> (() <$ space)

ws :: CharParser () ()
ws = () <$ (many _ws)

bool :: CharParser () Bool
bool = True <$ string "true"
    <|> False <$ string "false"

_decimalLit :: CharParser () Int
_decimalLit = oneOf "123456789" <::> many digit >>= _read readDec

_octalLit :: CharParser () Int
_octalLit = many1 octDigit >>= _read readOct

_hexLit :: CharParser () Int
_hexLit = many1 hexDigit >>= _read readHex

_read :: ReadS a -> String -> CharParser () a
_read read s = case read s of
    [(n, "")]   -> return n
    ((n, ""):_) -> return n
    _           -> fail "digit"

_optionalSign :: (Num a) => CharParser () a
_optionalSign = -1 <$ char '-' <|> return 1

_signedIntLit :: CharParser () Int
_signedIntLit = (*) <$> _optionalSign <*> _intLit

_intLit :: CharParser () Int
_intLit = _decimalLit 
    <|> char '0' *> (_octalLit 
                    <|> (oneOf "xX" *> _hexLit)
                    <|> return 0
    )

intLit :: CharParser () Int
intLit = string "int(" *> _signedIntLit <* char ')'
    <|> _signedIntLit
    <?> "int_lit"

uintCastLit :: CharParser () Int
uintCastLit = string "uint(" *> _intLit <* char ')'

_exponent :: CharParser () String
_exponent = oneOf "eE" <::> opt (oneOf "+-") <++> many1 digit

_floatLit :: CharParser () Double
_floatLit = do
    i <- many1 digit
    e <- _exponent 
        <|> ((string "." <|> empty) <++> 
            (_exponent 
            <|> many1 digit <++> 
                (_exponent
                <|> empty)
            )
        ) 
        <|> empty
    _read readFloat (i ++ e)

doubleCastLit :: CharParser () Double
doubleCastLit = string "double(" *> ((*) <$> _optionalSign <*> _floatLit) <* char ')'

idLit :: CharParser () String
idLit = (letter <|> char '_') <::> many (alphaNum <|> char '_')

_qualid :: CharParser () String
_qualid = idLit <++> (concat <$> many (char '.' <::> idLit))

_bigUValue :: CharParser () Char
_bigUValue = char 'U' *> do {
    hs <- count 8 hexDigit;
    n <- _read readHex hs;
    return $ toEnum n
}

_littleUValue :: CharParser () Char
_littleUValue = char 'u' *> do { 
    hs <- count 4 hexDigit;
    n <- _read readHex hs;
    return $ toEnum n
}

_escapedChar :: CharParser () Char
_escapedChar = choice (zipWith (\c r -> r <$ char c) "abnfrtv'\\\"/" "\a\b\n\f\r\t\v\'\\\"/")

_unicodeValue :: CharParser () Char
_unicodeValue = (char '\\' *> 
    (_bigUValue 
        <|> _littleUValue 
        <|> _hexByteUValue 
        <|> _escapedChar
        <|> _octalByteUValue)
    ) <|> noneOf "\\\""

_interpretedString :: CharParser () String
_interpretedString = between (char '"') (char '"') (many _unicodeValue)

_rawString :: CharParser () String
_rawString = between (char '`') (char '`') (many $ noneOf "`")

stringLit :: CharParser () String
stringLit = _rawString <|> _interpretedString

_hexByteUValue :: CharParser () Char
_hexByteUValue = char 'x' *> do {
    hs <- count 2 hexDigit;
    n <- _read readHex hs;
    return $ chr n
}

_octalByteUValue :: CharParser () Char
_octalByteUValue = do {
    os <- count 3 octDigit;
    n <- _read readOct os;
    return $ toEnum n
}

_byteLit :: CharParser () Char
_byteLit = do {
    i <- _intLit;
    if i > 255 then
        fail $ "too large for byte: " ++ show i
    else
        return $ chr i
}

_byteElem :: CharParser () Char
_byteElem = _byteLit <|> between (char '\'') (char '\'') (_unicodeValue <|> _octalByteUValue <|> _hexByteUValue)

bytesCastLit :: CharParser () String
bytesCastLit = string "[]byte{" *> sepBy (ws *> _byteElem <* ws) (char ',') <* char '}'

data ParsedExpr 
    = BoolExpr (Expr Bool)
    | DoubleExpr (Expr Double)
    | IntExpr (Expr Int)
    | UintExpr (Expr Uint)
    | StringExpr (Expr String)
    | BytesExpr (Expr Bytes)
    | BoolListExpr [(Expr Bool)]
    | DoubleListExpr [(Expr Double)]
    | IntListExpr [(Expr Int)]
    | UintListExpr [(Expr Uint)]
    | StringListExpr [(Expr String)]
    | BytesListExpr [(Expr Bytes)]
    deriving Show

_literal :: CharParser () ParsedExpr
_literal = BoolExpr . Const <$> bool
    <|> IntExpr . Const <$> intLit
    <|> UintExpr . Const <$> uintCastLit
    <|> DoubleExpr . Const <$> doubleCastLit
    <|> StringExpr . Const <$> stringLit
    <|> BytesExpr . Const <$> bytesCastLit

_terminal :: CharParser () ParsedExpr
_terminal = (char '$' *> (
    BoolExpr BoolVariable <$ string "bool"
    <|> IntExpr IntVariable <$ string "int"
    <|> UintExpr UintVariable <$ string "uint"
    <|> DoubleExpr DoubleVariable <$ string "double"
    <|> StringExpr StringVariable <$ string "string"
    <|> BytesExpr BytesVariable <$ string "[]byte" ))
    <|> _literal

_builtinSymbol :: CharParser () String
_builtinSymbol = string "==" 
    <|> string "!=" 
    <|> char '<' <::> opt (char '=')
    <|> char '>' <::> opt (char '=')
    <|> string "~="
    <|> string "*="
    <|> string "^="
    <|> string "$="
    <|> string "::"

check :: Either String ParsedExpr -> CharParser () ParsedExpr
check (Right r) = return r
check (Left l) = fail l

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

constToVar :: ParsedExpr -> ParsedExpr
constToVar (BoolExpr Const{}) = BoolExpr BoolVariable
constToVar (DoubleExpr Const{}) = DoubleExpr DoubleVariable
constToVar (IntExpr Const{}) = IntExpr IntVariable
constToVar (UintExpr Const{}) = UintExpr UintVariable
constToVar (BytesExpr Const{}) = BytesExpr BytesVariable
constToVar (StringExpr Const{}) = StringExpr StringVariable

-- |
-- newBuiltIn parsers a builtin function to a relapse expression.
newBuiltIn :: String -> ParsedExpr -> Either String ParsedExpr
newBuiltIn symbol constExpr = funcName symbol >>= (\name ->
        if name /= "type" then
            newFunction name [(constToVar constExpr), constExpr]
        else
            newFunction name [constExpr]
    )

_builtin :: CharParser () ParsedExpr
_builtin = newBuiltIn <$> _builtinSymbol <*> (ws *> _expr) >>= check

-- |
-- newFunction parsers a relapse function to a relapse expression.
newFunction :: String -> [ParsedExpr] -> Either String ParsedExpr
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

_function :: CharParser () ParsedExpr
_function = newFunction <$> idLit <*> (char '(' *> sepBy (ws *> _expr <* ws) (char ',') <* char ')') >>= check

_listType :: CharParser () String
_listType = string "[]" <++> (
    string "bool"
    <|> string "int"
    <|> string "uint"
    <|> string "double"
    <|> string "string"
    <|> string "[]byte" )

_mustBool :: ParsedExpr -> CharParser () (Expr Bool)
_mustBool (BoolExpr e) = return e
_mustBool e = fail $ "want BoolExpr, got: " ++ show e

_mustInt :: ParsedExpr -> CharParser () (Expr Int)
_mustInt (IntExpr e) = return e
_mustInt e = fail $ "want IntExpr, got: " ++ show e

_mustUint :: ParsedExpr -> CharParser () (Expr Uint)
_mustUint (UintExpr e) = return e
_mustUint e = fail $ "want UintExpr, got: " ++ show e

_mustDouble :: ParsedExpr -> CharParser () (Expr Double)
_mustDouble (DoubleExpr e) = return e
_mustDouble e = fail $ "want DoubleExpr, got: " ++ show e

_mustString :: ParsedExpr -> CharParser () (Expr String)
_mustString (StringExpr e) = return e
_mustString e = fail $ "want StringExpr, got: " ++ show e

_mustBytes :: ParsedExpr -> CharParser () (Expr Bytes)
_mustBytes (BytesExpr e) = return e
_mustBytes e = fail $ "want BytesExpr, got: " ++ show e

newList :: String -> [ParsedExpr] -> CharParser () ParsedExpr
newList "[]bool" es = BoolListExpr <$> mapM _mustBool es
newList "[]int" es = IntListExpr <$> mapM _mustInt es
newList "[]uint" es = UintListExpr <$> mapM _mustUint es
newList "[]double" es = DoubleListExpr <$> mapM _mustDouble es
newList "[]string" es = StringListExpr <$> mapM _mustString es
newList "[][]byte" es = BytesListExpr <$> mapM _mustBytes es

_list :: CharParser () ParsedExpr
_list = do {
    ltype <- _listType;
    es <- ws *> char '{' *> sepBy (ws *> _expr <* ws) (char ',') <* char '}';
    newList ltype es
}

_expr :: CharParser () ParsedExpr
_expr = try _terminal <|> _list <|> _function

expr :: CharParser () (Expr Bool)
expr = (try _terminal <|> _builtin <|> _function) >>= _mustBool

_name :: CharParser () (Expr Bool)
_name = (newBuiltIn "==" <$> (_literal <|> (StringExpr . Const <$> idLit))) >>= check >>= _mustBool

sepBy2 :: CharParser () a -> String -> CharParser () [a]
sepBy2 p sep = do {
    x1 <- p;
    string sep;
    x2 <- p;
    xs <- many (try (string sep *> p));
    return (x1:x2:xs)
}

_nameChoice :: CharParser () (Expr Bool)
_nameChoice = foldl1 OrFunc <$> sepBy2 (ws *> nameExpr <* ws) "|"

nameExpr :: CharParser () (Expr Bool)
nameExpr =  (Const True <$ char '_')
    <|> (NotFunc <$> (char '!' *> ws *> char '(' *> ws *> nameExpr <* ws <* char ')'))
    <|> (char '(' *> ws *> _nameChoice <* ws <* char ')')
    <|> _name

_concatPattern :: CharParser () Pattern
_concatPattern = char '[' *> (foldl1 Concat <$> sepBy2 (ws *> pattern <* ws) ",") <* (optional ((char ',') <* ws)) <* char ']'

_interleavePattern :: CharParser () Pattern
_interleavePattern = char '{' *> (foldl1 Interleave <$> sepBy2 (ws *> pattern <* ws) ";") <* (optional ((char ';') <* ws)) <* char '}'

_parenPattern :: CharParser () Pattern
_parenPattern = do {
    char '(';
    ws;
    first <- pattern;
    ws;
    ( char ')' *> ws *>
        (
            ZeroOrMore first <$ char '*'
            <|> Optional first <$ char '?'
        )
    ) <|> ( 
        (
            (first <$ (char '|') >>= _orList) <|> 
            (first <$ (char '&') >>= _andList)
        ) <* char ')'
    )
}

_orList :: Pattern -> CharParser () Pattern
_orList p = Or p <$> foldl1 Or <$> sepBy1 (ws *> pattern <* ws) (char '|')

_andList :: Pattern -> CharParser () Pattern
_andList p = And p <$> foldl1 And <$> sepBy1 (ws *> pattern <* ws) (char '&')

_refPattern :: CharParser () Pattern
_refPattern = Reference <$> (char '@' *> ws *> idLit)

_notPattern :: CharParser () Pattern
_notPattern = Not <$> (char '!' *> ws *> char '(' *> ws *> pattern <* ws <* char ')')

_emptyPattern :: CharParser () Pattern
_emptyPattern = Empty <$ string "<empty>"

_zanyPattern :: CharParser () Pattern
_zanyPattern = ZAny <$ string "*"

_containsPattern :: CharParser () Pattern
_containsPattern = Contains <$> (char '.' *> pattern)

_treenodePattern :: CharParser () Pattern
_treenodePattern = Node <$> nameExpr <*> ( ws *> ( try (char ':' *> ws *> pattern) <|> _depthPattern ) )

_depthPattern :: CharParser () Pattern
_depthPattern = _concatPattern <|> _interleavePattern <|> _containsPattern 
    <|> (flip Node) Empty <$> ( (string "->" *> expr ) <|> (_builtin >>= _mustBool) )

pattern :: CharParser () Pattern
pattern = _zanyPattern
    <|> _parenPattern
    <|> _refPattern
    <|> (try _emptyPattern)
    <|> (try _treenodePattern)
    <|> (try _depthPattern)
    <|> (_notPattern)
    
_patternDecl :: CharParser () Refs
_patternDecl = newRef <$> (char '#' *> ws *> idLit) <*> (ws *> char '=' *> ws *> pattern)

grammar :: CharParser () Refs
grammar = ws *> (foldl1 union <$> many1 (_patternDecl <* ws))
    <|> union <$> (newRef "main" <$> pattern) <*> (foldl union emptyRef <$> many (ws *> _patternDecl <* ws))
