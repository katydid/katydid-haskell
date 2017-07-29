module Parser (
    parseGrammar, grammar, pattern, nameExpr, expr, 
    idLit, bytesCastLit, stringLit, doubleCastLit, uintCastLit, intLit, ws
) where

import Text.ParserCombinators.Parsec
import Numeric (readDec, readOct, readHex, readFloat)
import Data.Char (chr)

import Values
import Patterns
import ParsePatterns

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

_literal :: CharParser () Expr
_literal = BoolExpr . BoolConst <$> bool
    <|> IntExpr . IntConst <$> intLit
    <|> UintExpr . UintConst <$> uintCastLit
    <|> DoubleExpr . DoubleConst <$> doubleCastLit
    <|> StringExpr . StringConst <$> stringLit
    <|> BytesExpr . BytesConst <$> bytesCastLit

_terminal :: CharParser () Expr
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

check :: Either String Expr -> CharParser () Expr
check (Right r) = return r
check (Left l) = fail l

_builtin :: CharParser () Expr
_builtin = newBuiltIn <$> _builtinSymbol <*> (ws *> _expr) >>= check

_function :: CharParser () Expr
_function = newFunction <$> idLit <*> (char '(' *> sepBy (ws *> _expr <* ws) (char ',') <* char ')') >>= check

_listType :: CharParser () String
_listType = string "[]" <++> (
    string "bool"
    <|> string "int"
    <|> string "uint"
    <|> string "double"
    <|> string "string"
    <|> string "[]byte" )

_mustBool :: Expr -> CharParser () BoolExpr
_mustBool (BoolExpr e) = return e
_mustBool e = fail $ "want BoolExpr, got: " ++ show e

_mustInt :: Expr -> CharParser () IntExpr
_mustInt (IntExpr e) = return e
_mustInt e = fail $ "want IntExpr, got: " ++ show e

_mustUint :: Expr -> CharParser () UintExpr
_mustUint (UintExpr e) = return e
_mustUint e = fail $ "want UintExpr, got: " ++ show e

_mustDouble :: Expr -> CharParser () DoubleExpr
_mustDouble (DoubleExpr e) = return e
_mustDouble e = fail $ "want DoubleExpr, got: " ++ show e

_mustString :: Expr -> CharParser () StringExpr
_mustString (StringExpr e) = return e
_mustString e = fail $ "want StringExpr, got: " ++ show e

_mustBytes :: Expr -> CharParser () BytesExpr
_mustBytes (BytesExpr e) = return e
_mustBytes e = fail $ "want BytesExpr, got: " ++ show e

newList :: String -> [Expr] -> CharParser () Expr
newList "[]bool" es = BoolListExpr <$> mapM _mustBool es
newList "[]int" es = IntListExpr <$> mapM _mustInt es
newList "[]uint" es = UintListExpr <$> mapM _mustUint es
newList "[]double" es = DoubleListExpr <$> mapM _mustDouble es
newList "[]string" es = StringListExpr <$> mapM _mustString es
newList "[][]byte" es = BytesListExpr <$> mapM _mustBytes es

_list :: CharParser () Expr
_list = do {
    ltype <- _listType;
    es <- ws *> char '{' *> sepBy (ws *> _expr <* ws) (char ',') <* char '}';
    newList ltype es
}

_expr :: CharParser () Expr
_expr = try _terminal <|> _list <|> _function

expr :: CharParser () BoolExpr
expr = (try _terminal <|> _builtin <|> _function) >>= _mustBool

_name :: CharParser () BoolExpr
_name = (newBuiltIn "==" <$> (_literal <|> (StringExpr . StringConst <$> idLit))) >>= check >>= _mustBool

sepBy2 :: CharParser () a -> String -> CharParser () [a]
sepBy2 p sep = do {
    x1 <- p;
    string sep;
    x2 <- p;
    xs <- many (try (string sep *> p));
    return (x1:x2:xs)
}

_nameChoice :: CharParser () BoolExpr
_nameChoice = foldl1 OrFunc <$> sepBy2 (ws *> nameExpr <* ws) "|"

nameExpr :: CharParser () BoolExpr
nameExpr =  (BoolConst True <$ char '_')
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

parseGrammar :: String -> Either ParseError Refs
parseGrammar s = parse (grammar <* eof) "" s