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
import Control.Monad.Except (Except, runExcept, throwError)

import Expr
import Exprs
import Exprs.Logic
import Exprs.Var
import Patterns

-- | parseGrammar parses the Relapse Grammar.
parseGrammar :: String -> Either ParseError Refs
parseGrammar = parse (grammar <* eof) ""

infixl 4 <++>
(<++>) :: CharParser () String -> CharParser () String -> CharParser () String
f <++> g = (++) <$> f <*> g

infixr 5 <::>
(<::>) :: CharParser () Char -> CharParser () String -> CharParser () String
f <::> g = (:) <$> f <*> g

check :: Except String a -> CharParser () a
check e = case runExcept e of
    (Left err) -> fail err
    (Right v) -> return v

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
_ws = _comment <|> () <$ space

ws :: CharParser () ()
ws = () <$ many _ws

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
_exponent = oneOf "eE" <::> (
    oneOf "+-" <::> many1 digit 
    <|> many1 digit)

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

_literal :: CharParser () AnyExpr
_literal = mkBoolExpr . boolExpr <$> bool
    <|> mkIntExpr . intExpr <$> intLit
    <|> mkUintExpr . uintExpr <$> uintCastLit
    <|> mkDoubleExpr . doubleExpr <$> doubleCastLit
    <|> mkStringExpr . stringExpr <$> stringLit
    <|> mkBytesExpr . bytesExpr <$> bytesCastLit

_terminal :: CharParser () AnyExpr
_terminal = (char '$' *> (
    mkBoolExpr varBoolExpr <$ string "bool"
    <|> mkIntExpr varIntExpr <$ string "int"
    <|> mkUintExpr varUintExpr <$ string "uint"
    <|> mkDoubleExpr varDoubleExpr <$ string "double"
    <|> mkStringExpr varStringExpr <$ string "string"
    <|> mkBytesExpr varBytesExpr <$ string "[]byte" ))
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

_builtin :: CharParser () AnyExpr
_builtin = mkBuiltIn <$> _builtinSymbol <*> (ws *> _expr) >>= check

_function :: CharParser () AnyExpr
_function = mkExpr <$> idLit <*> (char '(' *> sepBy (ws *> _expr <* ws) (char ',') <* char ')') >>= check

_listType :: CharParser () String
_listType = char '[' <::> char ']' <::> (
    string "bool"
    <|> string "int"
    <|> string "uint"
    <|> string "double"
    <|> string "string"
    <|> string "[]byte" )

_mustBool :: AnyExpr -> CharParser () (Expr Bool)
_mustBool = check . assertBool

newList :: String -> [AnyExpr] -> CharParser () AnyExpr
newList "[]bool" es = mkBoolsExpr . boolsExpr <$> mapM (check . assertBool) es
newList "[]int" es = mkIntsExpr . intsExpr <$> mapM (check . assertInt) es
newList "[]uint" es = mkUintsExpr . uintsExpr <$> mapM (check . assertUint) es
newList "[]double" es = mkDoublesExpr . doublesExpr <$> mapM (check . assertDouble) es
newList "[]string" es = mkStringsExpr . stringsExpr <$> mapM (check . assertString) es
newList "[][]byte" es = mkListOfBytesExpr . listOfBytesExpr <$> mapM (check . assertBytes) es

_list :: CharParser () AnyExpr
_list = do {
    ltype <- _listType;
    es <- ws *> char '{' *> sepBy (ws *> _expr <* ws) (char ',') <* char '}';
    newList ltype es
}

_expr :: CharParser () AnyExpr
_expr = try _terminal <|> _list <|> _function

expr :: CharParser () (Expr Bool)
expr = (try _terminal <|> _builtin <|> _function) >>= _mustBool

_nameString :: CharParser () (Expr Bool)
_nameString = (mkBuiltIn "==" <$> (_literal <|> (mkStringExpr . stringExpr <$> idLit))) >>= check >>= _mustBool

sepBy2 :: CharParser () a -> String -> CharParser () [a]
sepBy2 p sep = do {
    x1 <- p;
    string sep;
    x2 <- p;
    xs <- many (try (string sep *> p));
    return (x1:x2:xs)
}

_nameChoice :: CharParser () (Expr Bool)
_nameChoice = foldl1 orExpr <$> sepBy2 (ws *> nameExpr <* ws) "|"

nameExpr :: CharParser () (Expr Bool)
nameExpr =  (boolExpr True <$ char '_')
    <|> (notExpr <$> (char '!' *> ws *> char '(' *> ws *> nameExpr <* ws <* char ')'))
    <|> (char '(' *> ws *> _nameChoice <* ws <* char ')')
    <|> _nameString

_concatPattern :: CharParser () Pattern
_concatPattern = char '[' *> (foldl1 Concat <$> sepBy2 (ws *> pattern <* ws) ",") <* optional (char ',' <* ws) <* char ']'

_interleavePattern :: CharParser () Pattern
_interleavePattern = char '{' *> (foldl1 Interleave <$> sepBy2 (ws *> pattern <* ws) ";") <* optional (char ';' <* ws) <* char '}'

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
            (first <$ char '|' >>= _orList) <|> 
            (first <$ char '&' >>= _andList)
        ) <* char ')'
    )
}

_orList :: Pattern -> CharParser () Pattern
_orList p = Or p . foldl1 Or <$> sepBy1 (ws *> pattern <* ws) (char '|')

_andList :: Pattern -> CharParser () Pattern
_andList p = And p . foldl1 And <$> sepBy1 (ws *> pattern <* ws) (char '&')

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
    <|> flip Node Empty <$> ( (string "->" *> expr ) <|> (_builtin >>= _mustBool) )

newContains :: CharParser () AnyExpr -> CharParser () Pattern
newContains e = flip Node Empty <$> ((mkBuiltIn "*=" <$> e) >>= check >>= _mustBool)

pattern :: CharParser () Pattern
pattern = char '*' *> (
        (char '=' *> (newContains (ws *> _expr)))
        <|> return ZAny
    ) <|> _parenPattern
    <|> _refPattern
    <|> try _emptyPattern
    <|> try _treenodePattern
    <|> try _depthPattern
    <|> _notPattern
    
_patternDecl :: CharParser () Refs
_patternDecl = newRef <$> (char '#' *> ws *> idLit) <*> (ws *> char '=' *> ws *> pattern)

grammar :: CharParser () Refs
grammar = ws *> (foldl1 union <$> many1 (_patternDecl <* ws))
    <|> union <$> (newRef "main" <$> pattern) <*> (foldl union emptyRef <$> many (ws *> _patternDecl <* ws))

