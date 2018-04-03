-- |
-- This module parses the Relapse Grammar using the Parsec Library.

module Parser (
    -- * Parse Grammar
    parseGrammar, parseGrammarWithUDFs
    -- * Internal functions
    -- | These functions are exposed for testing purposes.
    , grammar, pattern, nameExpr, expr, 
    idLit, bytesCastLit, stringLit, doubleCastLit, uintCastLit, intLit, ws
) where

import Text.ParserCombinators.Parsec
import Numeric (readDec, readOct, readHex, readFloat)
import Data.Char (chr)
import Control.Monad.Except (Except, runExcept, throwError)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as ByteString

import Expr
import Exprs
import Exprs.Logic
import Exprs.Var
import Patterns

-- | parseGrammar parses the Relapse Grammar.
parseGrammar :: String -> Either ParseError Refs
parseGrammar = parseGrammarWithUDFs stdOnly

parseGrammarWithUDFs :: MkFunc -> String -> Either ParseError Refs
parseGrammarWithUDFs extraMkFunc = 
    let mkFunc n es = case runExcept $ mkExpr n es of
            (Left _) -> extraMkFunc n es
            (Right v) -> return v
    in parse (grammar mkFunc <* eof) ""

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

uintLit :: CharParser () Word
uintLit = do {
    i <- intLit;
    if i < 0
        then fail "negative uint" 
        else return $ fromIntegral i;
}

uintCastLit :: CharParser () Word
uintCastLit = string "uint(" *> uintLit <* char ')'

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
    <|> mkStringExpr . stringExpr . Text.pack <$> stringLit
    <|> mkBytesExpr . bytesExpr . ByteString.pack <$> bytesCastLit

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

_builtin :: MkFunc -> CharParser () AnyExpr
_builtin mkFunc = mkBuiltIn <$> _builtinSymbol <*> (ws *> _expr mkFunc) >>= check

_function :: MkFunc -> CharParser () AnyExpr
_function mkFunc = mkFunc <$> idLit <*> (char '(' *> sepBy (ws *> _expr mkFunc <* ws) (char ',') <* char ')') >>= check

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

_list :: MkFunc -> CharParser () AnyExpr
_list mkFunc = do {
    ltype <- _listType;
    es <- ws *> char '{' *> sepBy (ws *> _expr mkFunc <* ws) (char ',') <* char '}';
    newList ltype es
}

_expr :: MkFunc -> CharParser () AnyExpr
_expr mkFunc = try _terminal <|> _list mkFunc <|> _function mkFunc

expr :: MkFunc -> CharParser () (Expr Bool)
expr mkFunc = (try _terminal <|> _builtin mkFunc <|> _function mkFunc) >>= _mustBool

_nameString :: CharParser () (Expr Bool)
_nameString = (mkBuiltIn "==" <$> 
    (_literal <|> 
    (mkStringExpr . stringExpr . Text.pack <$> idLit))) 
    >>= check >>= _mustBool

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

_concatPattern :: MkFunc -> CharParser () Pattern
_concatPattern mkFunc = char '[' *> (foldl1 Concat <$> sepBy2 (ws *> pattern mkFunc <* ws) ",") <* optional (char ',' <* ws) <* char ']'

_interleavePattern :: MkFunc -> CharParser () Pattern
_interleavePattern mkFunc = char '{' *> (foldl1 Interleave <$> sepBy2 (ws *> pattern mkFunc <* ws) ";") <* optional (char ';' <* ws) <* char '}'

_parenPattern :: MkFunc -> CharParser () Pattern
_parenPattern mkFunc = do {
    char '(';
    ws;
    first <- pattern mkFunc;
    ws;
    ( char ')' *> ws *>
        (
            ZeroOrMore first <$ char '*'
            <|> Optional first <$ char '?'
        )
    ) <|> ( 
        (
            (first <$ char '|' >>= _orList mkFunc) <|> 
            (first <$ char '&' >>= _andList mkFunc)
        ) <* char ')'
    )
}

_orList :: MkFunc -> Pattern -> CharParser () Pattern
_orList mkFunc p = Or p . foldl1 Or <$> sepBy1 (ws *> pattern mkFunc <* ws) (char '|')

_andList :: MkFunc -> Pattern -> CharParser () Pattern
_andList mkFunc p = And p . foldl1 And <$> sepBy1 (ws *> pattern mkFunc <* ws) (char '&')

_refPattern :: CharParser () Pattern
_refPattern = Reference <$> (char '@' *> ws *> idLit)

_notPattern :: MkFunc -> CharParser () Pattern
_notPattern mkFunc = Not <$> (char '!' *> ws *> char '(' *> ws *> pattern mkFunc <* ws <* char ')')

_emptyPattern :: CharParser () Pattern
_emptyPattern = Empty <$ string "<empty>"

_zanyPattern :: CharParser () Pattern
_zanyPattern = ZAny <$ string "*"

_containsPattern :: MkFunc -> CharParser () Pattern
_containsPattern mkFunc = Contains <$> (char '.' *> pattern mkFunc)

_treenodePattern :: MkFunc -> CharParser () Pattern
_treenodePattern mkFunc = Node <$> nameExpr <*> ( ws *> ( try (char ':' *> ws *> pattern mkFunc) <|> _depthPattern mkFunc) )

_depthPattern :: MkFunc -> CharParser () Pattern
_depthPattern mkFunc = _concatPattern mkFunc <|> _interleavePattern mkFunc<|> _containsPattern mkFunc
    <|> flip Node Empty <$> ( (string "->" *> expr mkFunc) <|> (_builtin mkFunc>>= _mustBool) )

newContains :: CharParser () AnyExpr -> CharParser () Pattern
newContains e = flip Node Empty <$> ((mkBuiltIn "*=" <$> e) >>= check >>= _mustBool)

pattern :: MkFunc -> CharParser () Pattern
pattern mkFunc = char '*' *> (
        (char '=' *> newContains (ws *> _expr mkFunc))
        <|> return ZAny
    ) <|> _parenPattern mkFunc
    <|> _refPattern
    <|> try _emptyPattern
    <|> try (_treenodePattern mkFunc)
    <|> try (_depthPattern mkFunc)
    <|> _notPattern mkFunc
    
_patternDecl :: MkFunc -> CharParser () Refs
_patternDecl mkFunc = newRef <$> (char '#' *> ws *> idLit) <*> (ws *> char '=' *> ws *> pattern mkFunc)

grammar :: MkFunc -> CharParser () Refs
grammar mkFunc = ws *> (foldl1 union <$> many1 (_patternDecl mkFunc <* ws))
    <|> union <$> (newRef "main" <$> pattern mkFunc) <*> (foldl union emptyRef <$> many (ws *> _patternDecl mkFunc <* ws))

