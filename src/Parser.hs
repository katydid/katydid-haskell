module Parser where

import Text.ParserCombinators.Parsec
import Patterns
import GHC.Base (ord)
import Numeric (readDec, readOct, readHex, readFloat)
import Data.Word
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

ws :: CharParser () ()
ws = _comment <|> (spaces *> return ())

bool :: CharParser () Bool
bool = True <$ string "true"
    <|> False <$ string "false"

_decimalLit :: CharParser () Int
_decimalLit = _read readDec <$> oneOf "123456789" <::> many digit

_octalLit :: CharParser () Int
_octalLit = _read readOct <$> many1 octDigit

_hexLit :: CharParser () Int
_hexLit = _read readHex <$> many1 hexDigit

_read :: ReadS a -> String -> a
_read read s = case read s of
    [(n, "")]   -> n
    ((n, ""):_) -> n
    _           -> error "digit"

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
    return $ _read readFloat (i ++ e)

doubleCastLit :: CharParser () Double
doubleCastLit = string "double(" *> ((*) <$> _optionalSign <*> _floatLit) <* char ')'

idLit :: CharParser () String
idLit = (letter <|> char '_') <::> many (alphaNum <|> char '_')

_qualid :: CharParser () String
_qualid = idLit <++> (concat <$> many (char '.' <::> idLit))

_bigUValue :: CharParser () Char
_bigUValue = char 'U' *> (toEnum . _read readHex <$> count 8 hexDigit)

_littleUValue :: CharParser () Char
_littleUValue = char 'u' *> (toEnum . _read readHex <$> count 4 hexDigit)

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
_hexByteUValue = char 'x' *> (chr . _read readHex <$> count 2 hexDigit)

_octalByteUValue :: CharParser () Char
_octalByteUValue = toEnum . _read readOct <$> count 3 octDigit

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
bytesCastLit = string "[]byte{" *> sepBy (spaces *> _byteElem <* spaces) (char ',') <* char '}'

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

checky :: Either String Expr -> CharParser () Expr
checky (Right r) = return r
checky (Left l) = fail l

check :: CharParser () (Either String Expr) -> CharParser () Expr
check c = do {
    either <- c;
    checky either
} 

_builtin :: CharParser () Expr
_builtin = check $ newBuiltIn <$> _builtinSymbol <*> (spaces *> _expr)

_function :: CharParser () Expr
_function = check $ newFunction <$> idLit <*> (char '(' *> sepBy (spaces *> _expr <* spaces) (char ',') <* char ')')

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
    lt <- _listType;
    es <- spaces *> char '{' *> sepBy (spaces *> _expr <* spaces) (char ',') <* char '}';
    newList lt es
}

_expr :: CharParser () Expr
_expr = _terminal
    <|> _list
    <|> _function

expr :: CharParser () BoolExpr
expr = do {
    e <- _terminal
        <|> _builtin
        <|> _function;
    _mustBool e
}