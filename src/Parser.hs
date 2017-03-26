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
_lineComment = char '/' *> (many $ noneOf "\n") <* (char '\n') *> return ()

_blockComment :: CharParser () ()
_blockComment = char '*' *> (many $ noneOf "*") <* (char '*') <* char '/' *> return ()

_comment :: CharParser () ()
_comment = char '/' *> (_lineComment <|> _blockComment)

ws :: CharParser () ()
ws = _comment <|> (spaces *> return ())

bool :: CharParser () Bool
bool = True <$ string "true"
    <|> False <$ string "false"

_decimal_lit :: CharParser () Int
_decimal_lit = _read readDec <$> oneOf "123456789" <::> many digit

_octal_lit :: CharParser () Int
_octal_lit = _read readOct <$> many1 octDigit

_hex_lit :: CharParser () Int
_hex_lit = _read readHex <$> many1 hexDigit

_read :: ReadS a -> String -> a
_read read s = case read s of
    [(n, "")]   -> n
    ((n, ""):_) -> n
    _           -> error "digit"

_optionalSign :: (Num a) => CharParser () a
_optionalSign = -1 <$ char '-' <|> return 1

_signed_int_lit :: CharParser () Int
_signed_int_lit = (*) <$> _optionalSign <*> _int_lit

_int_lit :: CharParser () Int
_int_lit = _decimal_lit 
    <|> char '0' *> (_octal_lit 
                    <|> (oneOf "xX" *> _hex_lit)
                    <|> return 0
    )

int_lit :: CharParser () Int
int_lit = string "int(" *> _signed_int_lit <* char ')'
    <|> _signed_int_lit
    <?> "int_lit"

uint_cast_lit :: CharParser () Int
uint_cast_lit = string "uint(" *> _int_lit <* char ')'

_exponent :: CharParser () String
_exponent = oneOf "eE" <::> (opt (oneOf "+-")) <++> many1 digit

_float_lit :: CharParser () Double
_float_lit = do
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

double_cast_lit :: CharParser () Double
double_cast_lit = string "double(" *> ((*) <$> _optionalSign <*> _float_lit) <* (char ')')

id_lit :: CharParser () String
id_lit = (letter <|> char '_') <::> many (alphaNum <|> char '_')

_qualid :: CharParser () String
_qualid = id_lit <++> (concat <$> many (char '.' <::> id_lit))

double_var :: CharParser () String
double_var = string "$double"

int_var :: CharParser () String
int_var = string "$int"

uint_var :: CharParser () String
uint_var = string "$uint"

bytes_var :: CharParser () String
bytes_var = string "$[]byte"

string_var :: CharParser () String
string_var = string "$string"

bool_var :: CharParser () String
bool_var = string "$bool"

_big_u_value :: CharParser () Char
_big_u_value = char 'U' *> (toEnum . (_read readHex) <$> count 8 hexDigit)

_little_u_value :: CharParser () Char
_little_u_value = char 'u' *> (toEnum . (_read readHex) <$> count 4 hexDigit)

_escaped_char :: CharParser () Char
_escaped_char = choice (zipWith (\c r -> r <$ char c) "abnfrtv'\\\"/" "\a\b\n\f\r\t\v\'\\\"/")

_unicode_value :: CharParser () Char
_unicode_value = (char '\\' *> 
    (_big_u_value 
        <|> _little_u_value 
        <|> _hex_byte_u_value 
        <|> _escaped_char
        <|> _octal_byte_u_value)
    ) <|> noneOf "\\\""

_interpreted_string :: CharParser () String
_interpreted_string = between (char '"') (char '"') (many _unicode_value)

_raw_string :: CharParser () String
_raw_string = between (char '`') (char '`') (many $ noneOf "`")

string_lit :: CharParser () String
string_lit = _raw_string <|> _interpreted_string

_hex_byte_u_value :: CharParser () Char
_hex_byte_u_value = char 'x' *> (chr . (_read readHex) <$> count 2 hexDigit)

_octal_byte_u_value :: CharParser () Char
_octal_byte_u_value = toEnum . (_read readOct) <$> count 3 octDigit

_byte_lit :: CharParser () Char
_byte_lit = do {
    i <- _int_lit;
    if i > 255 then
        fail $ "too large for byte: " ++ (show i)
    else
        return $ chr i
}

_byte_elem :: CharParser () Char
_byte_elem = _byte_lit <|> (between (char '\'') (char '\'') (_unicode_value <|> _octal_byte_u_value <|> _hex_byte_u_value))

bytes_cast_lit :: CharParser () [Char]
bytes_cast_lit = string "[]byte{" *> (sepBy (spaces *> _byte_elem <* spaces) (char ',')) <* char '}'

_literal :: CharParser () Expr
_literal = BoolExpr . BoolConst <$> bool
    <|> IntExpr . IntConst <$> int_lit
    <|> UintExpr . UintConst <$> uint_cast_lit
    <|> DoubleExpr . DoubleConst <$> double_cast_lit
    <|> StringExpr . StringConst <$> string_lit
    <|> BytesExpr . BytesConst <$> bytes_cast_lit

_terminal :: CharParser () Expr
_terminal =
    BoolExpr BoolVariable <$ bool_var
    <|> IntExpr IntVariable <$ int_var
    <|> UintExpr UintVariable <$ uint_var
    <|> DoubleExpr DoubleVariable <$ double_var
    <|> StringExpr StringVariable <$ string_var
    <|> BytesExpr BytesVariable <$ bytes_var
    <|> _literal

_builtin_symbol :: CharParser () String
_builtin_symbol = string "==" 
    <|> string "!=" 
    <|> char '<' <::> ((string "=") <|> empty)
    <|> char '>' <::> ((string "=") <|> empty)
    <|> string "~="
    <|> string "*="
    <|> string "^="
    <|> string "$="
    <|> string "::"

_builtin :: CharParser () Expr
_builtin = newBuiltIn <$> _builtin_symbol <*> (spaces *> _expr)

_function :: CharParser () Expr
_function = newFunction <$> id_lit <*> (char '(' *> sepBy (spaces *> _expr <* spaces) (char ',') <* char ')')

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
    es <- (spaces *> char '{' *> sepBy (spaces *> _expr <* spaces) (char ',') <* char '}');
    newList lt es
}

_expr :: CharParser () Expr
_expr = _terminal
    <|> _list
    <|> _function

expr :: CharParser () BoolExpr
expr = do {
    e <- (_terminal
        <|> _builtin
        <|> _function
    );
    _mustBool e
}