module Parser where

import Text.ParserCombinators.Parsec
import Patterns
import GHC.Base (ord)
import Numeric (readDec, readOct, readHex, readFloat)
import Data.Word
import Data.Char (chr)

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

_id :: CharParser () String
_id = (letter <|> char '_') <::> many (alphaNum <|> char '_')

_qualid :: CharParser () String
_qualid = _id <++> (concat <$> many (char '.' <::> _id))

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
