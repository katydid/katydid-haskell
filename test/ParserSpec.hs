module ParserSpec where

import Test.HUnit
import Parser
import Text.ParserCombinators.Parsec
import Values
import Patterns

success :: (Eq a, Show a) => String -> CharParser () a -> String -> a -> Test
success name p input want = TestLabel name $ TestCase $ case parse (p <* eof) "" input of
    (Left err) -> assertFailure $ "given input: " ++ input ++ " got error: " ++ (show err)
    (Right got) -> if got == want then
        return ()
    else
        assertFailure $ "want: " ++ (show want) ++ " got: " ++ (show got)

failure :: (Show a) => String -> CharParser () a -> String -> Test
failure name p input = TestLabel name $ TestCase $ case parse (p <* eof) "" input of
    (Left _) -> return ()
    (Right got) -> assertFailure $ "want error from input: " ++ show input ++ ", but got: " ++ show got

tests = TestList [
    success "linecomment success" ws "//bla\n" (),
    failure "linecomment failure" ws "//bla",
    success "oneline blockcomment" ws "/*bla*/" (),
    success "twoline blockcomment" ws "/*bla\nbla*/" (),
    success "embedded blockcomment" ws "/*bla//bla*/" (),
    -- TODO success "nested blockcomment" ws "/*bla/*bla*/bla*/" (),
    failure "blockcomment failure" ws "/*bla*",

    success "zero" int_lit "0" 0,
    success "single decimal" int_lit "1" 1,
    success "multi decimal" int_lit "1230" 1230,
    failure "not an octal or a decimal" int_lit "09",
    success "single octal" int_lit "01" 1,
    success "multi octal" int_lit "017" 15,
    failure "not an octal or an hex" int_lit "01f",
    success "single hex" int_lit "0xf" 15,
    success "multi hex" int_lit "0Xff" 255,
    success "signed hex" int_lit "-0xff" (- 255),
    success "cast oct" int_lit "int(0114)" 76,
    success "cast signed int" int_lit "int(-114)" (- 114),
    failure "cast error not closing" int_lit "int(-114",
    failure "cast error not opening" int_lit "int-114)",

    success "uint" uint_cast_lit "uint(114)" 114,
    success "uint oct" uint_cast_lit "uint(025)" 21,
    failure "uint failure" uint_cast_lit "uint(-12)",

    success "double" double_cast_lit "double(2.1)" 2.1,
    success "double int" double_cast_lit "double(2)" 2,
    success "double exponent" double_cast_lit "double(2E+2)" 200,
    success "double exponent without sign" double_cast_lit "double(2E2)" 200,
    success "double exponent negative sign" double_cast_lit "double(2E-2)" 0.02,
    success "double exponent and dot" double_cast_lit "double(2.1E-2)" 0.021,
    failure "double failure" double_cast_lit "double(1/2)",

    success "double var" double_var "$double" "$double",
    success "bytes var" bytes_var "$[]byte" "$[]byte",
    
    success "interpreted string" string_lit "\"abc\"" "abc",
    success "unicode small" string_lit "\"\\u002E\"" ".",
    success "unicode big" string_lit "\"\\U0000002E\"" ".",
    success "hex char" string_lit "\"\\x2E\"" ".",
    success "octal char" string_lit "\"\\056\"" ".",
    success "escaped" string_lit "\"\\t\"" "\t",
    success "mixed interpreted string" string_lit "\"\\u002Eabc\\x2E\"" ".abc.",
    success "raw string" string_lit "`abc`" "abc",
    success "raw string with quote" string_lit "`ab\"c`" "ab\"c",
    failure "raw string failure" string_lit "`a`b`",
    failure "escaped failure" string_lit "\\/",

    success "bytes char" bytes_cast_lit "[]byte{'a'}" "a",
    success "bytes string" bytes_cast_lit "[]byte{'a', 'b', 'c'}" "abc",
    success "bytes unicode char" bytes_cast_lit "[]byte{'\\u002E'}" ".",
    success "bytes hex char" bytes_cast_lit "[]byte{'\\x2E'}" ".",
    success "bytes octal char" bytes_cast_lit "[]byte{'\\056'}" ".",
    success "bytes number" bytes_cast_lit "[]byte{46}" ".",
    failure "bytes too high number" bytes_cast_lit "[]byte{1000000}",
    success "bytes number with spaces" bytes_cast_lit "[]byte{ 46 }" ".",
    success "bytes number with more spaces" bytes_cast_lit "[]byte{ 46 ,    46     , 46}" "...",

    success "id" id_lit "abc" "abc",
    success "id with number" id_lit "abc123" "abc123",
    success "id with underscore" id_lit "abc_123" "abc_123",
    failure "id starts with number" id_lit "123abc",

    success "expr bool var" expr "$bool" BoolVariable,
    success "expr bool const" expr "true" (BoolConst True),
    success "expr ==" expr "== true" (BoolEqualFunc BoolVariable (BoolConst True)),
    success "expr not" expr "not(true)" (NotFunc (BoolConst True)),
    success "expr eq" expr "eq($bool, true)" (BoolEqualFunc BoolVariable (BoolConst True)),
    
   (TestCase (return ()))]

parserSpec :: IO Counts
parserSpec = runTestTT tests