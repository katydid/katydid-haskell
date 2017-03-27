module ParserSpec where

import Test.HUnit
import Parser
import Text.ParserCombinators.Parsec
import Values
import Patterns
import Control.Monad (unless)

success :: (Eq a, Show a) => String -> CharParser () a -> String -> a -> Test
success name p input want = TestLabel name $ TestCase $ case parse (p <* eof) "" input of
    (Left err) -> assertFailure $ "given input: " ++ input ++ " got error: " ++ show err
    (Right got) -> unless (got == want) $ assertFailure $ "want: " ++ show want ++ " got: " ++ show got

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

    success "zero" intLit "0" 0,
    success "single decimal" intLit "1" 1,
    success "multi decimal" intLit "1230" 1230,
    failure "not an octal or a decimal" intLit "09",
    success "single octal" intLit "01" 1,
    success "multi octal" intLit "017" 15,
    failure "not an octal or an hex" intLit "01f",
    success "single hex" intLit "0xf" 15,
    success "multi hex" intLit "0Xff" 255,
    success "signed hex" intLit "-0xff" (- 255),
    success "cast oct" intLit "int(0114)" 76,
    success "cast signed int" intLit "int(-114)" (- 114),
    failure "cast error not closing" intLit "int(-114",
    failure "cast error not opening" intLit "int-114)",

    success "uint" uintCastLit "uint(114)" 114,
    success "uint oct" uintCastLit "uint(025)" 21,
    failure "uint failure" uintCastLit "uint(-12)",

    success "double" doubleCastLit "double(2.1)" 2.1,
    success "double int" doubleCastLit "double(2)" 2,
    success "double exponent" doubleCastLit "double(2E+2)" 200,
    success "double exponent without sign" doubleCastLit "double(2E2)" 200,
    success "double exponent negative sign" doubleCastLit "double(2E-2)" 0.02,
    success "double exponent and dot" doubleCastLit "double(2.1E-2)" 0.021,
    failure "double failure" doubleCastLit "double(1/2)",
    
    success "interpreted string" stringLit "\"abc\"" "abc",
    success "unicode small" stringLit "\"\\u002E\"" ".",
    success "unicode big" stringLit "\"\\U0000002E\"" ".",
    success "hex char" stringLit "\"\\x2E\"" ".",
    success "octal char" stringLit "\"\\056\"" ".",
    success "escaped" stringLit "\"\\t\"" "\t",
    success "mixed interpreted string" stringLit "\"\\u002Eabc\\x2E\"" ".abc.",
    success "raw string" stringLit "`abc`" "abc",
    success "raw string with quote" stringLit "`ab\"c`" "ab\"c",
    failure "raw string failure" stringLit "`a`b`",
    failure "escaped failure" stringLit "\\/",

    success "bytes char" bytesCastLit "[]byte{'a'}" "a",
    success "bytes string" bytesCastLit "[]byte{'a', 'b', 'c'}" "abc",
    success "bytes unicode char" bytesCastLit "[]byte{'\\u002E'}" ".",
    success "bytes hex char" bytesCastLit "[]byte{'\\x2E'}" ".",
    success "bytes octal char" bytesCastLit "[]byte{'\\056'}" ".",
    success "bytes number" bytesCastLit "[]byte{46}" ".",
    failure "bytes too high number" bytesCastLit "[]byte{1000000}",
    success "bytes number with spaces" bytesCastLit "[]byte{ 46 }" ".",
    success "bytes number with more spaces" bytesCastLit "[]byte{ 46 ,    46     , 46}" "...",

    success "id" idLit "abc" "abc",
    success "id with number" idLit "abc123" "abc123",
    success "id with underscore" idLit "abc_123" "abc_123",
    failure "id starts with number" idLit "123abc",

    success "expr bool var" expr "$bool" BoolVariable,
    success "expr bool const" expr "true" (BoolConst True),
    success "expr ==" expr "== true" (BoolEqualFunc BoolVariable (BoolConst True)),
    success "expr not" expr "not(true)" (NotFunc (BoolConst True)),
    success "expr eq bool" expr "eq($bool, true)" (BoolEqualFunc BoolVariable (BoolConst True)),
    success "expr eq int" expr "eq($int, 1)" (IntEqualFunc IntVariable (IntConst 1)),
    failure "expr eq type mismatch" expr "eq($bool, 1)",
    
   TestCase (return ())]

parserSpec :: IO Counts
parserSpec = runTestTT tests