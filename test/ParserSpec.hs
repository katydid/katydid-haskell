module ParserSpec where

import Test.HUnit
import Parser
import Text.ParserCombinators.Parsec

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
   (TestCase (return ()))]

parserSpec :: IO Counts
parserSpec = runTestTT tests