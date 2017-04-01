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
    success "expr list" expr "eq($int, length([]int{1,2}))" (IntEqualFunc IntVariable (IntListLengthFunc [IntConst 1, IntConst 2])),
    
    success "name bool" nameExpr "true" (BoolEqualFunc BoolVariable (BoolConst True)),
    success "name id" nameExpr "a" (StringEqualFunc StringVariable (StringConst "a")),
    success "name string" nameExpr "\"a\"" (StringEqualFunc StringVariable (StringConst "a")),
    success "name not" nameExpr "!(a)" (NotFunc (StringEqualFunc StringVariable (StringConst "a"))),
    success "name any" nameExpr "_" (BoolConst True),
    success "name or" nameExpr "(a|b)" (OrFunc (StringEqualFunc StringVariable (StringConst "a")) (StringEqualFunc StringVariable (StringConst "b"))),
    failure "name grouping" nameExpr "((a))",

    success "empty" pattern "<empty>" Empty,
    success "zany" pattern "*" ZAny,
    success "or" pattern "(*|*)" (Or ZAny ZAny),
    success "or list" pattern "(*|*|*)" (Or ZAny (Or ZAny ZAny)),
    success "or list longer" pattern "(*|*|*|*|*)" (Or ZAny (Or (Or (Or ZAny ZAny) ZAny) ZAny)),
    success "and" pattern "(*&*)" (And ZAny ZAny),
    success "and list" pattern "(*&*&*)" (And ZAny (And ZAny ZAny)),
    failure "mix and or" pattern "(*|*&*)",
    failure "one item in paren" pattern "(*)",
    failure "empty paren" pattern "()",
    success "zero or more" pattern "(*)*" (ZeroOrMore ZAny),
    success "optional" pattern "(*)?" (Optional ZAny),
    success "not" pattern "!(*)" (Not ZAny),
    success "reference" pattern "@name" (Reference "name"),
    success "concat" pattern "[*,*]" (Concat ZAny ZAny),
    failure "single concat" pattern "[*]",
    failure "empty concat" pattern "[]",
    success "concat list" pattern "[*,*,*]" (Concat (Concat ZAny ZAny) ZAny),
    success "interleave" pattern "{*;*}" (Interleave ZAny ZAny),
    success "interleave list" pattern "{*;*;*}" (Interleave (Interleave ZAny ZAny) ZAny),
    failure "empty interleave" pattern "{}",
    failure "single interleave" pattern "{*}",
    success "contains" pattern ".*" (Contains ZAny),
    success "leaf builtin" pattern "== 1" (Node (IntEqualFunc IntVariable (IntConst 1)) Empty),
    success "leaf function" pattern "->eq($int, 1)" (Node (IntEqualFunc IntVariable (IntConst 1)) Empty),
    success "treenode" pattern "a:*" (Node (StringEqualFunc StringVariable (StringConst "a")) ZAny),
    success "any treenode" pattern "_:*" (Node (BoolConst True) ZAny),
    success "treenode no colon" pattern "_[*,*]" (Node (BoolConst True) (Concat ZAny ZAny)),

    success "single pattern grammar" grammar "*" (newRef "main" ZAny),
    success "single pattern decl" grammar "#main = *" (newRef "main" ZAny),
    failure "two patterns grammar" grammar "* *",
    success "two pattern decls" grammar "#main = * #a = *" ((newRef "main" ZAny) `union` (newRef "a" ZAny)),
    success "one pattern and one pattern decl" grammar "* #a = *" ((newRef "main" ZAny) `union` (newRef "a" ZAny)),
    success "one pattern and two pattern decls" grammar "* #a = * #b = *" ((newRef "main" ZAny) `union` (newRef "a" ZAny) `union` (newRef "b" ZAny)),

   TestCase (return ())]

parserSpec :: IO Counts
parserSpec = runTestTT tests