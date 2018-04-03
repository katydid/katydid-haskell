{-# LANGUAGE OverloadedStrings #-}

-- | 
-- This module ParserSpec tests the Parser module.
module ParserSpec (
    tests
) where

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HUnit

import Text.ParserCombinators.Parsec (CharParser, parse, eof)
import Control.Monad.Except (Except, runExcept)

import Parser
import Expr
import Exprs.Compare
import Exprs.Contains
import Exprs.Elem
import Exprs.Length
import Exprs.Logic
import Exprs.Strings
import Exprs.Type
import Exprs.Var
import Exprs
import Patterns

import UserDefinedFuncs

success :: (Eq a, Show a) => String -> CharParser () a -> String -> a -> T.TestTree
success name p input want = HUnit.testCase name $ case parse (p <* eof) "" input of
    (Left err) -> HUnit.assertFailure $ "given input: " ++ input ++ " got error: " ++ show err
    (Right got) -> HUnit.assertEqual ("want: " ++ show want ++ " got: " ++ show got) want got

failure :: (Show a) => String -> CharParser () a -> String -> T.TestTree
failure name p input = HUnit.testCase name $ case parse (p <* eof) "" input of
    (Left _) -> return ()
    (Right got) -> HUnit.assertFailure $ "want error from input: " ++ show input ++ ", but got: " ++ show got

tests = T.testGroup "Parser" [
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

    success "expr bool var" (expr mkExpr) "$bool" varBoolExpr,
    success "expr bool const" (expr mkExpr) "true" (boolExpr True),
    success "expr ==" (expr mkExpr) "== true" (eqExpr varBoolExpr (boolExpr True)),
    success "expr *=" (expr mkExpr) "*= \"a\"" (containsStringExpr varStringExpr (stringExpr "a")),
    success "expr not" (expr mkExpr) "not(true)" (notExpr (boolExpr True)),
    success "expr eq bool" (expr mkExpr) "eq($bool, true)" (eqExpr varBoolExpr (boolExpr True)),
    success "expr eq int" (expr mkExpr) "eq($int, 1)" (eqExpr varIntExpr (intExpr 1)),
    failure "expr eq type mismatch" (expr mkExpr) "eq($bool, 1)",
    success "expr list" (expr mkExpr) "eq($int, length([]int{1,2}))" (eqExpr varIntExpr (lengthListExpr $ intsExpr [intExpr 1, intExpr 2])),
    
    success "name bool" nameExpr "true" (eqExpr varBoolExpr (boolExpr True)),
    success "name id" nameExpr "a" (eqExpr varStringExpr (stringExpr "a")),
    success "name string" nameExpr "\"a\"" (eqExpr varStringExpr (stringExpr "a")),
    success "name not" nameExpr "!(a)" (notExpr (eqExpr varStringExpr (stringExpr "a"))),
    success "name any" nameExpr "_" (boolExpr True),
    success "name or" nameExpr "(a|b)" (orExpr (eqExpr varStringExpr (stringExpr "a")) (eqExpr varStringExpr (stringExpr "b"))),
    failure "name grouping" nameExpr "((a))",

    success "empty" (pattern mkExpr) "<empty>" Empty,
    success "zany" (pattern mkExpr) "*" ZAny,
    success "or" (pattern mkExpr) "(*|*)" (Or ZAny ZAny),
    success "or list" (pattern mkExpr) "(*|*|*)" (Or ZAny (Or ZAny ZAny)),
    success "or list longer" (pattern mkExpr) "(*|*|*|*|*)" (Or ZAny (Or (Or (Or ZAny ZAny) ZAny) ZAny)),
    success "and" (pattern mkExpr) "(*&*)" (And ZAny ZAny),
    success "and list" (pattern mkExpr) "(*&*&*)" (And ZAny (And ZAny ZAny)),
    failure "mix and or" (pattern mkExpr) "(*|*&*)",
    failure "one item in paren" (pattern mkExpr) "(*)",
    failure "empty paren" (pattern mkExpr) "()",
    success "zero or more" (pattern mkExpr) "(*)*" (ZeroOrMore ZAny),
    success "optional" (pattern mkExpr) "(*)?" (Optional ZAny),
    success "not" (pattern mkExpr) "!(*)" (Not ZAny),
    success "reference" (pattern mkExpr) "@name" (Reference "name"),
    success "concat" (pattern mkExpr) "[*,*]" (Concat ZAny ZAny),
    failure "single concat" (pattern mkExpr) "[*]",
    failure "empty concat" (pattern mkExpr) "[]",
    success "concat list" (pattern mkExpr) "[*,*,*]" (Concat (Concat ZAny ZAny) ZAny),
    success "interleave" (pattern mkExpr) "{*;*}" (Interleave ZAny ZAny),
    success "interleave list" (pattern mkExpr) "{*;*;*}" (Interleave (Interleave ZAny ZAny) ZAny),
    failure "empty interleave" (pattern mkExpr) "{}",
    failure "single interleave" (pattern mkExpr) "{*}",
    success "contains" (pattern mkExpr) ".*" (Contains ZAny),
    success "leaf builtin" (pattern mkExpr) "== 1" (Node (eqExpr varIntExpr (intExpr 1)) Empty),
    success "leaf function" (pattern mkExpr) "->eq($int, 1)" (Node (eqExpr varIntExpr (intExpr 1)) Empty),
    success "treenode" (pattern mkExpr) "a:*" (Node (eqExpr varStringExpr (stringExpr "a")) ZAny),
    success "any treenode" (pattern mkExpr) "_:*" (Node (boolExpr True) ZAny),
    success "treenode no colon" (pattern mkExpr) "_[*,*]" (Node (boolExpr True) (Concat ZAny ZAny)),

    success "treenode with contains" (pattern mkExpr) "a:*=\"b\"" (
        Node (eqExpr varStringExpr (stringExpr "a"))
            $ Node (containsStringExpr varStringExpr (stringExpr "b")) Empty),
    success "anynode with contains" (pattern mkExpr) "_:*=\"b\"" (
        Node (boolExpr True)
            $ Node (containsStringExpr varStringExpr (stringExpr "b")) Empty),
    success "contains anynode with contains" (pattern mkExpr) "._:*=\"b\"" (
        Contains $ Node (boolExpr True)
            $ Node (containsStringExpr varStringExpr (stringExpr "b")) Empty),
    success "contains anynode with contains or" (pattern mkExpr) "(._:*=\"b\"|*)" (
        Or (Contains $ Node (boolExpr True) $ Node (containsStringExpr varStringExpr (stringExpr "b")) Empty)
           ZAny
    ),
    -- (~=\"^([ \t\r\n\v\f])+$\")*
    success "Page195E0AddrE0NameE0" (pattern mkExpr) "Person:{Name:*;(Addr:*)?;(Email:*)*}" (
        Node (eqExpr varStringExpr (stringExpr "Person"))
            (Interleave
                (Interleave
                    (Node (eqExpr varStringExpr (stringExpr "Name")) ZAny)
                    (Optional $ Node (eqExpr varStringExpr (stringExpr "Addr")) ZAny)
                )
                (ZeroOrMore (Node (eqExpr varStringExpr (stringExpr "Email")) ZAny))
            )
    ),
    success "whitespace regex" (pattern mkExpr) "(~=\"^([ \t\r\n\v\f])+$\")*" (
        ZeroOrMore $ Node (regexExpr (stringExpr "^([ \t\r\n\v\f])+$") varStringExpr) Empty
    ),
    success "Page195E0AddrE0NameE0 with whitespace" (pattern mkExpr) "Person:{Name:*;(Addr:*)?;(Email:*)*;(~=\"^([ \t\r\n\v\f])+$\")*}" (
        Node (eqExpr varStringExpr (stringExpr "Person"))
            (Interleave
                (Interleave
                    (Interleave
                        (Node (eqExpr varStringExpr (stringExpr "Name")) ZAny)
                        (Optional $ Node (eqExpr varStringExpr (stringExpr "Addr")) ZAny)
                    )
                    (ZeroOrMore (Node (eqExpr varStringExpr (stringExpr "Email")) ZAny))
                )
                (ZeroOrMore $ Node (regexExpr (stringExpr "^([ \t\r\n\v\f])+$") varStringExpr) Empty)
            )
    ),

    success "single pattern grammar" (grammar mkExpr) "*" $ newRef "main" ZAny,
    success "single pattern decl" (grammar mkExpr) "#main = *" $ newRef "main" ZAny,
    failure "two patterns grammar" (grammar mkExpr) "* *",
    success "two pattern decls" (grammar mkExpr) "#main = * #a = *" $ newRef "main" ZAny `union` newRef "a" ZAny,
    success "one pattern and one pattern decl" (grammar mkExpr) "* #a = *" $ newRef "main" ZAny `union` newRef "a" ZAny,
    success "one pattern and two pattern decls" (grammar mkExpr) "* #a = * #b = *" $ newRef "main" ZAny `union` newRef "a" ZAny `union` newRef "b" ZAny,

    success "not pattern, not name and != conflicts without not enough lookahead" (grammar mkExpr) "!(A):*" (newRef "main" (Node (notExpr (eqExpr varStringExpr (stringExpr "A"))) ZAny)),
    success "->type conflicts with ->true and -1 conflicts with ->" (grammar mkExpr) "->type($string)" (newRef "main" (Node (typeExpr varStringExpr) Empty)),
    success "<= conflicts with <empty>" (grammar mkExpr) "<= 0" (newRef "main" (Node (leExpr varIntExpr (intExpr 0)) Empty)),
    success "unexpected space builtin treenode child" (grammar mkExpr) "A == \"F\"" (newRef "main" (Node (eqExpr varStringExpr (stringExpr "A")) (Node (eqExpr varStringExpr (stringExpr "F")) Empty))),
    success "unexpected space after comment" (grammar mkExpr) "(* & */*spaces*/ )" (newRef "main" (And ZAny ZAny)),
    success "treenode with child builtin type" (grammar mkExpr) "A :: $string" (newRef "main" (Node (eqExpr varStringExpr (stringExpr "A")) (Node (typeExpr varStringExpr) Empty))),
    success "extra semicolon" (grammar mkExpr) "{*;*;}" (newRef "main" (Interleave ZAny ZAny)),

    success "user defined function" (grammar bothLibs) "->isPrime($int)" (newRef "main" (Node (isPrimeExpr varIntExpr) Empty)),
    failure "user defined function" (grammar mkExpr) "->isPrime($int)",

   HUnit.testCase "" (return ())]

bothLibs :: String -> [AnyExpr] -> Except String AnyExpr
bothLibs name args = case runExcept $ mkExpr name args of
    (Left err) -> userLib name args
    (Right expr) -> return expr