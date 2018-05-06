-- | 
-- This module RelapseSpec tests the Relapse module.
module RelapseSpec (
    tests
) where

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HUnit

import Relapse
import Json
import UserDefinedFuncs
import Expr (AnyExpr)
import Exprs (mkExpr)

tests = T.testGroup "Relapse" [
    HUnit.testCase "parseGrammar success" $ either HUnit.assertFailure (\_ -> return ()) $
        Relapse.parse "a == 1"

    , HUnit.testCase "parseGrammar failure" $ either (\_ -> return ()) (\_ -> HUnit.assertFailure "expected error") $
        Relapse.parse "{ a : 1 }" 

    , HUnit.testCase "validate success" $ 
        either HUnit.assertFailure (HUnit.assertBool "expected success") $ 
        Relapse.validate <$> 
            Relapse.parse "a == 1" <*> 
            Json.decodeJSON "{\"a\":1}"

    , HUnit.testCase "validate failure" $
        either HUnit.assertFailure (HUnit.assertBool "expected failure" . not) $
        Relapse.validate <$> 
            Relapse.parse "a == 1" <*> 
            Json.decodeJSON "{\"a\":2}"

    , HUnit.testCase "filter" $ case do {
        refs <- Relapse.parse "a == 1";
        want <- Json.decodeJSON "{\"a\":1}";
        other <- Json.decodeJSON "{\"a\":2}";
        return (Relapse.filter refs [want, other], [want]);
    } of
        (Left err) -> HUnit.assertFailure err
        (Right (got, want)) -> HUnit.assertEqual "expected the same tree" want got

    , HUnit.testCase "user defined function" $ case do {
        refs <- Relapse.parseWithUDFs userLib "a->isPrime($int)";
        want <- Json.decodeJSON "{\"a\":3}";
        other <- Json.decodeJSON "{\"a\":4}";
        return (Relapse.filter refs [want, other], [want]);
    } of
        (Left err) -> HUnit.assertFailure err
        (Right (got, want)) -> HUnit.assertEqual "expected the same tree" want got

    ]
