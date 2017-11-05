-- | 
-- This module RelapseSpec tests the Relapse module.
module RelapseSpec (
    tests
) where

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HUnit

import Control.Monad.Except (runExcept)

import Relapse
import Json

tests = T.testGroup "Relapse" [
    HUnit.testCase "parseGrammar success" $ either HUnit.assertFailure (\_ -> return ()) $
        runExcept $ Relapse.parseGrammar "a == 1"

    , HUnit.testCase "parseGrammar failure" $ either (\_ -> return ()) (\_ -> HUnit.assertFailure "expected error") $
        runExcept $ Relapse.parseGrammar "{ a : 1 }" 

    , HUnit.testCase "validate success" $ 
        either HUnit.assertFailure (HUnit.assertBool "expected success") $ 
        Relapse.validate <$> 
            runExcept (Relapse.parseGrammar "a == 1") <*> 
            Json.decodeJSON "{\"a\":1}"

    , HUnit.testCase "validate failure" $
        either HUnit.assertFailure (HUnit.assertBool "expected failure" . not) $
        Relapse.validate <$> 
            runExcept (Relapse.parseGrammar "a == 1") <*> 
            Json.decodeJSON "{\"a\":2}"

    , HUnit.testCase "filter" $ case do {
        refs <- runExcept $ Relapse.parseGrammar "a == 1";
        want <- Json.decodeJSON "{\"a\":1}";
        other <- Json.decodeJSON "{\"a\":2}";
        return (Relapse.filter refs [want, other], [want]);
    } of
        (Left err) -> HUnit.assertFailure err
        (Right (got, want)) -> HUnit.assertEqual "expected the same tree" want got

    ]
