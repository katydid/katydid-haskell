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
    HUnit.testCase "parseGrammar success" $
    case runExcept $ Relapse.parseGrammar "a == 1" of
        (Left err) -> HUnit.assertFailure $ show err
        (Right _) -> return ()

    , HUnit.testCase "parseGrammar failure" $
    case runExcept $ Relapse.parseGrammar "{ a : 1 }" of
        (Left err) -> return ()
        (Right _) -> HUnit.assertFailure "expected error"

    , HUnit.testCase "validate success" $
    case runExcept $ Relapse.parseGrammar "a == 1" of
        (Left err) -> HUnit.assertFailure $ "relapse parse error: " ++ show err
        (Right refs) -> case Json.decodeJSON "{\"a\":1}" of
            (Left err) -> HUnit.assertFailure $ "json parse error: " ++ show err
            (Right tree) -> HUnit.assertBool "expected success" $ Relapse.validate refs tree

    , HUnit.testCase "validate failure" $
    case runExcept $ Relapse.parseGrammar "a == 1" of
        (Left err) -> HUnit.assertFailure $ "relapse parse error: " ++ show err
        (Right refs) -> case Json.decodeJSON "{\"a\":2}" of
            (Left err) -> HUnit.assertFailure $ "json parse error: " ++ show err
            (Right tree) -> HUnit.assertBool "expected failure" $ not $ Relapse.validate refs tree

    , HUnit.testCase "filter" $
    case runExcept $ Relapse.parseGrammar "a == 1" of
        (Left err) -> HUnit.assertFailure $ "relapse parse error: " ++ show err
        (Right refs) -> case Json.decodeJSON "{\"a\":1}" of
            (Left err) -> HUnit.assertFailure $ "json parse error: " ++ show err
            (Right want) -> case Json.decodeJSON "{\"a\":2}" of
                (Left err) -> HUnit.assertFailure $ "json parse error: " ++ show err
                (Right other) -> case Relapse.filter refs [want, other] of
                    [got] -> HUnit.assertEqual "expected the same tree" want got
                    _ -> HUnit.assertFailure "expected a single tree"
    ]
