-- | 
-- This module RelapseSpec tests the Relapse module.
module RelapseSpec (
    relapseSpec
) where

import qualified Test.HUnit as HUnit
import Control.Monad.Except (runExcept)

import Relapse
import Json

tests = HUnit.TestList [
    HUnit.TestLabel "parseGrammar success" $ HUnit.TestCase $ 
    case runExcept $ Relapse.parseGrammar "a == 1" of
        (Left err) -> HUnit.assertFailure $ show err
        (Right _) -> return (),

    HUnit.TestLabel "parseGrammar failure" $ HUnit.TestCase $ 
    case runExcept $ Relapse.parseGrammar "{ a : 1 }" of
        (Left err) -> return ()
        (Right _) -> HUnit.assertFailure "expected error",

    HUnit.TestLabel "validate success" $ HUnit.TestCase $ 
    case runExcept $ Relapse.parseGrammar "a == 1" of
        (Left err) -> HUnit.assertFailure $ "relapse parse error: " ++ show err
        (Right refs) -> case Json.decodeJSON "{\"a\":1}" of
            (Left err) -> HUnit.assertFailure $ "json parse error: " ++ show err
            (Right tree) -> if (Relapse.validate refs tree)
                then return ()
                else HUnit.assertFailure "expected success",

    HUnit.TestLabel "validate failure" $ HUnit.TestCase $ 
    case runExcept $ Relapse.parseGrammar "a == 1" of
        (Left err) -> HUnit.assertFailure $ "relapse parse error: " ++ show err
        (Right refs) -> case Json.decodeJSON "{\"a\":2}" of
            (Left err) -> HUnit.assertFailure $ "json parse error: " ++ show err
            (Right tree) -> if (Relapse.validate refs tree)
                then HUnit.assertFailure "expected failure"
                else return (),

    HUnit.TestLabel "filter" $ HUnit.TestCase $ 
    case runExcept $ Relapse.parseGrammar "a == 1" of
        (Left err) -> HUnit.assertFailure $ "relapse parse error: " ++ show err
        (Right refs) -> case Json.decodeJSON "{\"a\":1}" of
            (Left err) -> HUnit.assertFailure $ "json parse error: " ++ show err
            (Right want) -> case Json.decodeJSON "{\"a\":2}" of
                (Left err) -> HUnit.assertFailure $ "json parse error: " ++ show err
                (Right other) -> case (Relapse.filter refs [want, other]) of
                    [got] -> if got == want
                        then return ()
                        else HUnit.assertFailure "expected the same tree"
                    _ -> HUnit.assertFailure "expected a single tree",
    
    HUnit.TestCase (return ())]

relapseSpec :: IO HUnit.Counts
relapseSpec = HUnit.runTestTT tests