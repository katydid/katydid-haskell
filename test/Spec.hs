{-# LANGUAGE FlexibleInstances #-}

-- |
-- This module runs the relapse parsing and validation tests.
module Main where

import qualified Test.HUnit as HUnit
import Control.Monad (when)
import Control.Monad.Except (Except(..), runExcept)

import Parsers (Tree)
import Patterns (Pattern, Refs, nullable, hasRecursion)
import qualified Derive
import qualified MemDerive
import qualified VpaDerive

import qualified ParserSpec
import Suite (readTestCases, TestSuiteCase(..), EncodedData(..))
import qualified RelapseSpec

data Algo = AlgoDeriv
    | AlgoZip
    | AlgoMap
    | AlgoVpa
    deriving Show

must :: Except String Pattern -> Pattern
must e = case runExcept e of
    (Left l) -> error l
    (Right r) -> r

testDeriv :: Tree t => Algo -> String -> Refs -> [t] -> Bool -> IO ()
testDeriv AlgoDeriv name g ts want = 
    let p = must $ Derive.derive g ts 
        got = nullable g p
    in when (want /= got) $ error $ "want " ++ show want ++ " got " ++ show got ++ "\nresulting derivative = " ++ show p
testDeriv AlgoZip name g ts want = 
    let p = must $ Derive.zipderive g ts 
        got = nullable g p
    in when (want /= got) $ error $ "want " ++ show want ++ " got " ++ show got ++ "\nresulting derivative = " ++ show p
testDeriv AlgoMap name g ts want  = 
    let p = must $ MemDerive.derive g ts 
        got = nullable g p
    in when (want /= got) $ error $ "want " ++ show want ++ " got " ++ show got ++ "\nresulting derivative = " ++ show p
testDeriv AlgoVpa name g ts want  = 
    let p = must $ VpaDerive.derive g ts 
        got = nullable g p
    in when (want /= got) $ error $ "want " ++ show want ++ " got " ++ show got ++ "\nresulting derivative = " ++ show p

testName :: Algo -> TestSuiteCase -> String
testName algo (TestSuiteCase name g t want) = name ++ "_" ++ show algo

newTestCase :: Algo -> TestSuiteCase -> HUnit.Test
newTestCase algo c@(TestSuiteCase name g (XMLData t) want) = 
    HUnit.TestLabel (testName algo c) $ HUnit.TestCase $ testDeriv algo name g t want
newTestCase algo c@(TestSuiteCase name g (JsonData t) want) = 
    HUnit.TestLabel (testName algo c) $ HUnit.TestCase $ testDeriv algo name g t want

main :: IO ()
main = do {
    putStrLn "TESTING PARSER";
    parserCounts <- ParserSpec.parserSpec;
    print parserCounts;

    putStrLn "TESTING DERIVATIVE ALGORITHMS";
    testSuiteCases <- readTestCases;

    let nonRecursiveTestCases = filter (\(TestSuiteCase _ g _ _) -> not (hasRecursion g)) testSuiteCases
        derivTests = map (newTestCase AlgoDeriv) nonRecursiveTestCases
        zipTests = map (newTestCase AlgoZip) nonRecursiveTestCases
        mapTests = map (newTestCase AlgoMap) nonRecursiveTestCases
        vpaTests = map (newTestCase AlgoVpa) nonRecursiveTestCases
    ;
    
    counts <- HUnit.runTestTT $ HUnit.TestList $ derivTests ++ zipTests ++ mapTests ++ vpaTests;
    print counts;

    putStrLn "TESTING RELAPSE";
    relapseCounts <- RelapseSpec.relapseSpec;
    print relapseCounts;
    
    return ()
}