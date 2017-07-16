{-# LANGUAGE FlexibleInstances #-}

module Main where

import ParserSpec
import qualified Test.HUnit as HUnit

import System.Directory (getCurrentDirectory, listDirectory)
import System.FilePath (FilePath, (</>), takeExtension, takeBaseName, takeDirectory)
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Control.Monad (when)
import Control.Monad.Except (Except(..), runExcept)

import Parsers
import Parser
import ParsePatterns
import Patterns
import Values
import Json
import Xml
import Deriv
import MapDeriv
import VpaDeriv

data EncodedData 
    = XMLData [XmlTree]
    | JsonData [JsonTree]
    deriving Show

data Algo = AlgoDeriv
    | AlgoZip
    | AlgoMap
    | AlgoVpa
    deriving Show

data TestSuiteCase = TestSuiteCase {
    name        :: String
    , grammar   :: Refs
    , input     :: EncodedData
    , valid     :: Bool
} deriving Show

getRelapseJson :: [FilePath] -> FilePath
getRelapseJson paths = head $ filter (\fname -> takeExtension fname == ".json" && takeBaseName fname == "relapse") paths

getRelapse :: [FilePath] -> FilePath
getRelapse paths = head $ filter (\fname -> takeExtension fname == ".txt" && takeBaseName fname == "relapse") paths

isValidCase :: [FilePath] -> Bool
isValidCase paths = length (filter (\fname -> takeBaseName fname == "valid") paths) == 1

filepathWithExt :: [FilePath] -> String -> FilePath
filepathWithExt paths ext = head $ filter (\fname -> takeExtension fname == ext && takeBaseName fname /= "relapse") paths

fromGrammar :: String -> Refs
fromGrammar s = case parseGrammar s of
    (Left err) -> error $ "given input: <" ++ s ++ "> got parse error: " ++ show err
    (Right r) -> r

readJsonTest :: FilePath -> IO TestSuiteCase
readJsonTest path = do {
    files <- ls path;
    grammarData <- readFile $ getRelapse files;
    jsonData <- readFile $ filepathWithExt files ".json";
    jValue <- return $ case decodeJSON jsonData of
        (Left e) -> error e
        (Right r) -> r
    ;
    return $ TestSuiteCase (takeBaseName path) (fromGrammar grammarData) (JsonData jValue) (isValidCase files)
}

readXMLTest :: FilePath -> IO TestSuiteCase
readXMLTest path = do {
    files <- ls path;
    grammarData <- readFile $ getRelapse files;
    xmlData <- readFile $ filepathWithExt files ".xml";
    return $ TestSuiteCase (takeBaseName path) (fromGrammar grammarData) (XMLData $ decodeXML xmlData) (isValidCase files)
}

ls :: FilePath -> IO [FilePath]
ls path = do {
    dirs <- listDirectory path;
    return $ map (path </>) dirs
}

testPath :: IO FilePath
testPath = do {
     path <- getCurrentDirectory;
     return $ takeDirectory path </> "testsuite" </> "relapse" </> "tests"
}

readTestCases :: IO [TestSuiteCase]
readTestCases = do {
    path <- testPath;
    jsondirs <- ls $ path </> "json";
    xmldirs <- ls $ path </> "xml";
    xmlTestCases <- mapM readXMLTest xmldirs;
    jsonTestCases <- mapM readJsonTest jsondirs;
    return $ jsonTestCases ++ xmlTestCases
}

must :: Except String Pattern -> Pattern
must e = case runExcept e of
    (Left l) -> error l
    (Right r) -> r

testDeriv :: Tree t => Algo -> String -> Refs -> [t] -> Bool -> IO ()
testDeriv AlgoDeriv name g ts want = 
    let p = must $ derivs g ts 
        got = nullable g p
    in when (want /= got) $ error $ "want " ++ show want ++ " got " ++ show got ++ "\nresulting derivative = " ++ show p
testDeriv AlgoZip name g ts want = 
    let p = must $ zipderivs g ts 
        got = nullable g p
    in when (want /= got) $ error $ "want " ++ show want ++ " got " ++ show got ++ "\nresulting derivative = " ++ show p
testDeriv AlgoMap name g ts want  = 
    let p = must $ mderivs g ts 
        got = nullable g p
    in when (want /= got) $ error $ "want " ++ show want ++ " got " ++ show got ++ "\nresulting derivative = " ++ show p
testDeriv AlgoVpa name g ts want  = 
    let p = must $ vderivs g ts 
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
    parserCounts <- parserSpec;
    putStrLn $ show parserCounts;

    putStrLn "TESTING DERIVATIVE ALGORITHMS";
    testSuiteCases <- readTestCases;
    nonRecursiveTestCases <- return $ filter (\(TestSuiteCase _ g _ _) -> not (hasRecursion g)) testSuiteCases;
    -- putStrLn $ show $ zip [1..] (map (\(TestSuiteCase name _ _ _) -> name) nonRecursiveTestCases);
    derivTests <- return $ map (newTestCase AlgoDeriv) nonRecursiveTestCases;
    zipTests <- return $ map (newTestCase AlgoZip) nonRecursiveTestCases;
    mapTests <- return $ map (newTestCase AlgoMap) nonRecursiveTestCases;
    vpaTests <- return $ map (newTestCase AlgoVpa) nonRecursiveTestCases;
    counts <- HUnit.runTestTT $ HUnit.TestList $ derivTests ++ zipTests ++ mapTests ++ vpaTests;
    putStrLn $ show counts;
    
    return ()
}