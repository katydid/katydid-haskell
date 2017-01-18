{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified Test.HUnit as HUnit

import System.Directory (getCurrentDirectory, listDirectory)
import System.FilePath (FilePath, (</>), takeExtension, takeBaseName, takeDirectory)
import Text.XML.HXT.DOM.TypeDefs (XmlTree)

import Parsers
import ParsePatterns
import Patterns
import Values
import Json
import Xml
import Deriv
import UnsafeDeriv
import MapDeriv

data EncodedData 
    = XMLData [XmlTree]
    | JsonData [JsonTree]
    deriving Show

data Algo = AlgoDeriv
    | AlgoZip
    | AlgoUnsafe
    | AlgoMap
    deriving Show

data TestSuiteCase = TestSuiteCase {
    name        :: String
    , grammar   :: Refs
    , input     :: EncodedData
    , valid     :: Bool
} deriving Show

getRelapseJson :: [FilePath] -> FilePath
getRelapseJson paths = head $ filter (\fname -> (takeExtension fname) == ".json" && (takeBaseName fname) == "relapse") paths

isValidCase :: [FilePath] -> Bool
isValidCase paths = 1 == (length $ filter (\fname -> (takeBaseName fname) == "valid") paths)

filepathWithExt :: [FilePath] -> String -> FilePath
filepathWithExt paths ext = head $ filter (\fname -> (takeExtension fname) == ext && (takeBaseName fname) /= "relapse") paths

readJsonTest :: FilePath -> IO TestSuiteCase
readJsonTest path = do {
    files <- ls path;
    grammarData <- readFile $ getRelapseJson files;
    jsonData <- readFile $ filepathWithExt files ".json";
    return $ TestSuiteCase (takeBaseName path) (fromJson grammarData) (JsonData $ decodeJSON jsonData) (isValidCase files)
}

readXMLTest :: FilePath -> IO TestSuiteCase
readXMLTest path = do {
    files <- ls path;
    grammarData <- readFile $ getRelapseJson files;
    xmlData <- readFile $ filepathWithExt files ".xml";
    return $ TestSuiteCase (takeBaseName path) (fromJson grammarData) (XMLData $ decodeXML xmlData) (isValidCase files)
}

ls :: FilePath -> IO [FilePath]
ls path = do {
    dirs <- listDirectory path;
    return $ map (path </>) dirs
}

testPath :: IO FilePath
testPath = do {
     path <- getCurrentDirectory;
     return $ (takeDirectory path) </> "testsuite" </> "relapse" </> "tests"
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

testDeriv :: Tree t => Algo -> String -> Refs -> [t] -> Bool -> IO ()
testDeriv AlgoDeriv name g ts want = case derivs g ts of
    (Value p)   ->  let got = nullable g p
                    in if want /= got then
                        error $ "want " ++ show want ++ " got " ++ show got ++ "\nresulting derivative = " ++ show p
                    else
                        return ()
    (Err e)     ->  error e
testDeriv AlgoZip name g ts want = case zipderivs g ts of
    (Value p)   ->  let got = nullable g p
                    in if want /= got then
                        error $ "want " ++ show want ++ " got " ++ show got ++ "\nresulting derivative = " ++ show p
                    else
                        return ()
    (Err e)     ->  error e
testDeriv AlgoUnsafe name g ts want = case uderivs g ts of
    (Value p)   ->  let got = nullable g p
                    in if want /= got then
                        error $ "want " ++ show want ++ " got " ++ show got ++ "\nresulting derivative = " ++ show p
                    else
                        return ()
    (Err e)     ->  error e
testDeriv AlgoMap name g ts want = case mderivs g ts of
    (Value p)   ->  let got = nullable g p
                    in if want /= got then
                        error $ "want " ++ show want ++ " got " ++ show got ++ "\nresulting derivative = " ++ show p
                    else
                        return ()
    (Err e)     ->  error e

testName :: Algo -> TestSuiteCase -> String
testName algo (TestSuiteCase name g t want) = name ++ "_" ++ show algo

newTestCase :: Algo -> TestSuiteCase -> HUnit.Test
newTestCase algo c@(TestSuiteCase name g (XMLData t) want) = 
    HUnit.TestLabel (testName algo c) $ HUnit.TestCase $ testDeriv algo name g t want
newTestCase algo c@(TestSuiteCase name g (JsonData t) want) = 
    HUnit.TestLabel (testName algo c) $ HUnit.TestCase $ testDeriv algo name g t want

main :: IO ()
main = do {
    testSuiteCases <- readTestCases;
    nonRecursiveTestCases <- return $ filter (\(TestSuiteCase _ g _ _) -> not (hasRecursion g)) testSuiteCases;
    putStrLn $ show $ zip [1..] (map (\(TestSuiteCase name _ _ _) -> name) nonRecursiveTestCases);
    derivTests <- return $ map (newTestCase AlgoDeriv) nonRecursiveTestCases;
    zipTests <- return $ map (newTestCase AlgoZip) nonRecursiveTestCases;
    unsafeTests <- return $ map (newTestCase AlgoUnsafe) nonRecursiveTestCases;
    mapTests <- return $ map (newTestCase AlgoMap) nonRecursiveTestCases;
    counts <- HUnit.runTestTT $ HUnit.TestList $ derivTests ++ zipTests ++ mapTests;
    -- TODO remember to add unsafeTests
    putStrLn $ show counts;
    return ()
}