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

data EncodedData 
    = XMLData [XmlTree]
    | JsonData [JsonTree]
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
    return $ tail $ tail $ tail $ take 4 jsonTestCases -- TODO add xmlTestCases
}

testDeriv :: Tree t => String -> Refs -> [t] -> Bool -> IO ()
testDeriv name g ts want = case derivs g ts of
    (Value p)   ->  let got = nullable g p
                    in if want /= got then
                        error $ "want " ++ show want ++ " got " ++ show got ++ "\nresulting derivative = " ++ show p
                    else
                        return ()
    (Err e)     ->  error e

testACase :: TestSuiteCase -> IO ()
testACase (TestSuiteCase name g (XMLData t) want) = testDeriv name g t want
testACase (TestSuiteCase name g (JsonData t) want) = testDeriv name g t want

newTestCaseList :: [TestSuiteCase] -> HUnit.Test
newTestCaseList suite = HUnit.TestList $ map newTestCase suite

testName :: TestSuiteCase -> String
testName (TestSuiteCase name g t want) = name ++ ":" ++ "input tree = " ++ show t

newTestCase :: TestSuiteCase -> HUnit.Test
newTestCase c@(TestSuiteCase name g (XMLData t) want) = 
    HUnit.TestLabel (testName c) $ HUnit.TestCase $ testDeriv name g t want
newTestCase c@(TestSuiteCase name g (JsonData t) want) = 
    HUnit.TestLabel (testName c) $ HUnit.TestCase $ testDeriv name g t want

main :: IO ()
main = do {
    testSuiteCases <- readTestCases;
    -- mapM testACase testSuiteCases;
    counts <- HUnit.runTestTT $ newTestCaseList testSuiteCases;
    putStrLn $ show counts;
    return ()
}