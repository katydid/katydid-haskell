{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Directory (getCurrentDirectory, listDirectory)
import System.FilePath (FilePath, (</>), takeExtension, takeBaseName, takeDirectory)
import Text.XML.HXT.DOM.TypeDefs (XmlTree)

import Parsers
import ParsePatterns
import Patterns
import Json
import Xml

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
    return $ TestSuiteCase (takeBaseName path) (fromJson grammarData) (JsonData (decodeJSON jsonData)) (isValidCase files)
}

readXMLTest :: FilePath -> IO TestSuiteCase
readXMLTest path = do {
    files <- ls path;
    grammarData <- readFile $ getRelapseJson files;
    xmlData <- readFile $ filepathWithExt files ".xml";
    return $ TestSuiteCase (takeBaseName path) (fromJson grammarData) (XMLData (decodeXML xmlData)) (isValidCase files)
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

main :: IO ()
main = do {
    path <- testPath;
    jsondirs <- ls $ path </> "json";
    xmldirs <- ls $ path </> "xml";
    xmlTestCases <- mapM readXMLTest xmldirs;
    jsonTestCases <- mapM readJsonTest jsondirs;
    putStrLn $ show $ head (xmlTestCases ++ jsonTestCases);
    return ()
}