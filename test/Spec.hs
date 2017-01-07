{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Directory (getCurrentDirectory, listDirectory)
import System.FilePath (FilePath, (</>), takeExtension, takeBaseName, takeDirectory)
import Text.XML.HXT.Parser.XmlParsec (xread)
import Text.XML.HXT.DOM.TypeDefs (XmlTree)

import ParsedTree

data EncodedData 
    = XMLData [XmlTree]
    | JsonData [JsonTree]
    deriving Show

data TestSuiteCase = TestSuiteCase {
    name        :: String
    , grammar   :: String
    , input     :: EncodedData
    , valid     :: Bool
} deriving Show

decodeJson :: String -> [JsonTree]
decodeJson jsonStr = unmarshal jsonStr

getRelapseJson :: [FilePath] -> FilePath
getRelapseJson paths = head $ filter (\fname -> (takeExtension fname) == ".json" && (takeBaseName fname) == "relapse") paths

isValidCase :: [FilePath] -> Bool
isValidCase paths = 1 == (length $ filter (\fname -> (takeBaseName fname) == "valid") paths)

getJson :: [FilePath] -> FilePath
getJson paths = head $ filter (\fname -> (takeExtension fname) == ".json" && (takeBaseName fname) /= "relapse") paths

getXML :: [FilePath] -> FilePath
getXML paths = head $ filter (\fname -> (takeExtension fname) == ".xml" && (takeBaseName fname) /= "relapse") paths

readJsonTest :: FilePath -> IO TestSuiteCase
readJsonTest path = do {
    files <- ls path;
    grammar <- readFile $ getRelapseJson files;
    jsonData <- readFile $ getJson files;
    return $ TestSuiteCase (takeBaseName path) grammar (JsonData (decodeJson jsonData)) (isValidCase files)
}

readXMLTest :: FilePath -> IO TestSuiteCase
readXMLTest path = do {
    files <- ls path;
    grammar <- readFile $ getRelapseJson files;
    xmlData <- readFile $ getXML files;
    return $ TestSuiteCase (takeBaseName path) grammar (XMLData (xread xmlData)) (isValidCase files)
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
    testCases <- mapM readXMLTest xmldirs;
    putStrLn $ show $ head testCases;
    return ()
}