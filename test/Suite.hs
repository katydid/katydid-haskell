-- |
-- Suite parses the testsuite folder and creates test cases
module Suite (
    readTestCases, TestSuiteCase(..), EncodedData(..)
) where

import System.Directory (getCurrentDirectory, listDirectory)
import System.FilePath (FilePath, (</>), takeExtension, takeBaseName, takeDirectory)
import Text.XML.HXT.DOM.TypeDefs (XmlTree)

import Patterns (Refs, Pattern)
import Json (JsonTree, decodeJSON)
import Xml (decodeXML)
import Parser (parseGrammar)

data TestSuiteCase = TestSuiteCase {
    name        :: String
    , grammar   :: Refs
    , input     :: EncodedData
    , valid     :: Bool
} deriving Show

data EncodedData 
    = XMLData [XmlTree]
    | JsonData [JsonTree]
    deriving Show

readTestCases :: IO [TestSuiteCase]
readTestCases = do {
    path <- testPath;
    jsondirs <- ls $ path </> "json";
    xmldirs <- ls $ path </> "xml";
    xmlTestCases <- mapM readXMLTest xmldirs;
    jsonTestCases <- mapM readJsonTest jsondirs;
    return $ jsonTestCases ++ xmlTestCases
}

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
    let jValue = case decodeJSON jsonData of
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
