module Main where

import System.Directory
import System.FilePath
import ParsedTree

data TestSuiteCase = TestSuiteCase {
    name        :: String
    , grammar   :: String
    , json      :: [ParsedTree]
    , xml       :: String
    , valid     :: Bool
} deriving Show

decodeJson :: String -> [ParsedTree]
decodeJson jsonStr = unmarshal jsonStr

getRelapseJson :: [FilePath] -> FilePath
getRelapseJson paths = head $ filter (\fname -> (takeExtension fname) == ".json" && (takeBaseName fname) == "relapse") paths

isValidCase :: [FilePath] -> Bool
isValidCase paths = 1 == (length $ filter (\fname -> (takeBaseName fname) == "valid") paths)

getJson :: [FilePath] -> FilePath
getJson paths = head $ filter (\fname -> (takeExtension fname) == ".json" && (takeBaseName fname) /= "relapse") paths

readTest :: FilePath -> IO TestSuiteCase
readTest path = do {
    files <- ls path;
    grammar <- readFile $ getRelapseJson files;
    jsonData <- readFile $ getJson files;
    return $ TestSuiteCase (takeBaseName path) grammar (decodeJson jsonData) "" (isValidCase files)
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

jsonTestsPath :: IO FilePath
jsonTestsPath = do {
    path <- testPath;
    return $ path </> "json"
}

main :: IO ()
main = do {
    jsondir <- jsonTestsPath;
    dirs <- ls jsondir;
    testCases <- mapM readTest dirs;
    putStrLn $ show $ head testCases;
    return ()
}