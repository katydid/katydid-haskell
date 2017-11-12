-- |
-- Suite parses the testsuite folder and creates benchmarks
module Suite (
    readBenches
) where

import System.Directory (getCurrentDirectory, listDirectory, doesDirectoryExist)
import System.FilePath (FilePath, (</>), takeExtension, takeBaseName, takeDirectory)
import Text.XML.HXT.DOM.TypeDefs (XmlTree)

import Patterns (Refs, hasRecursion)
import Json (JsonTree, decodeJSON)
import Xml (decodeXML)
import Parser (parseGrammar)

readBenches :: IO [BenchSuiteCase]
readBenches = do {
    path <- benchPath;
    exists <- doesDirectoryExist path;
    if exists
    then do {
        jsondirs <- ls $ path </> "json";
        xmldirs <- ls $ path </> "xml";
        xmlBenches <- mapM readXMLBench xmldirs;
        jsonBenches <- mapM readJsonBench jsondirs;
        return $ filter (\(BenchSuiteCase _ g _) -> not (hasRecursion g)) (jsonBenches ++ xmlBenches)
    } else return []
}

data BenchSuiteCase = BenchSuiteCase {
    name        :: String
    , grammar   :: Refs
    , input     :: EncodedData
} deriving Show

data EncodedData 
    = XMLDatas [[XmlTree]]
    | JsonDatas [[JsonTree]]
    deriving Show

must :: Either String a -> a
must e = case e of
    (Left l) -> error l
    (Right r) -> r

getRelapse :: [FilePath] -> FilePath
getRelapse paths = head $ filter (\fname -> takeExtension fname == ".txt" && takeBaseName fname == "relapse") paths

filesWithExt :: String -> [FilePath] -> [FilePath]
filesWithExt ext = filter (\fname -> takeExtension fname == ext && takeBaseName fname /= "relapse")

fromGrammar :: String -> Refs
fromGrammar s = case parseGrammar s of
    (Left err) -> error $ "given input: <" ++ s ++ "> got parse error: " ++ show err
    (Right r) -> r

readJsonBench :: FilePath -> IO BenchSuiteCase
readJsonBench path = do {
    files <- ls path;
    grammarData <- readFile $ getRelapse files;
    jsonDatas <- mapM readFile $ filesWithExt ".json" files;
    return $ BenchSuiteCase (takeBaseName path) (fromGrammar grammarData) (JsonDatas $ map (must . decodeJSON) jsonDatas)
}

readXMLBench :: FilePath -> IO BenchSuiteCase
readXMLBench path = do {
    files <- ls path;
    grammarData <- readFile $ getRelapse files;
    xmlDatas <- mapM readFile $ filesWithExt ".xml" files;
    return $ BenchSuiteCase (takeBaseName path) (fromGrammar grammarData) (XMLDatas $ map decodeXML xmlDatas)
}

ls :: FilePath -> IO [FilePath]
ls path = do {
    dirs <- listDirectory path;
    return $ map (path </>) dirs
}

benchPath :: IO FilePath
benchPath = do {
     path <- getCurrentDirectory;
     return $ takeDirectory path </> "testsuite" </> "relapse" </> "benches"
}
