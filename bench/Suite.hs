{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- |
-- Suite parses the testsuite folder and creates benchmarks
module Suite
  ( readBenches
  , BenchSuiteCase(..)
  , stretch
  , runBench
  )
where

import qualified Data.Text                     as T
                                                ( unpack )
import qualified Data.Text.IO                  as TIO
                                                ( readFile )
import           System.Directory               ( getCurrentDirectory
                                                , listDirectory
                                                , doesDirectoryExist
                                                )
import           System.FilePath                ( FilePath
                                                , (</>)
                                                , takeExtension
                                                , takeBaseName
                                                , takeDirectory
                                                )
import           Text.XML.HXT.DOM.TypeDefs      ( XmlTree )
import           Control.DeepSeq                ( NFData )
import           GHC.Generics                   ( Generic )
import           Data.Int                       ( Int64 )

import           Data.Katydid.Parser.Json       ( JsonTree
                                                , decodeJSON
                                                )
import           Data.Katydid.Parser.Xml        ( decodeXML )

import qualified Data.Katydid.Relapse.Ast      as Ast
import qualified Data.Katydid.Relapse.Parser   as Parser
import qualified Data.Katydid.Relapse.Relapse  as Relapse

runBench :: BenchSuiteCase -> IO Int
runBench (BenchSuiteCase _ g (XMLDatas inputs)) =
  return $ length $ Relapse.filter (compileGrammar g) inputs
runBench (BenchSuiteCase _ g (JsonDatas inputs)) =
  return $ length $ Relapse.filter (compileGrammar g) inputs

readBenches :: IO [BenchSuiteCase]
readBenches = do
  path   <- benchPath
  exists <- doesDirectoryExist path
  if exists
    then do
      jsondirs    <- ls $ path </> "json"
      -- TODO create xml benches in testsuite
      -- xmldirs <- ls $ path </> "xml";
      -- xmlBenches <- mapM readXMLBench xmldirs;
      jsonBenches <- mapM readJsonBench jsondirs
      return
        $ filter (\(BenchSuiteCase _ g _) -> not (hasRecursion g)) jsonBenches
    else return []

data BenchSuiteCase = BenchSuiteCase {
    benchname        :: String
    , grammar   :: String
    , input     :: EncodedData
} deriving (Show, Generic, NFData)

data EncodedData
    = XMLDatas [[XmlTree]]
    | JsonDatas [[JsonTree]]
    deriving (Show, Generic, NFData)

stretch :: BenchSuiteCase -> Int64 -> IO BenchSuiteCase
stretch (BenchSuiteCase name g (XMLDatas xs)) n =
  return $ BenchSuiteCase name g $ XMLDatas $ stretch' (fromIntegral n) xs
stretch (BenchSuiteCase name g (JsonDatas xs)) n =
  return $ BenchSuiteCase name g $ JsonDatas $ stretch' (fromIntegral n) xs

stretch' :: Int -> [a] -> [a]
stretch' n xs | length xs > n = take n xs
              | otherwise     = xs ++ stretch' (n - length xs) xs

must :: Either String a -> a
must e = case e of
  (Left  l) -> error l
  (Right r) -> r

getRelapse :: [FilePath] -> FilePath
getRelapse paths = head $ filter
  (\fname -> takeExtension fname == ".txt" && takeBaseName fname == "relapse")
  paths

filesWithExt :: String -> [FilePath] -> [FilePath]
filesWithExt ext = filter
  (\fname -> takeExtension fname == ext && takeBaseName fname /= "relapse")

compileGrammar :: String -> Relapse.Grammar
compileGrammar s = case Relapse.parse s of
  (Left err) ->
    error $ "given input: <" ++ s ++ "> got compile error: " ++ show err
  (Right r) -> r

hasRecursion :: String -> Bool
hasRecursion s = case Parser.parseGrammar s >>= Ast.hasRecursion of
  (Left err) ->
    error $ "given input: <" ++ s ++ "> got parse error: " ++ show err
  (Right r) -> r

readFileStrict :: FilePath -> IO String
readFileStrict = fmap T.unpack . TIO.readFile

readJsonBench :: FilePath -> IO BenchSuiteCase
readJsonBench path = do
  files       <- ls path
  grammarData <- readFileStrict $ getRelapse files
  jsonDatas   <- mapM readFileStrict $ filesWithExt ".json" files
  return $ BenchSuiteCase (takeBaseName path ++ "Json")
                          grammarData
                          (JsonDatas $ map (must . decodeJSON) jsonDatas)

readXMLBench :: FilePath -> IO BenchSuiteCase
readXMLBench path = do
  files       <- ls path
  grammarData <- readFileStrict $ getRelapse files
  xmlDatas    <- mapM readFileStrict $ filesWithExt ".xml" files
  return $ BenchSuiteCase (takeBaseName path ++ "XML")
                          grammarData
                          (XMLDatas $ map decodeXML xmlDatas)

ls :: FilePath -> IO [FilePath]
ls path = do
  dirs <- listDirectory path
  return $ map (path </>) dirs

benchPath :: IO FilePath
benchPath = do
  path <- getCurrentDirectory
  return $ takeDirectory path </> "testsuite" </> "relapse" </> "benches"
