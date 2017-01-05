module Main where

import System.Directory
import System.FilePath

type TestSuiteCase = [FilePath]

readTest :: FilePath -> IO TestSuiteCase
readTest path = do {
    files <- ls path;
    return files
}

ls :: FilePath -> IO [FilePath]
ls path = do {
    dirs <- listDirectory path;
    return $ map (path </>) dirs
}

main :: IO ()
main = do {
    path <- getCurrentDirectory;
    let testdir = (takeDirectory path) </> "testsuite" </> "relapse" </> "tests"
        jsondir = testdir </> "json"
    in do {
        dirs <- ls jsondir;

        putStrLn $ show dirs;
        return ()
    }
}