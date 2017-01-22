module Main where

import Data.Map
import Text.Regex.TDFA

main :: IO ()
main = putStrLn $ show $ ("\n\t" =~ "^([ \t\r\n\v\f])+$" :: Bool)
-- main = putStrLn $ show $ matchRegex (mkRegex "^(\\s)+$") " "
