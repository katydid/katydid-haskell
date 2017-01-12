module Main where

import UnsafeMem
import MapDeriv
import Deriv
import Data.Map
import UnsafeDeriv
import Text.Regex.TDFA

main :: IO ()
main = putStrLn $ show $ ("\n\t" =~ "^([ \t\r\n\v\f])+$" :: Bool)
-- main = putStrLn $ show $ matchRegex (mkRegex "^(\\s)+$") " "
