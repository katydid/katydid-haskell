module Main where

import Data.Map
import Text.Regex.TDFA
import Control.Monad.Except

f :: Int -> Except String Int
f 0 = throwError "too small"
f n = return (n-1)

-- main = putStrLn $ show $ ("\n\t" =~ "^([ \t\r\n\v\f])+$" :: Bool)
-- main = putStrLn $ show $ matchRegex (mkRegex "^(\\s)+$") " "

main :: IO ()
main = putStrLn $ case runExcept (f 10) of
	(Left s) -> s
	(Right i) -> show i

handler :: String -> Except String Int
handler s = error s
