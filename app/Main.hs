module Main where

import qualified Relapse
import qualified Json
import Control.Monad.Except (runExcept)

main :: IO ()
main = either 
    (\err -> putStrLn $ "error:" ++ err) 
    (\valid -> if valid 
        then putStrLn "dragons exist" 
        else putStrLn "dragons are fictional"
    ) $
    Relapse.validate <$> 
        runExcept (Relapse.parseGrammar ".DragonsExist == true") <*> 
        Json.decodeJSON "{\"DragonsExist\": false}"

