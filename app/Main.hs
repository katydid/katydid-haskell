module Main where

import qualified Relapse
import qualified Json

main :: IO ()
main = either 
    (\err -> putStrLn $ "error:" ++ err) 
    (\valid -> if valid 
        then putStrLn "dragons exist" 
        else putStrLn "dragons are fictional"
    ) $
    Relapse.validate <$> 
        Relapse.parse ".DragonsExist == true" <*> 
        Json.decodeJSON "{\"DragonsExist\": false}"

