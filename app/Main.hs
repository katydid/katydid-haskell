module Main where

import qualified Data.Katydid.Relapse.Relapse  as Relapse
import qualified Data.Katydid.Parser.Json      as Json

main :: IO ()
main =
  either
      (\err -> putStrLn $ "error:" ++ err)
      (\valid -> if valid
        then putStrLn "dragons exist"
        else putStrLn "dragons are fictional"
      )
    $   Relapse.validate
    <$> Relapse.parse ".DragonsExist == true"
    <*> Json.decodeJSON "{\"DragonsExist\": false}"

