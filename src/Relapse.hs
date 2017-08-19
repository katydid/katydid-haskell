module Relapse (
    parseGrammar, validate, filter
) where

import Prelude hiding (filter)
import Control.Monad.Except (Except, throwError, return)
import Control.Monad.State (runState)
import Control.Monad (filterM)

import qualified Parser
import Patterns (Refs)
import qualified Patterns
import qualified MemDerive
import Parsers

parseGrammar :: String -> Except String Refs
parseGrammar grammarString = case Parser.parseGrammar grammarString of
    (Left l) -> throwError (show l)
    (Right r) -> return r

validate :: Tree t => Refs -> [t] -> Bool
validate refs tree = case filter refs [tree] of
    [] -> False
    _ -> True

filter :: Tree t => Refs -> [[t]] -> [[t]]
filter refs trees = 
    let start = Patterns.lookupRef refs "main"
        f = filterM (MemDerive.validate refs start) trees
        (r, _) = runState f MemDerive.newMem
    in r
