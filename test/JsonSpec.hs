-- | 
-- This module JsonSpec tests the Json module.
module JsonSpec (
    jsonSpec
) where

import qualified Test.HUnit as HUnit
import Control.Monad (unless)
import Text.JSON (decode, Result(..), JSValue(..), fromJSString, fromJSObject)
import qualified Data.Tree as DataTree

import qualified Parsers
import Json (decodeJSON, JsonTree)

-- Simple Json implementation against which we can test our efficient Json implementation.

simpleDecodeJSON :: String -> Either String [JsonTree]
simpleDecodeJSON s = case decode s of
    (Error e) -> Left e
    (Ok v) -> Right (uValue v)

uValue :: JSValue -> [JsonTree]
uValue JSNull = []
uValue (JSBool b) = [DataTree.Node (Parsers.Bool b) []]
uValue (JSRational _ r) = [DataTree.Node (Parsers.Number r) []]
uValue (JSString s) = [DataTree.Node (Parsers.String (fromJSString s)) []]
uValue (JSArray vs) = uArray 0 vs
uValue (JSObject o) = uObject $ fromJSObject o

uArray :: Int -> [JSValue] -> [JsonTree]
uArray _ [] = []
uArray index (v:vs) = DataTree.Node (Parsers.Number (toRational index)) (uValue v):uArray (index+1) vs

uObject :: [(String, JSValue)] -> [JsonTree]
uObject = map uKeyValue

uKeyValue :: (String, JSValue) -> JsonTree
uKeyValue (name, value) = DataTree.Node (Parsers.String name) (uValue value)

-- tests

same :: String -> HUnit.Test
same input = HUnit.TestLabel input $ HUnit.TestCase $ 
    case simpleDecodeJSON input of
        (Left err) -> HUnit.assertFailure $ "simple json parse error:" ++ show err
        (Right simp) -> case Json.decodeJSON input of
            (Left err) -> HUnit.assertFailure $ "json parse error:" ++ show err
            (Right real) -> unless (simp == real) $ HUnit.assertFailure $ show simp ++ " != " ++ show real 

jsonSpec :: IO HUnit.Counts
jsonSpec = HUnit.runTestTT $ 
    HUnit.TestList [
        same "{\"a\":\"#\"}",
        same "{\"a\":1}",
        same "{\"Name\": \"Robert\", \"Addresses\": [{\"Number\": 456,\"Street\": \"TheStreet\"}],\"Telephone\": \"0127897897\"}",
        same "{\"A\": \"#\",\"B\": \"#\",\"C\": \"#\",\"D\": \"#\"}"
    ]