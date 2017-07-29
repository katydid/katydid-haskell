{-# LANGUAGE FlexibleInstances #-}

-- |
-- This module contains the XML Parser.

module Xml (
    decodeXML
) where

import Text.Read (readMaybe)
import Text.XML.HXT.DOM.TypeDefs (XmlTree, XNode(..), blobToString, localPart)
import Text.XML.HXT.Parser.XmlParsec (xread)
import Data.Tree.NTree.TypeDefs (NTree(..))

import Parsers

instance Tree XmlTree where
    getLabel (NTree n _ ) = case xmlLabel n of
        (Left err) -> String $ "XML Parse Error:" ++ err
        (Right r) -> r
    getChildren (NTree _ cs) = cs

-- |
-- decodeXML returns a XmlTree, given an input string.
decodeXML :: String -> [XmlTree]
decodeXML = xread

xmlLabel :: XNode -> Either String Label
xmlLabel (XText s) = return $ parseLabel s
xmlLabel (XBlob b) = return $ parseLabel $ blobToString b
xmlLabel x@(XCharRef _) = fail $ "XCharRef not supported" ++ show x
xmlLabel x@(XEntityRef _) = fail $ "XEntityRef not supported" ++ show x
xmlLabel x@(XCmt _) = fail $ "XCmt not supported" ++ show x
xmlLabel (XCdata s) = return $ parseLabel s
xmlLabel x@(XPi _ _) = fail $ "XPi not supported" ++ show x
xmlLabel (XTag qname attrs) = return $ parseLabel (localPart qname) -- TODO attrs should be part of the children returned by getChildren
xmlLabel x@(XDTD _ _) = fail $ "XDTD not supported" ++ show x
xmlLabel (XAttr qname) = return $ parseLabel (localPart qname)
xmlLabel x@(XError _ _) = fail $ "XError not supported" ++ show x

-- TODO what about other leaf types
parseLabel :: String -> Label
parseLabel s = case (readMaybe s :: Maybe Int) of
    (Just i) -> Number (toRational i)
    Nothing -> String s
