{-# LANGUAGE FlexibleInstances #-}

module Xml where

import Text.Read (readMaybe)
import Text.XML.HXT.DOM.TypeDefs (XmlTree, XNode(..), blobToString, localPart)
import Text.XML.HXT.Parser.XmlParsec (xread)
import Data.Tree.NTree.TypeDefs (NTree(..))

import Parsers

instance Tree XmlTree where
	getLabel (NTree n _ ) = xmlLabel n
	getChildren (NTree n cs) = cs

decodeXML :: String -> [XmlTree]
decodeXML = xread

xmlLabel :: XNode -> Label
xmlLabel (XText s) = parseLabel s
xmlLabel (XBlob b) = parseLabel $ blobToString b
xmlLabel x@(XCharRef _) = error $ "XCharRef not supported" ++ show x
xmlLabel x@(XEntityRef _) = error $ "XEntityRef not supported" ++ show x
xmlLabel x@(XCmt _) = error $ "XCmt not supported" ++ show x
xmlLabel (XCdata s) = parseLabel s
xmlLabel x@(XPi _ _) = error $ "XPi not supported" ++ show x
xmlLabel (XTag qname attrs) = parseLabel (localPart qname) -- TODO attrs should be part of the children returned by getChildren
xmlLabel x@(XDTD _ _) = error $ "XDTD not supported" ++ show x
xmlLabel (XAttr qname) = parseLabel (localPart qname)
xmlLabel x@(XError _ _) = error $ "XError not supported" ++ show x

-- TODO what about other leaf types
parseLabel :: String -> Label
parseLabel s = case (readMaybe s :: Maybe Int) of
	(Just i) -> Number (toRational i)
	Nothing -> String s
