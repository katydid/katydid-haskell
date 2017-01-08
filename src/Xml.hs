{-# LANGUAGE FlexibleInstances #-}

module Xml where

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
xmlLabel (XText s) = String s
xmlLabel (XBlob b) = String $ blobToString b
xmlLabel x@(XCharRef _) = error $ "XCharRef not supported" ++ show x
xmlLabel x@(XEntityRef _) = error $ "XEntityRef not supported" ++ show x
xmlLabel x@(XCmt _) = error $ "XCmt not supported" ++ show x
xmlLabel (XCdata s) = String s
xmlLabel x@(XPi _ _) = error $ "XPi not supported" ++ show x
xmlLabel (XTag qname attrs) = String (localPart qname) -- TODO attrs should be part of the children returned by getChildren
xmlLabel x@(XDTD _ _) = error $ "XDTD not supported" ++ show x
xmlLabel (XAttr qname) = String (localPart qname)
xmlLabel x@(XError _ _) = error $ "XError not supported" ++ show x


