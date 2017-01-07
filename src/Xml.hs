{-# LANGUAGE FlexibleInstances #-}

module Xml where

import Text.XML.HXT.DOM.TypeDefs (XmlTree, XNode)
import Text.XML.HXT.Parser.XmlParsec (xread)
import Data.Tree.NTree.TypeDefs (NTree(..))

import Parsers

instance Tree XmlTree where
	getLabel (NTree n _ ) = xmlLabel n
	getChildren (NTree n cs) = cs

decodeXML :: String -> [XmlTree]
decodeXML = xread

xmlLabel :: XNode -> Label
xmlLabel _ = error "xmlLabel: todo"



