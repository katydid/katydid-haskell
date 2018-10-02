{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- This module contains the Protobuf Parser.

module Data.Katydid.Parser.Protobuf.Protobuf
  ( tests
  )
where

import           Data.ByteString                ( ByteString )
import           Data.Word                      ( Word64
                                                , Word32
                                                )
import qualified Data.Tree                     as Tree
import           Data.Bits                      ( (.&.) )
import           Data.Maybe                     ( catMaybes )
import           Control.Monad                  ( void )
import           Flow                           ( (|>) )
import qualified Data.Text                     as Text
import           Data.Int                       ( Int32
                                                , Int64
                                                )

import           Data.Text.Encoding             ( decodeUtf8' )

import           Data.Attoparsec.ByteString    as Parse
import qualified Data.Katydid.Parser.Protobuf.Bytes
                                               as Bytes

import qualified Data.Katydid.Parser.Protobuf.Descriptor
                                               as D
import qualified Data.Katydid.Parser.Parser    as Parser

-- Start Tests

import qualified Test.Tasty                    as T
import qualified Test.Tasty.HUnit              as T

import           Lens.Micro                     ( (^.)
                                                , (.~)
                                                , (&)
                                                )
import           Data.ProtoLens                 ( def
                                                , showMessage
                                                )
import qualified Data.ProtoLens.Encoding       as Encoding
import           Data.ProtoLens.Message         ( Message )
import qualified Proto.Google.Protobuf.Descriptor
                                               as P
import qualified Proto.Google.Protobuf.Descriptor_Fields
                                               as P

import qualified Data.Katydid.Parser.Protobuf.Testdata.Proto.Person
                                               as Person
import qualified Data.Katydid.Parser.Protobuf.Testdata.Proto.Person_Fields
                                               as Person
import qualified Data.Katydid.Parser.Protobuf.Testdata.Proto.Phone
                                               as Phone
import qualified Data.Katydid.Parser.Protobuf.Testdata.Proto.Phone_Fields
                                               as Phone
import qualified Data.Katydid.Parser.Protobuf.Testdata.Descriptors
                                               as TestDescs
import qualified Debug.Trace                   as Debug
debug s a = Debug.trace (s ++ " <" ++ show a ++ ">") a

assert s got want = T.testCase s $ T.assertEqual s want got

tests = T.testGroup "Data.Katydid.Parser.Protobuf.Protobuf" [testDecode]

testHelperDecode :: (Message msg) => String -> msg -> Either String [ProtoNode]
testHelperDecode qualifiedMessageName msg = do
  descMap <- D.mkDescMap TestDescs.exampleFileDescriptorSet
  case D.lookupMessageIdx descMap qualifiedMessageName of
    Nothing -> Left
      (  "could not find <"
      ++ qualifiedMessageName
      ++ "> in the given file descriptor set"
      )
    Just msgIdx ->
      let bytes = Encoding.encodeMessage msg in decode descMap msgIdx bytes

testDecode = T.testGroup
  "decode"
  [ assert
    "Name Only"
    (testHelperDecode ".person.Person"
                      (def & (Person.name .~ "Simon") :: Person.Person)
    )
    (Right
      [Tree.Node (Parser.String "Name") [Tree.Node (Parser.String "Simon") []]]
    )
  , assert
    "Address"
    (testHelperDecode
      ".person.Person"
      (def
      & (Person.name .~ "Simon")
      & (  Person.address
        .~ (def & Person.street .~ "Dalsig Road" & Person.number .~ 9)
        ) :: Person.Person
      )
    )
    (Right
      [ Tree.Node (Parser.String "Name") [Tree.Node (Parser.String "Simon") []]
      , Tree.Node
        (Parser.String "Address")
        [ Tree.Node (Parser.String "Street")
                    [Tree.Node (Parser.String "Dalsig Road") []]
        , Tree.Node (Parser.String "Number") [Tree.Node (Parser.Uint 9) []]
        ]
      ]
    )
  , assert
    "Address And Phone Number"
    (testHelperDecode
      ".person.Person"
      (def
      & (Person.name .~ "Simon")
      & (  Person.address
        .~ (def & Person.street .~ "Dalsig Road" & Person.number .~ 9)
        )
      & (  Person.phoneNumbers
        .~ [ def & Phone.number .~ "0211234567"
           , def & Phone.number .~ "0217654321"
           ]
        ) :: Person.Person
      )
    )
    (Right
      [ Tree.Node (Parser.String "Name") [Tree.Node (Parser.String "Simon") []]
      , Tree.Node
        (Parser.String "Address")
        [ Tree.Node (Parser.String "Street")
                    [Tree.Node (Parser.String "Dalsig Road") []]
        , Tree.Node (Parser.String "Number") [Tree.Node (Parser.Uint 9) []]
        ]
      , Tree.Node
        (Parser.String "PhoneNumbers")
        [ Tree.Node (Parser.String "Number")
                    [Tree.Node (Parser.String "0211234567") []]
        , Tree.Node (Parser.String "Number")
                    [Tree.Node (Parser.String "0217654321") []]
        ]
      ]
    )
  ]

-- End of Tests

-- |
-- ProtoNode is a tree that can be validated by Relapse.
type ProtoNode = Tree.Tree Parser.Label

instance Parser.Tree ProtoNode where
    getLabel (Tree.Node l _) = l
    getChildren (Tree.Node _ cs) = cs

decode :: D.DescMap -> D.MessageIdx -> ByteString -> Either String [ProtoNode]
decode desc msgIdx bytes =
  let fieldLookup = D.lookupFieldIdx desc msgIdx
                                                                                                      -- TODO: repeated fields should be given indices
  in  catMaybes
        <$> Parse.parseOnly (parseMsg desc fieldLookup <* endOfInput) bytes

type FieldLookup = Word64 -> Maybe D.FieldIdx

parseMsg :: D.DescMap -> FieldLookup -> Parse.Parser [Maybe ProtoNode]
parseMsg desc getField = Parse.many' (parseField desc getField)

parseField :: D.DescMap -> FieldLookup -> Parse.Parser (Maybe ProtoNode)
parseField desc getField = do
  tag <- Bytes.getVarInt
  case D.getWireFromTag tag of
    (Left  err ) -> fail err
    (Right wire) -> case getField tag of
      Nothing -> skipValue wire >> return Nothing
      (Just fieldIdx) ->
        let fieldDesc = D.getField desc fieldIdx
        in  Just <$> parseValue desc fieldIdx wire

skipValue :: D.WireType -> Parse.Parser ()
skipValue wire = case wire of
  D.VarInt  -> Bytes.getVarInt |> void
  D.Fixed32 -> (Bytes.anyBits :: Parse.Parser Word32) |> void
  D.Fixed64 -> (Bytes.anyBits :: Parse.Parser Word64) |> void
  D.Lengthy -> Bytes.getVarInt >>= \length ->
    length |> (fromIntegral :: Word64 -> Int) |> Parse.take |> void

parseValue :: D.DescMap -> D.FieldIdx -> D.WireType -> Parse.Parser ProtoNode
parseValue desc fieldIdx wire
  = let
      fieldDesc = D.getField desc fieldIdx
      fieldName = fieldDesc ^. P.name
      fieldType = fieldDesc ^. P.type'
    in
      case wire of
        D.VarInt -> do
          value <- Bytes.getVarInt
          let
            toInt  = fromIntegral :: Word64 -> Int
            toUint = fromIntegral :: Word64 -> Word
            label  = case fieldType of
              P.FieldDescriptorProto'TYPE_INT64  -> Parser.Int (toInt value)
              P.FieldDescriptorProto'TYPE_UINT64 -> Parser.Uint (toUint value)
              P.FieldDescriptorProto'TYPE_INT32  -> Parser.Int (toInt value)
              P.FieldDescriptorProto'TYPE_UINT32 -> Parser.Uint (toUint value)
              P.FieldDescriptorProto'TYPE_ENUM ->
                -- TODO: double check that this is correct, 
                -- Is it uint, int or string or more than one?
                Parser.Uint (toUint value)
              P.FieldDescriptorProto'TYPE_BOOL -> Parser.Bool (value /= 0)
              P.FieldDescriptorProto'TYPE_SINT32 ->
                value
                  |> (fromIntegral :: Word64 -> Word32)
                  |> Bytes.wordToSignedInt32
                  |> (fromIntegral :: Int32 -> Int)
                  |> Parser.Int
              P.FieldDescriptorProto'TYPE_SINT64 ->
                value
                  |> Bytes.wordToSignedInt64
                  |> (fromIntegral :: Int64 -> Int)
                  |> Parser.Int
          return $ Tree.Node (Parser.String fieldName) [Tree.Node label []]
        D.Fixed32 -> do
          value <- Bytes.anyBits :: Parse.Parser Word32
          let
            toInt    = fromIntegral :: Word32 -> Int
            toUint   = fromIntegral :: Word32 -> Word
            toDouble = (realToFrac :: Float -> Double) . Bytes.wordToFloat
            label    = case fieldType of
              P.FieldDescriptorProto'TYPE_FLOAT ->
                Parser.Double (toDouble value)
              P.FieldDescriptorProto'TYPE_FIXED32  -> Parser.Uint (toUint value)
              P.FieldDescriptorProto'TYPE_SFIXED32 -> Parser.Int (toInt value)
          return $ Tree.Node (Parser.String fieldName) [Tree.Node label []]
        D.Fixed64 -> do
          value <- Bytes.anyBits :: Parse.Parser Word64
          let
            toInt    = fromIntegral :: Word64 -> Int
            toUint   = fromIntegral :: Word64 -> Word
            toDouble = Bytes.wordToDouble
            label    = case fieldType of
              P.FieldDescriptorProto'TYPE_DOUBLE ->
                Parser.Double (toDouble value)
              P.FieldDescriptorProto'TYPE_FIXED64  -> Parser.Uint (toUint value)
              P.FieldDescriptorProto'TYPE_SFIXED64 -> Parser.Int (toInt value)
          return $ Tree.Node (Parser.String fieldName) [Tree.Node label []]
        D.Lengthy -> do
          length <- Bytes.getVarInt
          let toInt = fromIntegral :: Word64 -> Int
          bytes <- Parse.take (toInt length)
          case fieldType of
            -- TODO: all scalar fields can be packed and should also be handled in this case
            P.FieldDescriptorProto'TYPE_BYTES -> return $ Tree.Node
              (Parser.String fieldName)
              [Tree.Node (Parser.Bytes bytes) []]
            P.FieldDescriptorProto'TYPE_STRING -> case decodeUtf8' bytes of
              (Left  err) -> fail (show err)
              (Right str) -> return $ Tree.Node
                (Parser.String fieldName)
                [Tree.Node (Parser.String str) []]
            P.FieldDescriptorProto'TYPE_GROUP -> fail "group is not supported"
            P.FieldDescriptorProto'TYPE_MESSAGE ->
              case D.lookupFieldMessageTypeIdx desc fieldIdx of
                (Left  err   ) -> fail err
                (Right msgIdx) -> case decode desc msgIdx bytes of
                  (Left err) -> fail err
                  (Right fields) ->
                    return $ Tree.Node (Parser.String fieldName) fields
