{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- This module contains functions related to the Protobuf Descriptor.

module Data.Katydid.Parser.Protobuf.Descriptor
  ( decodeFileDescriptorSet
  , mkDescMap
  , DescMap
  , MessageIdx
  , FieldIdx
  , lookupMessageIdx
  , lookupFieldMessageTypeIdx
  , lookupFieldIdx
  , getField
  , getWireFromTag
  , tests
  , WireType(..)
  )
where

import           Data.ByteString                ( ByteString )
import           Data.Word                      ( Word32
                                                , Word64
                                                )
import           Data.Bits                      ( shiftL
                                                , (.|.)
                                                , (.&.)
                                                )
import           Data.List.Index                ( imap )
import qualified Data.Text                     as Text
import qualified Data.Maybe                    as Maybe
import qualified Data.List.Safe                as Safe
import qualified Data.Map.Strict               as M
import           Data.Map.Strict                ( (!) )

import           Data.Text                      ( Text )
import           Data.Int                       ( Int32
                                                , Int64
                                                )

import           Flow                           ( (|>) )

import           Lens.Micro                     ( (^.)
                                                , (.~)
                                                , (&)
                                                )

import qualified Proto.Google.Protobuf.Descriptor
                                               as D
import qualified Proto.Google.Protobuf.Descriptor_Fields
                                               as D
import qualified Data.ProtoLens.Encoding       as Encoding

-- Start of Test Boilerplate

import qualified Test.Tasty                    as T
import qualified Test.Tasty.HUnit              as T
import qualified Data.Katydid.Parser.Protobuf.Testdata.Descriptors
                                               as TestDescs

assert s got want = T.testCase s $ T.assertEqual s want got

tests = T.testGroup "Data.Katydid.Parser.Protobuf.Descriptor"
                    [testListAllMessages, testFieldToMsg, testMsgToField]

-- End of Test Boilerplate

-- | decodeFileDescriptorSet unmarshals a marshaled FileDescriptorSet into the FileDescriptorSet data structure.
decodeFileDescriptorSet :: ByteString -> Either String D.FileDescriptorSet
decodeFileDescriptorSet = Encoding.decodeMessage

-- | FieldIdx represents the unique index that identifies a field descriptor.
newtype FieldIdx = FieldIdx Int
    deriving (Eq, Ord, Show)

-- | MessageIdx represents the unique index that identifies a message descriptor.
newtype MessageIdx = MessageIdx Int
    deriving (Eq, Ord, Show)

-- | DescMap is a collection of maps for quick navigation of the FileDescriptorSet.
data DescMap = DescMap {
    desc :: D.FileDescriptorSet
    , msgNameToId :: M.Map String MessageIdx
    , idToField :: M.Map FieldIdx D.FieldDescriptorProto
    , idToMsg :: M.Map MessageIdx D.DescriptorProto
    , fieldToMsg :: M.Map FieldIdx MessageIdx
    , msgToField :: M.Map MessageIdx (M.Map Word64 FieldIdx)
} deriving (Eq, Show)

-- | mkDescMap builds maps for quick navigation of the FileDescriptorSet, starting at a specific message.
mkDescMap :: D.FileDescriptorSet -> Either String DescMap
mkDescMap fset = do
  -- Recursively find all message descriptors.
  let msgs = listAllMessages fset

  -- Create a map from qualified message name to unique message index.
      msgNameToId =
        msgs
          |> imap (\index (name, _) -> (Text.unpack name, MessageIdx index))
          |> M.fromList

  -- Create a map from the unique message index to the message descriptor.
  let idToMsg =
        msgs |> imap (\index (_, msg) -> (MessageIdx index, msg)) |> M.fromList

  -- Create a map from each message to its field descriptors, while provided each field a globally unique index.
      msgFields :: M.Map MessageIdx [(FieldIdx, D.FieldDescriptorProto)]
      msgFields = mkAllMessageFields idToMsg

  -- Create a map from the globally unique field index to its respective field descriptor.
      idToField = M.elems msgFields |> concat |> M.fromList

  -- Create a map from each field (index) of type message to its respective message type (index).
  -- This is not the reverse index of msgFields, it is rather a way to traverse the type graph.
  fieldToMsg <- mkFieldToMsg msgNameToId idToField

  -- Create a map from each message index to another map.
  -- Each message map maps a tag (fieldNumber << 3 | wireType) to a field index.
  -- This means that if you know in which message you are
  -- then you can quickly lookup which field you are decoding by just looking at the tag (next varint).
  let msgToField = M.map mkTagToField msgFields

  return $ DescMap fset msgNameToId idToField idToMsg fieldToMsg msgToField

testMsgToField = assert
  "msgToField"
  (msgToField <$> mkDescMap TestDescs.exampleFileDescriptorSet)
  (Right $ M.fromList
    [ (MessageIdx 0, M.fromList [(10, FieldIdx 0)])
    , ( MessageIdx 1
      , M.fromList [(10, FieldIdx 1), (18, FieldIdx 2), (26, FieldIdx 3)]
      )
    , (MessageIdx 2, M.fromList [(10, FieldIdx 4), (16, FieldIdx 5)])
    ]
  )

mkTagToField :: [(FieldIdx, D.FieldDescriptorProto)] -> M.Map Word64 FieldIdx
mkTagToField fields =
  fields
    |> concatMap (\(fieldIdx, fieldDesc) -> map (, fieldIdx) (tags fieldDesc))
    |> M.fromList

testFieldToMsg = assert
  "fieldToMsg"
  (fieldToMsg <$> mkDescMap TestDescs.exampleFileDescriptorSet)
  (Right $ M.fromList [(FieldIdx 2, MessageIdx 2), (FieldIdx 3, MessageIdx 0)])

mkFieldToMsg
  :: M.Map String MessageIdx
  -> M.Map FieldIdx D.FieldDescriptorProto
  -> Either String (M.Map FieldIdx MessageIdx)
mkFieldToMsg msgNameToId fields =
  let (fieldToErr, fieldToMsg) =
        fields
          |> M.filter
               (\field ->
                 field ^. D.type' == D.FieldDescriptorProto'TYPE_MESSAGE
               )
          |> M.mapEither
               (\field ->
                 let typeName = field ^. D.typeName |> Text.unpack
                 in  case M.lookup typeName msgNameToId of
                       Nothing ->
                         Left ("message <" ++ typeName ++ "> does not exist")
                       (Just msgId) -> Right msgId
               )
  in  case M.elems fieldToErr |> Safe.head of
        Nothing  -> Right fieldToMsg
        (Just e) -> Left e

mkAllMessageFields
  :: M.Map MessageIdx D.DescriptorProto
  -> M.Map MessageIdx [(FieldIdx, D.FieldDescriptorProto)]
mkAllMessageFields msgs =
  msgs
    |> M.foldlWithKey
         (\(msgFields, currIdx) msgIdx msg ->
           let fields  = mkMessageFields currIdx msg
               nextIdx = currIdx + length fields
           in  (M.insert msgIdx fields msgFields, nextIdx)
         )
         (M.empty, 0)
    |> fst

mkMessageFields
  :: Int -> D.DescriptorProto -> [(FieldIdx, D.FieldDescriptorProto)]
mkMessageFields curr msg =
  imap (\index field -> (FieldIdx (curr + index), field)) (msg ^. D.field)

data WireType = VarInt
    | Fixed64
    | Fixed32
    | Lengthy
    deriving (Eq)

wireNum :: WireType -> Word64
wireNum VarInt  = 0
wireNum Fixed64 = 1
wireNum Fixed32 = 5
wireNum Lengthy = 2

getWireFromTag :: Word64 -> Either String WireType
getWireFromTag tag = case tag .&. 0x7 of
  0 -> Right VarInt
  1 -> Right Fixed64
  5 -> Right Fixed32
  2 -> Right Lengthy
  w -> Left $ "unknown wire type " ++ show w

wireType :: D.FieldDescriptorProto'Type -> WireType
wireType D.FieldDescriptorProto'TYPE_DOUBLE   = Fixed64
wireType D.FieldDescriptorProto'TYPE_FLOAT    = Fixed32
wireType D.FieldDescriptorProto'TYPE_INT64    = VarInt
wireType D.FieldDescriptorProto'TYPE_UINT64   = VarInt
wireType D.FieldDescriptorProto'TYPE_INT32    = VarInt
wireType D.FieldDescriptorProto'TYPE_UINT32   = VarInt
wireType D.FieldDescriptorProto'TYPE_FIXED64  = Fixed64
wireType D.FieldDescriptorProto'TYPE_FIXED32  = Fixed32
wireType D.FieldDescriptorProto'TYPE_BOOL     = VarInt
wireType D.FieldDescriptorProto'TYPE_STRING   = Lengthy
wireType D.FieldDescriptorProto'TYPE_GROUP    = Lengthy
wireType D.FieldDescriptorProto'TYPE_MESSAGE  = Lengthy
wireType D.FieldDescriptorProto'TYPE_BYTES    = Lengthy
wireType D.FieldDescriptorProto'TYPE_ENUM     = VarInt
wireType D.FieldDescriptorProto'TYPE_SFIXED32 = Fixed32
wireType D.FieldDescriptorProto'TYPE_SFIXED64 = Fixed64
wireType D.FieldDescriptorProto'TYPE_SINT32   = VarInt
wireType D.FieldDescriptorProto'TYPE_SINT64   = VarInt

-- | tags calculates the tag value of a field and if the field can possibly be packed also returns the packed tag value.
tags :: D.FieldDescriptorProto -> [Word64]
tags field =
  let wire        = field ^. D.type' |> wireType |> wireNum
      fieldNumber = field ^. D.number |> (fromIntegral :: Int32 -> Word64)
      num3        = fieldNumber `shiftL` 3
      normalTag   = num3 .|. wire
      packedWire  = 2
      packedTag   = num3 .|. packedWire
  in  if couldBePacked field then [normalTag, packedTag] else [normalTag]

-- TODO: create an assert function for tags that tests a packed field.

couldBePacked :: D.FieldDescriptorProto -> Bool
couldBePacked field = isRepeated field && isScalar field

isRepeated :: D.FieldDescriptorProto -> Bool
isRepeated field = field ^. D.label == D.FieldDescriptorProto'LABEL_REPEATED

isScalar :: D.FieldDescriptorProto -> Bool
isScalar field = wireType (field ^. D.type') /= Lengthy

-- | listAllMessages lists all messages and recursively lists nested types 
-- and pairs each up with their respective qualified message name, 
-- which starts with a dot if they are qualified.
listAllMessages :: D.FileDescriptorSet -> [(Text, D.DescriptorProto)]
listAllMessages fset = concatMap listMessagesInFile (fset ^. D.file)

testListAllMessages = assert
  "listAllMessages"
  (listAllMessages TestDescs.exampleFileDescriptorSet)
  [ (".phone.PhoneNumber"          , TestDescs.examplePhoneDesc)
  , (".person.Person"              , TestDescs.examplePersonDesc)
  , (".person.Person.PersonAddress", TestDescs.exampleAddressDesc)
  ]

listMessagesInFile :: D.FileDescriptorProto -> [(Text, D.DescriptorProto)]
listMessagesInFile file =
  let pkgName = file ^. D.maybe'package |> Maybe.maybe "" (Text.cons '.')
      topLevelMsgs =
        map (\msg -> (qualified pkgName msg, msg)) (file ^. D.messageType)
      nestedMsgs = concatMap listNestedMessages topLevelMsgs
  in  topLevelMsgs ++ nestedMsgs

qualified :: Text -> D.DescriptorProto -> Text
qualified prefix msg = prefix `Text.snoc` '.' `Text.append` (msg ^. D.name)

listNestedMessages :: (Text, D.DescriptorProto) -> [(Text, D.DescriptorProto)]
listNestedMessages (prefix, msg) =
  let nestedMsgs =
        map (\msg -> (qualified prefix msg, msg)) (msg ^. D.nestedType)
      nestedNestedMsgs = concatMap listNestedMessages nestedMsgs
  in  nestedMsgs ++ nestedNestedMsgs

-- | lookupMessageIdx returns the uniquely identifying message index for the given full qualified message name.
lookupMessageIdx :: DescMap -> String -> Maybe MessageIdx
lookupMessageIdx desc qualifiedMessageName =
  qualifiedMessageName `M.lookup` msgNameToId desc

-- | lookupFieldMessageTypeIdx returns uniquely identifying message index of field type,
-- given the field type is a message type.
lookupFieldMessageTypeIdx :: DescMap -> FieldIdx -> Either String MessageIdx
lookupFieldMessageTypeIdx desc f = case f `M.lookup` fieldToMsg desc of
  Nothing ->
    Left
      $  "could not find message: "
      ++ (Text.unpack $ getField desc f ^. D.name)
  Just msgIdx -> Right msgIdx

-- | lookupFieldIdx returns the uniquely identifying field index,
-- given a message and a tag (fieldNumber << 3 | wireType).
-- Some fields might be unknown, which is why this function returns a Maybe.
lookupFieldIdx :: DescMap -> MessageIdx -> Word64 -> Maybe FieldIdx
lookupFieldIdx desc msgId tag =
  let fields = msgToField desc ! msgId in tag `M.lookup` fields

-- | getField returns the field descriptor for the unique field index.
getField :: DescMap -> FieldIdx -> D.FieldDescriptorProto
getField desc fieldIdx = idToField desc ! fieldIdx
