{-# LANGUAGE OverloadedStrings #-}

-- |
-- This module contains example Protobuf Descriptors used for tests.

module Data.Katydid.Parser.Protobuf.Testdata.Descriptors
    ( exampleFileDescriptorSet
    , examplePersonDesc
    , exampleAddressDesc
    , examplePhoneDesc
    )
where

import           Lens.Micro                     ( (^.)
                                                , (.~)
                                                , (&)
                                                )

import           Data.ProtoLens                 ( def )

import qualified Proto.Google.Protobuf.Descriptor
                                               as D
import qualified Proto.Google.Protobuf.Descriptor_Fields
                                               as D

exampleFileDescriptorSet :: D.FileDescriptorSet
exampleFileDescriptorSet =
    def
        &  D.file
        .~ [ def
           & (D.name .~ "phone.proto")
           & (D.package .~ "phone")
           & (D.syntax .~ "proto2")
           & (D.messageType .~ [examplePhoneDesc])
           , def
           & (D.name .~ "person.proto")
           & (D.package .~ "person")
           & (D.syntax .~ "proto2")
           & (D.dependency .~ ["phone.proto"])
           & (D.messageType .~ [examplePersonDesc])
           ]

examplePersonDesc :: D.DescriptorProto
examplePersonDesc =
    def
        &  (D.name .~ "Person")
        &  D.field
        .~ [ def
           & (D.name .~ "Name")
           & (D.number .~ 1)
           & (D.label .~ D.FieldDescriptorProto'LABEL_OPTIONAL)
           & (D.type' .~ D.FieldDescriptorProto'TYPE_STRING)
           , def
           & (D.name .~ "Address")
           & (D.number .~ 2)
           & (D.label .~ D.FieldDescriptorProto'LABEL_OPTIONAL)
           & (D.type' .~ D.FieldDescriptorProto'TYPE_MESSAGE)
           & (D.typeName .~ ".person.Person.PersonAddress")
           , def
           & (D.name .~ "PhoneNumbers")
           & (D.number .~ 3)
           & (D.label .~ D.FieldDescriptorProto'LABEL_REPEATED)
           & (D.type' .~ D.FieldDescriptorProto'TYPE_MESSAGE)
           & (D.typeName .~ ".phone.PhoneNumber")
           ]
        &  (D.nestedType .~ [exampleAddressDesc])

exampleAddressDesc :: D.DescriptorProto
exampleAddressDesc =
    def
        &  (D.name .~ "PersonAddress")
        &  D.field
        .~ [ def
           & (D.name .~ "Street")
           & (D.number .~ 1)
           & (D.label .~ D.FieldDescriptorProto'LABEL_OPTIONAL)
           & (D.type' .~ D.FieldDescriptorProto'TYPE_STRING)
           , def
           & (D.name .~ "Number")
           & (D.number .~ 2)
           & (D.label .~ D.FieldDescriptorProto'LABEL_OPTIONAL)
           & (D.type' .~ D.FieldDescriptorProto'TYPE_UINT64)
           ]

examplePhoneDesc :: D.DescriptorProto
examplePhoneDesc =
    def
        &  (D.name .~ "PhoneNumber")
        &  D.field
        .~ [ def
             & (D.name .~ "Number")
             & (D.number .~ 1)
             & (D.label .~ D.FieldDescriptorProto'LABEL_OPTIONAL)
             & (D.type' .~ D.FieldDescriptorProto'TYPE_STRING)
           ]
